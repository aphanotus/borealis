#' Convert landmark data from xlsx or csv to tps format
#'
#' Reformats X and Y coordinate positions from a spreadsheet into the \code{tps} ("thin-plate spline")
#' file format defined by Rohlf (2015).
#'
#' The first row of the input file must provide column names.
#' There must be columns headed "x" and "y", although these are not case-sensitive
#' and can occur in any order.
#' There must be a column giving specimen IDs, using a name like "ID" or "specimen_IDs"
#' Any other columns are optional and may be used to encode metadata.
#' Columns named by \code{id.factors} will be added to the \code{ID=} line
#' in the resulting \code{tps} file, seperated by the character string in the \code{separator} parameter.
#'
#' If \code{include.scale = TRUE}, then you must
#' have a column headed "scale" to be included as a \code{SCALE=} line for each specimen in
#' the \code{tps} file.
#'
#' The scale value can also be inverted, by setting \code{invert.scale = TRUE}.
#' This may be useful, because \code{\link[geomorph]{readland.tps}} applies scale values by multiplication.
#' Typically this is appropriate when scale is recorded as unit distance (e.g. mm) per pixel.
#' However, if scale is recorded in pixels per unit distance (e.g. pixels/mm)
#' it will be appropriate to first invert the scaling factor before importing coordinate data.
#'
#' Each row must include X and Y coordinates for landmarks, in order.
#' Each specimen should appear with a  consequtive block of rows, with landmarks in the same order.
#' The number of landmarks must be consistent for all specimens. Specimen metadata must appear on the
#' first row for each specimen. (That is, on the row for landmark 1.)
#'
#' By default, the number of specimens and landmarks will be determined by the function. This will be done
#' by using the number of cells in the specimen ID column with non-whitespace characters. It will be assumed
#' that all other metadata appears in the same rows, and any information in other rows will be ignored.
#'
#' The \code{output.filename} may be specified, or by default it will be the input filename with the
#' addition of \code{.YYMMDD.tps}, where \code{YYMMDD} is the date.
#'
#' If \code{export.metadata = TRUE} then a seperate output file will be created that contains only the metadata.
#' All metadata will be included in this file.
#'
#' Only the first sheet will be used from \code{xlsx} input files.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references  Rohlf, FJ. 2015. The tps series of software. \emph{Hystrix} 26, 9–12.
#' (\href{https://doi.org/10.4404/hystrix-26.1-11264}{Link})
#'
#' @param input.filename The file name to import.
#' @param output.filename The file name to export.
#' @param id.factors Metadata column names to be encoded in the specimen IDs.
#' @param separator A character string to separate the terms used in the ID line.
#' @param include.scale A logical value indicating whether or not the imported data
#'     includes a column with scale values.
#' @param invert.scale A logical value indicating whether to invert the scale value.
#' @param export.metadata A logical value indicating whether or not metadata should be
#'     exported to a seperate file.
#'
#' @export
#'
#' @examples
#' # As an example, an input CSV file might looks like this:
#' #
#' # specimen.ID , digitizer , treatment , stage , scale , sex , x   , y
#' # T330.5      , DRA       , control   , adult , 1.23  , f   , 320 , 453
#' #             ,           ,           ,       ,       ,     , 303 , 468
#' #             ,           ,           ,       ,       ,     , 289 , 447
#' # T330.6      , DRA       , control   , adult , 1.24  , m   , 319 , 490
#' #             ,           ,           ,       ,       ,     , 300 , 501
#' #             ,           ,           ,       ,       ,     , 294 , 480
#'
#' create.tps(
#'   input.filename = "rawdata.csv",
#'   output.filename = "output.tps",
#'   id.factors = c('digitizer','treatment','stage','sex'),
#'   include.scale = TRUE )
#'
#' # The file, output.tps, would be created below
#' #
#' # LM=3
#' # 320 453
#' # 303 468
#' # 289 447
#' # ID=T330_5__DRA__control__adult__f
#' # SCALE=1.23
#' #
#' # LM=3
#' # 319 490
#' # 300 501
#' # 294 480
#' # ID=T330_6__DRA__control__adult__m
#' # SCALE=1.24

create.tps <- function (
  input.filename = NULL,
  output.filename = NULL,
  id.factors = NULL,
  separator = "__",
  include.scale = FALSE,
  invert.scale = FALSE,
  export.metadata = FALSE,
  specimen.number = NULL,
  landmark.number = NULL,
  include.header = TRUE
)
{
  # Determine the input file name and format
  if (is.null(input.filename)) { input.filename <- file.choose() }
  ISxlsx <- grepl('.xlsx$',input.filename)
  IScsv <- grepl('.csv$',input.filename)
  if (!ISxlsx & !IScsv) { stop(paste("The input file",input.filename,"must be xlsx or csv format.")) }

  # Import the raw data
  if (ISxlsx) {
    if (!require('openxlsx')) { stop("To import from xlsx files, please run `install.packages('openxlsx')` ")}
    raw <- openxlsx::read.xlsx(input.filename)
  } else {
    raw <- read.csv(input.filename, stringsAsFactors=FALSE)
  }
  colnames(raw) <- tolower(trimws(colnames(raw)))

  # Get the number of specimens and landmarks
  if (is.null(specimen.number)) {
    acceptable.ID.column.names <- c("id","ids","specimen","specimen id","specimen.id","specimen_id","specimen.ids","specimen_ids","sample","sample id","sample.id","sample_id","sample.ids","sample_ids")
    ID.col <- which(names(raw) %in% acceptable.ID.column.names)
    if (!(length(ID.col)==1)) {
      stop(paste("The input file",input.filename,"must have one column with specimen IDs using one of the following headings:",paste(acceptable.ID.column.names, collapse = ', ')))
    }
    specimen.number <- sum(!grepl("^\\s*$", na.omit(raw[,ID.col])))
  }
  if (is.null(landmark.number)) {
    acceptable.LM.column.names <- c("LM","lm","landmark","landmarks","landmark.number","landmark_number")
    LM.col <- which(names(raw) %in% acceptable.LM.column.names)
    if (length(LM.col)==1) { landmark.number <- max(raw[,LM.col])
    } else {
      landmark.number <- dim(raw)[1] / specimen.number
      if (landmark.number %% 1 != 0) {
        stop(paste("The input file",input.filename,"must have a consistent number of landmarks.",landmark.number,"landmarks detected."))
      }
    }
  }

  # Check for formatting issues
  if (landmark.number < 3) {
    stop(paste('Error:',input.filename,"does not have at least 3 landmarks.\n") )
  }
  if (!(any(grepl('x',names(raw), ignore.case = TRUE), na.rm = TRUE) &
        any(grepl('y',names(raw), ignore.case = TRUE), na.rm = TRUE) ) ) {
    stop(paste('Error:',input.filename,"does not contain one of the required column headings: 'X' and 'yY'.\n") )
  }
  if (specimen.number != dim(raw)[1]/landmark.number) {
    if ((dim(raw)[1] %% landmark.number == 0) & (dim(raw)[1]/landmark.number > specimen.number)) {
      x <- dim(raw)[1]/landmark.number - specimen.number
      s <- paste0('Error: ',input.filename,' contains ',x,' duplicate specimen IDs. ')
      x <- raw[,ID.col]; x <- x[which(nchar(x)>1)]; x <- x[which(duplicated(x))]
      stop(paste0(s,x,sep='\n'))
    } else {
      stop(paste('Error:',input.filename,'does not contain properly formatted landmark data.\n'))
    }
  }

  # Check for problems with the use of scale
  if (include.scale) {
    scale.col <- grep('scale',names(raw), ignore.case = TRUE)
    if (length(scale.col)==0) {
      warning(paste("Warning: include.scale = TRUE, but no scale column detected in",input.filename,". Proceeding without scale.\n"))
      include.scale <- FALSE
    } else {
      if (length(scale.col)>1) {
        warning(paste("Warning: Multiple columns (",paste(names(raw)[scale.col],collapse = ', '),") may contain scale information. Using only column '",names(raw)[scale.col[1]],"'.\n"))
        scale.col <- scale.col[1]
      } else {
        if (!is.numeric(raw[,scale.col])) {
          warning(paste0('Warning: Some specimens may have non-numeric scale values, e.g. ',raw[1,scale.col],'\n'))
        }
      }
    }
  }

  # The data are okay to proceed

  if (is.null(output.filename)) {
    output.filename <- sub(".csv$","",input.filename)
    output.filename <- sub(".xlsx$","",output.filename)
    output.filename <- paste(output.filename,format(Sys.time(), "%y%m%d"),'tps',sep='.')
  }

  # Send output to the output file
  sink(output.filename)

  # File header
  if (include.header) {
    cat("# ## TPS file creation \n# \n")
    cat(paste0("# Created by user `",(Sys.getenv("LOGNAME")),"` with `borealis::create.tps` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n# \n"))
    cat("# Input file: ",input.filename,"\n# \n")
    cat(paste0("# The dataset is ",landmark.number," x 2 x ",specimen.number," (*p* landmarks x *k* dimensions x *n* specimens)\n# \n"))
    if (length(id.factors)>0) {
      id.factors <- tolower(id.factors)
      cat(paste0("# Metadata are encoded in specimen ID lines from the following factors:\n# - ",paste0(id.factors, collapse = '\n# - '),"\n# \n"))
      cat(paste0("# Metadata separator: ",separator,"\n# \n"))
    }
    if (include.scale) {
      if (invert.scale) {
        cat("# **Scale** included and **inverted** from the original dataset.\n# \n")
      } else {
        cat("# **Scale** included directly from the original dataset.\n# \n")
      }
    }
    cat("\n")
  }

  # Loop for each specimen
  for (i in 1:specimen.number) {
    cat(paste0('LM=',landmark.number,'\n'))

    # Nested loop for each landmark
    for (j in 1:landmark.number) {
      x <- landmark.number*(i-1) + j
      cat(paste0(raw$x[x],' ',raw$y[x],'\n'))
    } # End nested loop for each landmark

    x <- landmark.number*(i-1) + 1
    id.text <- paste0(sub('\\.','_',trimws(raw[x,ID.col])))
    if (length(id.factors)>0) {
      id.text <- paste0(id.text,separator,paste(trimws(raw[x,id.factors]),collapse = separator) )
    }
    cat(paste0('ID=',id.text,'\n'))
    if (include.scale) {
      scale.value <- raw[x,scale.col]
      if (invert.scale) { scale.value <- 1 / as.numeric(scale.value) }
      if (!is.na(scale.value) & is.numeric(scale.value)) {
        cat(paste0('SCALE=',scale.value,'\n'))
      }
    }
    cat('\n')
  } # End loop for each specimen

  # Close the output file
  sink()

  if (export.metadata) {
    x <- seq(1,dim(raw)[1],landmark.number)
    y <- names(raw)[!grepl("^[xy]$",names(raw))]
    write.csv(raw[x,y], file = paste0(output.filename,'.metadata.csv'),
              quote = FALSE, row.names = FALSE )
  }

} # End of function

