#' Read in and convert linear multivariate morphometric data
#'
#' Reads in linear multivariate morphometric (MMM) data from a \code{csv} or \code{xlsx} file.
#' (Only the first sheet will be used from \code{xlsx} input files.)
#' The input spread sheet is assumed to be "long," in the sense that one column includes a list measurements
#' made either by hand or in grpahics software such as \href{https://imagej.net}{ImageJ}.
#' This organization is typically a convenient for rapid data entry.
#' The function reformats the multiple measurements into a traditional tabular format,
#' with each measurement in a seperate column.
#'
#' There must be a column giving specimen IDs, using a name like "ID" or "specimen_IDs".
#' Any other columns are optional and may be used to encode metadata.
#' Columns named by \code{metadata.cols} will be retained, others will be ignored.
#' All columns in the input file will be retained if \code{metadata.cols = "all"} .
#'
#' If a column name is supplied to \code{apply.scale}, then it will be used to adjust measurement values.
#' This is done by multiplyng the scaling factor by the measurement values.
#' Typically this is appropriate when scale is recorded as unit distance (e.g. mm) per pixel.
#' However, if scale is recorded in pixels per unit distance (e.g. pixels/mm)
#' it will be appropriate to set \code{invert.scale = TRUE}, if which case, the scaling factor will be
#' inverted before applyng it. (In other words, the scale value will be used to divide the measurement value.)
#' This is anologous to the treatment of scale in \code{\link[geomorph]{readland.tps}},
#' which applies scale values by multiplication, and in \code{\link[borelais]{create.tps}}, which allows
#' the user to specify how to apply the scaling factor.
#'
#' Each specimen should appear with a  consequtive block of rows, with measurements in the same order.
#' Each measurements must appear in a consistent order for all specimens.
#' The number of measurements must be consistent for all specimens. Specimen metadata must appear on the
#' first row for each specimen. (That is, on the row for measurement 1.)
#'
#' By default, the number of specimens and measurements will be determined by the function.
#' This will be done by using the number of cells in the specimen ID column with non-whitespace characters.
#' It will be assumed that all other metadata appears in the same rows, and that
#' any information in other rows will be ignored.
#'
#' @return Returns a list with several elements.
#'     \code{x} is a data frame with the measurements and metadata, each row correwsaponding to a specimen.
#'     \code{measurement.number} and \code{specimen.number} containing integer values.
#'     \code{scaled} is a logical flag for whether scaling has been applied to measurement values.
#'     If any specimens were missing a scale value, the the element \code{specimens.missing.scale}
#'     will be an ID-named index of those specimens. (These may want to be removed!)
#'     The element \code{provenance} is a list recording data provenance.
#'     If an \code{output.filename} is specified, then the data frame in \code{x} will be written to the
#'     file in \code{csv} or \code{tsv} format.
#'
#'     If \code{export.provenance. = TRUE} then a seperate text
#'     output file will be created that contains data provenance.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param input.filename The file name to import.
#' @param output.filename The file name to export. Or if \code{output.filename = TRUE},
#'     then it will be \code{input.filename.YYMMDD.csv}, where \code{YYMMDD} is the date.
#' @param metadata.cols Metadata column names or numbers to be retained in the output table.
#' @param measurement.col A string naming the column containing measurements.
#'     The function can make an educated guess at this, recognizing names like "Measurement", "distance" or "pixels".
#' @param apply.scale A string naming the column containing scale values to multiple measurement values.
#' @param invert.scale A logical value indicating whether to invert the scale value before applying it.
#' @param export.provenance A logical value indicating whether or not data provenance should be exported
#'     to a seperate file. This file's name will follows the \code{output.filename} and appended \code{.provenance.txt}.
#'
#' @export
#'
#' @examples
#' mmm.data <- read.mmm(
#'   input.filename = "raw.measurements.csv",
#'   output.filename = TRUE,
#'   metadata.cols = "all",
#'   apply.scale = TRUE,
#'   invert.scale = TRUE
#' )
#'
#' names(mmm.data)
#'
#' dim(mmm.data$x)
#' head(mmm.data$x)
#'
#' mmm.data$scaled
#'
#' mmm.data$missing.scale
#'
#' cat(unlist(mmm.data$provenance))
#'

read.mmm <- function (
  input.filename = NULL,
  output.filename = NULL,
  measurement.names = NULL,
  metadata.cols = NULL,
  measurement.col = NULL,
  apply.scale = FALSE,
  invert.scale = FALSE,
  export.provenance = FALSE
)
{
  # Determine the input file name and format
  if (is.null(input.filename)) { input.filename <- file.choose() }
  ISxlsx <- grepl('.xlsx$',input.filename)
  IScsv <- grepl('.csv$',input.filename)
  if (!ISxlsx & !IScsv) { return(cat(paste("The input file",input.filename,"must be xlsx or csv format."))) }

  # Import the raw data
  if (ISxlsx) {
    require('openxlsx')
    raw <- openxlsx::read.xlsx(input.filename)
  } else {
    raw <- read.csv(input.filename, stringsAsFactors=FALSE)
  }
  colnames(raw) <- tolower(trimws(colnames(raw)))

  # Get the number of specimens and measurements
  acceptable.ID.column.names <- c("id","ids","specimen","specimen id","specimen.id","specimen_id","specimen.ids","specimen_ids","sample","sample id","sample.id","sample_id","sample.ids","sample_ids")
  ID.col <- which(names(raw) %in% acceptable.ID.column.names)
  if (length(ID.col) != 1) {
    return(cat(paste("The input file",input.filename,"must have one column with specimen IDs using one of the following headings:",paste(acceptable.ID.column.names, collapse = ', '))))
  }
  specimen.number <- sum(!grepl("^\\s*$", na.omit(raw[,ID.col])))

  acceptable.MM.column.names <- c("mm","mmm","measurement","measurements","value","pixels","px","distance")
  MM.col <- which(names(raw) %in% acceptable.MM.column.names)
  if (length(MM.col) != 1) {
    return(cat(paste("The input file",input.filename,"must have one column with measurements using one of the following headings:",paste(acceptable.MM.column.names, collapse = ', '))))
  } else {
    measurement.number <- dim(raw)[1] / specimen.number
    if (measurement.number %% 1 != 0) {
      return(cat(paste("The input file",input.filename,"must have a consistent number of measurements.",measurement.number,"measurements detected.")))
    }
  }

  # Check for formatting issues
  warning.text <- NULL
  if (specimen.number != dim(raw)[1]/measurement.number) {
    if ((dim(raw)[1] %% measurement.number == 0) & (dim(raw)[1]/measurement.number > specimen.number)) {
      x <- dim(raw)[1]/measurement.number - specimen.number
      warning.text <- paste0('Warning: ',input.filename,' contains ',x,' duplicate specimen IDs.\n')
      cat(warning.text)
      x <- raw[,ID.col]; x <- x[which(nchar(x)>1)]; x <- x[which(duplicated(x))]
      x <- paste0("  ",x,"\n", collapse = "")
      cat(x)
      warning.text <- paste0(warning.text, x, collapse = "")
    } else {
      return(cat(paste('Error:',input.filename,'does not contain properly formatted measurement data.\n')))
    }
  }

  # If no measurement.names, make them up
  x <- ceiling(log10(measurement.number))
  x <- paste0("%0",x,"d")
  default.measurement.names <- paste0("m",sprintf(x,1:measurement.number))
  if (is.null(measurement.names)) {
    measurement.names <- default.measurement.names
  } else {
    # Check that measurement.names are not too many
    if (length(measurement.names) > measurement.number) {
      s <- paste("Warning: The number of measurement.names provided exceeds the number of detected measurements. Using only",measurement.number,"names.\n")
      cat(s)
      warning.text <- paste0(warning.text, s, collapse = "")
      measurement.names <- measurement.names[1:measurement.number]
    } else {
      # Check that there are enough measurement.names
      if (length(measurement.names) < measurement.number) {
        s <- paste("Warning: The number of measurement.names provided is fewer than the number of detected number of measurements:",measurement.number,"\n")
        cat(s)
        warning.text <- paste0(warning.text, s, collapse = "")
        measurement.names <- c(measurement.names,default.measurement.names[length(measurement.names):measurement.number])
      }
    }
  }

  # Finds the column containing scale info
  if (is.logical(apply.scale)) {
    if (apply.scale) {
      scale.col <- grep('scale',names(raw), ignore.case = TRUE)
      if (length(scale.col)==0) {
        s <- paste("Warning: apply.scale = TRUE, but no scale column detected in",input.filename,". Proceeding without scale. \nTry specifying a column name e.g. apply.scale = 'scale' \n")
        cat(s)
        warning.text <- paste0(warning.text, s, collapse = "")
        apply.scale <- NULL
        scale.col <- NULL
      } else {
        if (length(scale.col)>1) {
          s <- paste("Warning: Multiple columns (",paste(names(raw)[scale.col],collapse = ', '),") may contain scale information. Using only column '",names(raw)[scale.col[1]],"'.\nTry specifying a column name e.g. apply.scale = 'scale' \n")
          cat(s)
          warning.text <- paste0(warning.text, s, collapse = "")
          scale.col <- scale.col[1]
        } else {
          if (!is.numeric(raw[,scale.col])) {
            s <- paste("Warning: Some specimens may have non-numeric scale values. \nTry specifying a column name e.g. apply.scale = 'scale' \n")
            cat(s)
            warning.text <- paste0(warning.text, s, collapse = "")
          }
        }
      }
    }
  } else {
    if (!is.null(apply.scale)) {
      scale.col <- grep(apply.scale[1],names(raw), ignore.case = TRUE)
      if (length(scale.col)==0) {
        s <- paste("Warning: apply.scale =",apply.scale,", but no column with that name has been found. Proceeding without scale. \n")
        cat(s)
        warning.text <- paste0(warning.text, s, collapse = "")
        apply.scale <- NULL
        scale.col <- NULL
      } else {
        if (length(scale.col)>1) {
          s <- paste("Warning: Multiple columns (",paste(names(raw)[scale.col],collapse = ', '),") may contain scale information. Using only column '",names(raw)[scale.col[1]],"'.\nTry specifying a different column name e.g. apply.scale = 'scale'. \nRegex expressions may be used, such as 'scale$'.\n")
          cat(s)
          warning.text <- paste0(warning.text, s, collapse = "")
          scale.col <- scale.col[1]
        } else {
          if (!is.numeric(raw[,scale.col])) { cat("Warning: Some specimens may have non-numeric scale values. \nTry specifying a different column name e.g. apply.scale = 'scale' \n") }
        }
      }
    }
  }

  # Vet the metadata.cols
  md.cols.not.founds <- NULL
  if (metadata.cols == "all") {
    metadata.cols <- c(1:length(names(raw)))[-c(ID.col,MM.col)]
  } else {
    if (!is.null(metadata.cols)) {
      if (!is.numeric(metadata.cols)) {
        retain.metadata.cols <- NULL
        for (i in 1: length(metadata.cols)) {
          if (metadata.cols[i] %in% colnames(raw)) {
            retain.metadata.cols <- c(retain.metadata.cols, grep(paste0("^",metadata.cols[i],"$"), colnames(raw)))
          } else {
            md.cols.not.founds <- metadata.cols[i]
            s <- paste("Warning: Metadata column",md.cols.not.founds,"not found.\n")
            cat(s)
            warning.text <- paste0(warning.text, s, collapse = "")
          }
        }
        metadata.cols <- retain.metadata.cols
      } # End  if (!is.numeric(metadata.cols))
    } # End  if (!is.null(metadata.cols))
  } # End  else

  # Initalize the new reformatted data frame
  df <- data.frame(
    matrix(nrow = specimen.number,
           ncol = 1 + measurement.number + length(metadata.cols),
           dimnames = list(
             NULL,
             c(names(raw)[ID.col],measurement.names,names(raw)[metadata.cols])
           ))
  )
  df.ID.col <- 1
  df.MM.cols <- 1 + c(1:measurement.number)
  df.md.cols <- max(df.MM.cols) + c(1:length(metadata.cols))

  # MAIN LOOP
  if (!is.null(scale.col)) { scale.values <- NULL }

  # for each specimen
  for (i in 1:specimen.number) {
    spec.i <- measurement.number*(i-1) + 1

    # Copy ID
    df[i,df.ID.col] <- raw[spec.i,ID.col]

    # Copy metadata
    df[i,df.md.cols] <- raw[spec.i,metadata.cols]

    # Get scale values
    if (!is.null(scale.col)) { scale.values <- c(scale.values, raw[spec.i,scale.col]) }

    # Nested loop for each measurement
    for (j in 1:measurement.number) {
      x <- measurement.number*(i-1) + j # row index number in raw table
      df[i,df.MM.cols[j]] <- raw[x,MM.col]
    } # End nested loop for each measurement

  } # End loop for each specimen

  # Apply scaling
  specimens.without.scale <- NULL
  if (!is.null(scale.col)) {
    specimens.without.scale <- which(is.na(scale.values))
    if (length(specimens.without.scale) > 0) {
      s <- paste("Warning: The following specimens have no scale values. Using a scale factor of 1.\n")
      x <- paste0("  ",df[,df.ID.col][specimens.without.scale],"\n", collapse = "")
      s <- paste(s, x, collapse = "")
      cat(s)
      warning.text <- paste0(warning.text, s, collapse = "")
      scale.values[specimens.without.scale] <- 1
      names(specimens.without.scale) <- df[,df.ID.col][specimens.without.scale]
    }
    if (invert.scale) {
      scale.values <- 1 / scale.values
    }
    # The math
    df[,df.MM.cols] <- df[,df.MM.cols] * scale.values
  }

  # Automatic output filename creation
  default.output.filename <- paste(input.filename,"wide",format(Sys.time(), "%y%m%d"),sep='.')

  if (is.logical(output.filename)) {
    if (output.filename) {
      output.filename <- paste(default.output.filename,'csv',sep='.')
    }
  }

  # Write output file
  if (!is.null(output.filename)) {
    if (grepl(".tsv$",output.filename)) { seperator <- "\t" }
    else {
      if (grepl(".csv$",output.filename)) { seperator <- "," }
      else {
        s <- paste("Warning:",output.filename,"format not recognized from file name. Writing as",paste0(output.filename,'.csv'),"\n")
        cat(s)
        warning.text <- paste0(warning.text, s, collapse = "")
        seperator <- ","
        output.filename <- paste0(output.filename,".csv")
      }
      s <- paste("Table exported to",output.filename,"with",measurement.number,"measurements and",length(df.md.cols),"metadata columns for",specimen.number,"specimens.\n")
      cat(s)
      warning.text <- paste0(warning.text, s, collapse = "")
      write.table(df, file = output.filename, sep=seperator, quote = FALSE, row.names = FALSE )
    }
  }

  # Create provenance entry
  provenance <- paste0(
    paste0("read.mmm\n"),
    paste0("Data imported by ",toupper(Sys.getenv("LOGNAME"))," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n"),
    paste0("Input file: ",input.filename,"\n"),
    paste0("Specimens:    ",specimen.number,"\n"),
    paste0("Measurements: ",measurement.number,"\n")
  )
  s <- paste0("  ",measurement.names,"\n", collapse = "")
  provenance <- paste0(provenance, s, collapse = "")
  if (length(metadata.cols)>0) {
    provenance <- paste0(provenance,"Includes ",length(df.md.cols)," metadata columns with the names:\n", collapse = "")
    s <- paste0("  ",names(df)[df.md.cols],"\n", collapse = "")
    provenance <- paste0(provenance, s, collapse = "")
  }
  if (apply.scale) {
    if (invert.scale) {
      s <- "SCALE included and INVERTED from the original dataset.\n"
      provenance <- paste0(provenance, s, collapse = "")
    } else {
      s <- "SCALE included directly from the original dataset.\n"
      provenance <- paste0(provenance, s, collapse = "")
    }
  }
  provenance <- paste0(provenance, warning.text, collapse = "")

  # Export provenance
  if (export.provenance) {
    if (!is.null(output.filename)) {
      prov.filename <- output.filename
    } else {
      prov.filename <- default.output.filename
    }
    prov.filename <- paste0(prov.filename,".provenance.txt")
    write(provenance, prov.filename)
  }

  # Prep the final output
  output <- list(
    x = df,
    measurement.number = measurement.number,
    specimen.number = specimen.number,
    scaled = !is.null(apply.scale)
  )
  if (!is.null(specimens.without.scale)) {
    output[["specimens.missing.scale"]] <- specimens.without.scale
  }
  output[["provenance"]] <- list(read.mmm = provenance)

  return(output)

} # End of function

