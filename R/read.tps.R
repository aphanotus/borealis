#' Read shape data from a TPS file
#'
#' A wrapper function for \code{\link[geomorph]{readland.tps}}, which reads a \code{tps} file to
#' obtain landmark coordinates.
#'
#' If \code{extract.id.metadata = TRUE} (the default) then the function will extracts
#' metadata embedded in the ID lines based on the separator and metadata factor names
#' described in the header of the \code{tps} file. If \code{reflect.specimens = TRUE}
#' then the function will also re-orient specimens using \code{\link{align.reflect}}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @return Returns a list with several elements: \code{coords}, containing a 3-dimensional array
#'     of shape coordinates; and \code{landmark.number} and \code{specimen.number},
#'     containing integer values.
#'     \code{scaled} is a logical flag for whether scaling has been applied to shape coordinates.
#'     Data provenance is incluided in an element, \code{provenance},
#'     which is itself a list recording data manipulations made by functions in the
#'     \code{\href{https://github.com/aphanotus/borealis}{borealis}} package.
#'
#' @references  Rohlf, FJ. 2015. The tps series of software. \emph{Hystrix} 26, 9–12.
#' (\href{https://doi.org/10.4404/hystrix-26.1-11264}{Link})
#' @references  Geomorph: Software for geometric morphometric analyses.
#' R package version 3.2.1. D.C. Adams and M.L. Collyer and A. Kaliontzopoulou. 2020.
#' (\href{https://cran.r-project.org/package=geomorph}{Link})
#'
#' @export
#'
#' @examples
#' shapes <- read.tps("sample.tps", show.landmark.plot = FALSE)
#'
#' shapes <- read.tps("sample.tps")
#'
#' shapes <- read.tps("sample.tps", links = "chull")
#'
#' shapes <- read.tps("sample.tps",
#'                    links = "chull",
#'                    reflect.specimens = TRUE,
#'                    verbose = FALSE)
#'
#' names(shapes)
#'
#' shapes$landmark.number
#'
#' shapes$specimen.number
#'
#' cat(shapes$provenance)
#'

read.tps <- function (
  file,
  specID = "ID",
  negNA = FALSE,
  readcurves = FALSE,
  warnmsg = TRUE,
  extract.id.metadata = TRUE,
  keep.original.ids = FALSE,
  show.landmark.plot = TRUE,
  square = TRUE,
  links = NULL,
  text.color = "darkred",
  line.color = "darkgray",
  reflect.specimens = FALSE,
  top.pt = NULL,
  bottom.pt = NULL,
  left.pt = NULL,
  right.pt = NULL,
  verbose = TRUE
)
{
  # Don't bother running anything if geomorph isn't installed!
  if (!require(geomorph)) {
    return("Please run  install.packages('geomorph'). ")
  }

  warning.text <- NULL

  coords <- readland.tps(
    file = file,
    specID = specID,
    negNA = negNA,
    readcurves = readcurves,
    warnmsg = warnmsg
  )

  # Orient specimens
  reflect.provenance <- NULL
  if (reflect.specimens) {
    x <- align.reflect(coords,
                       top.pt = top.pt, bottom.pt = bottom.pt,
                       left.pt = left.pt, right.pt = right.pt,
                       show.plot = FALSE, verbose = verbose )
    coords <- x$coords
    reflect.provenance <- x$provenance$align.reflect
  }

  # Landmark plot
  if (show.landmark.plot) {
    landmark.plot(
      coords[,,1],
      square = square,
      links = links,
      text.color = text.color,
      line.color = line.color
    )
  }

  # Include header information
  header.text <- NULL
  metadata <- NULL
  scaled <- FALSE
  header.text <- read.delim(file, header = FALSE, sep = "\n", stringsAsFactors = FALSE)[,1]
  data.line1 <- grep("^LM=",header.text)[1]

  if (data.line1 > 1) {

    # Is scale present?
    scaled <- any(grepl("^SCALE=",header.text))

    # Save ID lines
    id.lines <- header.text[grep("^ID=",header.text)]
    id.lines <- sub("^ID=","",id.lines)

    # Truncate header.text to lines before the data
    header.text <- header.text[1:(data.line1-1)]
    header.text <- sub("^# ","",header.text)

    # Extract the names of the embedded metadata factors
    id.factors <- header.text[grep("^- ",header.text)]
    id.factors <- sub("^- ","",id.factors)

    separator <- header.text[grep("^Metadata separator: ",header.text)]
    if (length(separator) >0) {
      separator <- sub("^Metadata separator: ","",separator[[1]])
    } else {
      separator <- NULL
    }

    # Extract ID metadata
    if (extract.id.metadata & !is.null(separator)) {
      if (!is.null(id.lines)) {
        # Make sure the separator appears in the ID names!
        if (!any(grepl(separator,id.lines))) {
          s <- paste0("Warning: The separator ",paste0("'",separator,"'")," does not appear in the ID information. e.g. ",paste0("'",id.lines[1],"'"),".\n")
          cat(s)
          warning.text <- paste0(warning.text, "\n", s, collapse = "")
        }

        # Extract metadata from the ID string
        id.factors <- c("specimen.id",id.factors)
        metadata <- data.frame(matrix(ncol = length(id.factors)))
        colnames(metadata) <- id.factors
        missing.data <- NULL
        for (i in 1:length(id.lines)) {
          x <- unlist(strsplit(id.lines[i],separator))
          # Check that there's no right-hand over-run
          if (length(x) > dim(metadata)[2]) {
            metadata <- cbind(metadata, data.frame(matrix(ncol = length(x) - dim(metadata)[2])))
          } else {
            # Check whether any rows fail to fill the columns specified by id.factors
            if (length(x) < dim(metadata)[2]) { missing.data <- c(missing.data, id.lines[i]) }
          }
          metadata[i,] <- x
        }

        # Report on any missing data
        if (!is.null(missing.data)) {
          s <- paste0("Warning: The following specimen IDs appear to have missing data: \n- ",paste0(missing.data, collapse = "\n- "),"\n")
          cat(s)
          warning.text <- paste0(warning.text, "\n", s, collapse = "")
        }

        # If requested, keep the full original IDs from the tps file
        if (keep.original.ids) {
          metadata$specimen.id <- dimnames(coords)[[3]]
        } else {
          dimnames(coords)[[3]] <- metadata$specimen.id
        }

      } else {
        s <- "Warning: No specimen IDs detected.\n"
        cat(s)
        warning.text <- paste0(warning.text, "\n", s, collapse = "")
      }
    } # End   if (extract.id.metadata)

    # Add create.tps provenance
    header.text <- paste0(paste0(header.text, collapse = "\n"),"\n\n")
  } # End  if (data.line1 > 1)

  # Prep the output
  output <- list(
    coords = coords,
    landmark.number = dim(coords)[1],
    specimen.number = dim(coords)[3],
    scaled = scaled,
    metadata = metadata
  )
  output$provenance$create.tps <- header.text

  # Add current data provenance
  s <- paste0(
    paste0("## TPS data import\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::read.tps` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )
  if (extract.id.metadata) {
    s <- paste0(s, paste0("Metadata were extracted from specimen ID lines for the following factors:\n- ",paste0(id.factors, collapse = '\n- '),"\n\n"), collapse = "")
  }
  if (keep.original.ids) {
    s <- paste0(s, "ID names were kept from the original TPS file IDs. They were not shortened by removal of metadata.\n\n", collapse = "")
  }
  if (!is.null(warning.text)) {
    s <- paste0(s, warning.text, collapse = "")
  }
  output$provenance$read.tps <- s

  if (!is.null(reflect.provenance)) {
    output$provenance$align.reflect <- reflect.provenance
  }

  return(output)
}
