#' A wrapper function for geomorph::readland.tps
#'
#' This function reads a \code{tps} file to obtain landmark coordinates, using
#' \code{\link[geomorph]{readland.tps}} and includes a few routine follow-up steps.
#'
#' If \code{orient.specimens = TRUE} then the function will also re-orient specimens
#' using \code{\link{orient}}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @return Returns a list with several elements: \code{coords}, containing a 3-dimensional array
#'     of shape coordinates; and \code{landmark.number} and \code{specimen.number},
#'     containing integer values. If \code{include.header = TRUE}, the default, then the output
#'     will also incluide an element \code{provenance}, which is itself a list recording data provenance.
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
#'                    orient.specimens = TRUE,
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
  show.landmark.plot = TRUE,
  square = TRUE,
  links = NULL,
  text.color = "darkred",
  line.color = "darkgray",
  orient.specimens = FALSE,
  topLM = NULL,
  bottomLM = NULL,
  leftLM = NULL,
  rightLM = NULL,
  verbose = TRUE,
  include.header = TRUE
) {
  coords <- readland.tps(
    file = file,
    specID = specID,
    negNA = negNA,
    readcurves = readcurves,
    warnmsg = warnmsg
  )

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

  # Orient specimens
  if (orient.specimens) {
    coords <- orient(coords,
                     topLM = topLM, bottomLM = bottomLM,
                     leftLM = leftLM, rightLM = rightLM,
                     include.plot = FALSE,
                     verbose = verbose
    )
  }

  output <- list(
    coords = coords,
    landmark.number = dim(coords)[1],
    specimen.number = dim(coords)[3]
  )

  # Include header information
  if (include.header) {
    header.text <- read.delim(file, header = FALSE, sep = "\n", stringsAsFactors = FALSE)[,1]
    x <- grep("LM=",header.text)[1]
    if (x > 1) {
      header.text <- header.text[1:(x-1)]
      header.text <- sub("# ","",header.text)
      header.text <- paste0(header.text, collapse = "\n")
      output[["provenance"]][["tps.header"]] <- header.text
    }
  }

  return(output)
}
