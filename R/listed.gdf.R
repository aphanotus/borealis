#' Converts list elements gpagen and metadata into a geomorph data frame
#'
#' The input must be a list containing an element of type \code{gpagen}.
#' This is ideally produced by \code{\link[borealis]{procrustes.alignment}}.
#' If the input contains a \code{metadata} element, then it and the \code{gpagen} element are
#' used to create a new list element of class \code{geomorph.data.frame}.
#' This element can be used in downstream functions. However, keeping it as an element within
#' a larger list object allows retension of other elements, such as data provenance.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references  Geomorph: Software for geometric morphometric analyses.
#' R package version 3.2.1. D.C. Adams and M.L. Collyer and A. Kaliontzopoulou. 2020.
#' (\href{https://cran.r-project.org/package=geomorph}{Link})
#'
#' @param x A list containing the output of \code{\link[geomorph]{gpagen}}, metadata and provenance.
#'
#' @export
#'
#' @examples
#' library(geomorph)
#' data("Bombus.forewings", package = "borealis")
#'
#' names(Bombus.forewings)
#' cat(Bombus.forewings$provenance)
#'
#' Y.gpa <- gpagen(Bombus.forewings$coords)
#'
#' Y.gdf <- id.metadata.to.gdf(
#'   Y.gpa,
#'   id.factors = c("id","species","caste","digitizer"),
#'   provenance = Bombus.forewings$provenance
#' )
#'
#' names(Y.gdf)
#'
#' landmark.plot(Y.gdf$coords)
#' hist(Y.gdf$Csize)
#' with(Y.gdf, boxplot(Csize ~ species))
#' cat(Y.gdf$provenance)
#'

listed.gdf <- function (x)
{
  # Don't bother running anything if geomorph isn't installed!
  if (!require(geomorph)) {
    stop("Please run  install.packages('geomorph'). ")
  }

  # Vet the input
  if (!any(grepl("list",class(x)))) {
    stop("Error: Input is not a recognized type. (See the help entry: '?listed.gdf'.)")
  }

  gpa <- NULL
  if (!any(grepl("gpagen",names(x)))) {
    if (any(grepl("gdf",names(x)))) {
      warning("Input already contains a `gdf` element. (See the help entry: '?listed.gdf'.)")
      return(x)
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: '?listed.gdf'.)")
    }
  } else {
    gpa <- x$gpagen
  }

  # Create the inital geomorph.data.frame
  if (any(grepl("metadata",names(x)))) {
    if (!is.null(x$metadata)) {
      gdf <- geomorph.data.frame(gpa, x$metadata)
    } else {
      gdf <- geomorph.data.frame(gpa)
    }
  } else {
    gdf <- geomorph.data.frame(gpa)
  }

  # Prep the output
  output <- NULL
  output$gdf <- gdf

  # Other list elements to retain from the input
  additional.elements.of.x <- which(!(names(x) %in% c("coords","Csize","gpagen","metadata")))
  for (i in additional.elements.of.x) {
    j <- length(names(output)) +1
    output[[j]] <- x[[i]]
    names(output)[j] <- names(x)[i]
  }

  # Add new provenance
  s <- paste0(
    paste0("## Geomorph data frame conversion\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::listed.gdf` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("Current data structure:\n"),
    paste0("- gdf\n  - "),
    paste0(paste0(names(output$gdf), collapse = "\n  - "),"\n- "),
    paste0(paste0(names(output)[-1], collapse = "\n- "),"\n\n")
  )
  output$provenance$listed.gdf <- s

  return(output)

} # End of function
