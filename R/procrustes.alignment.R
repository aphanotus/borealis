#' A wrapper for geomorph::gpagen that includes provenance
#'
#' @return Returns an object of class \code{gpagen} with an added element for \code{provenance}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references  Geomorph: Software for geometric morphometric analyses.
#' R package version 3.2.1. D.C. Adams and M.L. Collyer and A. Kaliontzopoulou. 2020.
#' (\href{https://cran.r-project.org/package=geomorph}{Link})
#' @references  Rohlf, FJ. 2015. The tps series of software. \emph{Hystrix} 26, 9–12.
#' (\href{https://doi.org/10.4404/hystrix-26.1-11264}{Link})
#'
#' @param A 3-dimensional array containing XY shape corrdinates for multiple specimens, or a character vector of specimen IDs.
#' @param provenance Any object that should be retained for data provenance.
#'
#' @export
#'
#' @examples
#

procrustes.alignment <- function (A, provenance = NULL, ... )
{
  # Don't bother running anything if geomorph isn't installed!
  if (require(geomorph)) {

    shape.data <- NULL

    if (class(A)[1] %in% c("gpagen","list")) {
      shape.data <- A$coords
      if (is.null(provenance) & any(grepl("provenance",names(A)))) { provenance <- A$provenance }
    } else {
      if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
        shape.data <- A
      } else {
        return(cat("Error: Input is not a recognized type. (See the help entry: '?procrustes.alignment'.)"))
      }
    }

    # Do the GPA
    GPA <- gpagen(shape.data, ...)

    # Add provenance
    if (!is.null(provenance)) {
      GPA$provenance <- provenance
    }
    GPA[["provenance"]][["GPA"]] <- paste0("GPA\nGeneralized Procrustes analysis performed by ",toupper(Sys.getenv("LOGNAME"))," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n",GPA$call,"\n")

    return(GPA)
  }
} # End of function
