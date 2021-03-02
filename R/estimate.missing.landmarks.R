#' Estimate the location of missing landmarks
#'
#' This function is a wrapper for \code{geomorph:estimate.missing}, which adds
#' records of the specimens and landmarks altered by the imputation of missing
#' landmarks.
#'
#' Additional arguments are passed to \code{geomorph::estimate.missing}.
#'
#' @param A A 3-dimensional array containing XY shape coordinates for multiple specimens, or a list containing such as an array and data provenance.
#' @param na.value A numeric value to treat as missing data, (\code{NA}).
#'     For example, tpsDig2 (starting with version 2.18) records missing landmarks as
#'     coordinate values of -1.0. Setting \code{na.value = -1} will provide
#'     compatibility with these data.
#' @param verbose A logical argument specifying whether to display metrics each
#'     during each iteration.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references Adams and M.L. Collyer and A. Kaliontzopoulou. 2020. Geomorph: Software for geometric morphometric analyses. R package version 3.2.1. D.C. (\href{https://cran.r-project.org/package=geomorph}{Link})
#' @references  Rohlf, FJ. 2015. The tps series of software. \emph{Hystrix} 26, 9–12.
#' (\href{https://doi.org/10.4404/hystrix-26.1-11264}{Link})
#'
#' @export
#'
#' @examples
#' j <- data("Jadera", package = "borealis")
#'
#' j$coords[30,,c(1:3)] <- NA
#' j$coords[37,,c(1,4)] <- NA
#'
#' j2 <- estimate.missing.landmarks(j)
#' cat(j2$provenance$estimate.missing.landmarks)
#' landmark.plot(j2, specimen.number = 1:4, axes = TRUE)
#'

estimate.missing.landmarks <- function ( A, na.value = NA, verbose = TRUE, ... )
{ # Begin the function

  # Check packages
  if (!("geomorph" %in% rownames(installed.packages()))) {
    stop("Package missing. First, try running `install.packages('geomorph')`")
  }

  # Initialize
  shapes <- NULL
  output <- NULL

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      shapes <- A$coords
      output <- A[-grep("coords",names(A))]
    } else {
      if (any(grepl("land",names(A)))) {
        shapes <- A$land
        output <- A[-grep("land",names(A))]
      } else {
        stop("Error: Input is not a recognized type. (See the help entry: `?estimate.missing.landmarks`.)\n")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?estimate.missing.landmarks`.)\n")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?estimate.missing.landmarks`.)\n")
  }
  if (!any(is.na(unlist(shapes)))) {
    stop("No missing landmarks are present. (See the help entry: `?estimate.missing.landmarks`.)\n")
  }
  # End preliminary vetting of the input and arguments

  if (!is.na(na.value)) {
    shapes[(shapes == na.value)] <- NA
  }

  # Find a list of specimens and their missing landmarks
  # missing.values <- data.frame(matrix(c(NA,NA), ncol = 2, dimnames = list(NULL,c("specimen","lm"))))
  na.specimens <- NULL
  na.landmarks <- NULL
  for (i in 1:(dim(shapes)[3])) {  # for each specimen
    for (j in 1:(dim(shapes)[1])) {  # for each landmark
      if (any(is.na(shapes[j,,i]))) {
        na.specimens <- c(na.specimens,i)
        na.landmarks <- c(na.landmarks,j)
      }
    }
  }

  # Do the thing
  shapes <- geomorph::estimate.missing(shapes, ...)

  # Prep the output
  output$coords <- shapes
  if (!any(grepl("provenance",names(output)))) {
    output$provenance <- list()
  }

  na.landmarks <- paste0("landmark ", na.landmarks)
  if (is.null(dimnames(shapes)[[3]])) {
    na.specimens <- paste0("specimen ", na.specimens)
  } else{
    na.specimens <- dimnames(shapes)[[3]][na.specimens]
  }

  s <- paste0(
    paste0("## Estimate missing landmarks\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::estimate.missing.landmarks` version ",packageVersion("borealis")," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("### Landmarks estimated\n\n")
  )

  # Loop to add specimen names and landmark estimates
  for (i in 1:length(na.specimens)) {
    s <- paste0(s,
                paste0("- ",na.specimens[i],": ",na.landmarks[i],"\n"))
  }

  if (verbose) {
    cat("Landmarks estimated\n")
    for (i in 1:length(na.specimens)) {
      cat(paste0("- ",na.specimens[i],": ",na.landmarks[i],"\n"))
    }
  }

  output$provenance$estimate.missing.landmarks <- s

  return(output)

} # End of function
