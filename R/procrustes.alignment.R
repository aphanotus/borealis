#' A wrapper for geomorph::gpagen that includes interactive outlier detection and removal
#'
#' If \code{outlier.analysis = TRUE} then the function will display the results of
#' \code{\link[geomorph]{plotOutliers}}, then warp grids for the most extreme shapes.
#' The user is then prompted to select a cetain number of extreme shapes to exclude from
#' a repeated Procrustes alignment. This processes is iterative, until the user indicates that
#' it should stop or no shapes are requested for removal.
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
#' @param A A list or 3-dimensional array containing XY shape corrdinates for multiple specimens.
#' @param show.plot.gpa A logical value specifying whether to plot the Procrustes-aligned landmarks.
#' @param outlier.analysis A logical value specifying whether to poerform interactive outlier analysis.
#' @param display.outliers The number of outlier shapes to display.
#' @param provenance An object that should be retained for data provenance.
#'     Unnecessary if \code{A} is a list that already contains a \code{provenance} element.
#'
#' @export
#'
#' @examples
#' data("Bombus.forewings", package = "borealis")
#'
#' names(Bombus.forewings)
#' cat(Bombus.forewings$provenance[[1]])
#'
#' fw.gpa <- procrustes.alignment(Bombus.forewings, show.plot.gpa = FALSE)
#' plot(fw.gpa)
#'
#' fw.gpa <- procrustes.alignment(Bombus.forewings, outlier.analysis = TRUE)
#'
#' names(fw.gpa$provenance)
#' cat(fw.gpa$provenance$GPA)
#'

procrustes.alignment <- function (
  A,
  show.plot.gpa = TRUE,
  outlier.analysis = FALSE,
  display.outliers = 4,
  provenance = NULL,
  ...
)
{
  # Don't bother running anything if geomorph isn't installed!
  if (!require(geomorph)) {
    return("Please run  install.packages('geomorph'). ")
  }

  shape.data <- NULL

  if (class(A)[1] %in% c("gpagen","list")) {
    shape.data <- A$coords
    if (is.null(provenance) & any(grepl("provenance",names(A)))) { provenance <- A$provenance }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
      if (is.null(provenance)) {
        cat("Warning: No data provenance provided.\n")
      }
    } else {
      return(cat("Error: Input is not a recognized type. (See the help entry: '?procrustes.alignment'.)"))
    }
  }

  # Do the GPA
  GPA <- gpagen(shape.data, ...)
  new.prov.details <- paste0(
    paste0("## Generalized Procrustes analysis\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::procrustes.alignment` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )

  # GPA Plot
  if (show.plot.gpa) {
    plot(GPA)
  }

  # Outlier analysis
  if (outlier.analysis) {

    # Pause if the user requests both the GPA plot and outlier analysis
    if (show.plot.gpa) { invisible(readline(prompt="Press any key to continue.")) }

    names.of.outliers.removed <- NULL

    # Outlier Procrustes distances
    rounds.of.outlier.analysis <- 0
    outliers <- plotOutliers(GPA$coords)
    h <- ceiling(sqrt(display.outliers))
    w <- ifelse( ( h^2 - display.outliers) >= h, h-1, h )

    readline(prompt="Press any key to continue.")

    continue.loop <- TRUE
    while (continue.loop) {

      # Plot the worst outliers compared to the consensus GPA shape
      par(mfrow=c(h,w))
      for (i in 1:display.outliers) {
        plotRefToTarget(GPA$consensus,GPA$coords[,,outliers[i]])
        text(0,0.25,
             paste('outlier #',i,': ',dimnames(shape.data)[[3]][outliers[i]],sep=''),
             col='darkred', font=2)
      }
      par(mfrow=c(1,1))

      # Remove the extreme outlier. I think this is the one where I entered zeros for missing data
      rm.outliers <- readline(prompt="Remove how many of the most extreme outliers? ('Enter' or '0' for none.) ")
      if (rm.outliers %in% c(""," ","  ")) {
        rm.outliers <- NULL
      }
      else {
        if (!(rm.outliers %in% dimnames(shape.data)[[3]])) {
          rm.outliers <- as.integer(rm.outliers)
          if (rm.outliers > 0) {
            rm.outliers <- 1:rm.outliers
          } else {
            rm.outliers <- NULL
          }
        }
      }
      if (!is.null(rm.outliers)) {
        names.of.outliers.removed <- c(names.of.outliers.removed, dimnames(shape.data)[[3]][outliers[rm.outliers]])
        shape.data <- shape.data[,,-outliers[rm.outliers]]
        rounds.of.outlier.analysis <- rounds.of.outlier.analysis + 1

        # Repeat GPA
        GPA <- gpagen(shape.data, ...)

        if (show.plot.gpa) {
          plot(GPA)
          invisible(readline(prompt="Press any key to continue."))
        }

        outliers <- plotOutliers(GPA$coords)

        # Ask to continue
        if (continue.loop) {
          continue.loop <- askYesNo("Would you like to perform another round of outlier analysis?")
        }

      } else {
        continue.loop <- FALSE
      } # End if/else  (rm.outliers > 0)

    } # End while-loop

    if (!is.null(names.of.outliers.removed)) {
      new.prov.details <- paste0(new.prov.details,"The following ",length(names.of.outliers.removed)," specimens were removed after ",rounds.of.outlier.analysis," rounds of outlier analysis: \n- ",paste0(names.of.outliers.removed, collapse = "\n- "),"\n\n")
    }

  } # End outlier analysis

  # Add provenance
  GPA$provenance <- provenance
  GPA$provenance$procrustes.alignment <- new.prov.details

  return(GPA)

} # End of function
