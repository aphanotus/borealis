#' Consistently orient specimens in an array of coordinate shape values
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' If no values are given for the top, bottom, left or right landmarks, then all specimens
#' will be oriented to match the first specimen in the array.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A 3-dimensional array containing XY shape corrdinates for multiple specimens,
#'     or a list containing such as an array and data provenance.
#' @param topLM The row number of the landmark that should be on top.
#' @param bottomLM The row number of the landmark that should be on the bottom.
#' @param leftLM The row number of the landmark that should be on the left.
#' @param rightLM The row number of the landmark that should be on the right.
#' @param include.plot A logical factor specifying whether to include a plot showing th first specimen in the array, after orientation.
#' @param links A matrix with two columns indicating landmarks to connect by lines in the plot. Or \code{"chull"} can be used to draw a convex hull.
#' @param verbose A logical factor specifying whether to report out corrections to each specimen.
#' @param provenance An object that should be retained for data provenance.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#'
#' curated.coords <- orient(plethodon$land,
#'                          topLM = 8, bottomLM = 2,
#'                          leftLM = 12, rightLM = 7,
#'                          links = pletho.links )
#'

orient <- function(A, topLM = NULL, bottomLM = NULL, leftLM = NULL, rightLM = NULL, include.plot = TRUE, links = NULL, verbose = TRUE, provenance = NULL)
{
  shape.data <- NULL
  output <- NULL

  if (class(A)[1] %in% c("gpagen","list")) {
    shape.data <- A$coords
    # if (is.null(provenance) & any(grepl("provenance",names(A)))) { provenance <- A$provenance }
    output <- A[-grep("coords",names(A))]
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
    } else {
      return(cat("Error: Input is not a recognized type. (See the help entry: '?procrustes.alignment'.)"))
    }
  }
  if (dim(shape.data)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }

  if (is.null(topLM)) { topLM <- which.max(shape.data[,2,1]) }
  if (is.null(bottomLM)) { bottomLM <- which.min(shape.data[,2,1]) }
  if (is.null(leftLM)) { leftLM <- which.min(shape.data[,1,1]) }
  if (is.null(rightLM)) { rightLM <- which.max(shape.data[,1,1]) }

  landmark.number <- dim(shape.data)[1]
  total.number.flipped <- 0
  for (i in 1:(dim(shape.data)[3])) {
    OKright <- (shape.data[rightLM,1,i] > shape.data[leftLM,1,i])
    OKup    <- (shape.data[topLM,2,i] > shape.data[bottomLM,2,i])
    if (OKright & OKup) { if (verbose) { cat(dimnames(shape.data)[[3]][i],' ok\n') } }
    else { total.number.flipped <- total.number.flipped +1 }
    if (!OKright) { # If not, mirror the left side to the right.)
      mean.x <- mean(shape.data[,1,i])
      centered.x <- shape.data[,,i] - cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      centered.x <- centered.x * cbind(rep(-1,landmark.number),
                                       rep(1,landmark.number))
      shape.data[,,i] <- centered.x + cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      if (verbose) { cat(dimnames(shape.data)[[3]][i],' X-flipped\n') }
    }
    if (!OKup) { # If not, mirror the top to the bottom.)
      mean.y <- mean(shape.data[,2,i])
      centered.y <- shape.data[,,i] - cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      centered.y <- centered.y * cbind(rep(1,landmark.number),
                                       rep(-1,landmark.number))
      shape.data[,,i] <- centered.y + cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      if (verbose) { cat(dimnames(shape.data)[[3]][i],' Y-flipped\n') }
    }
  }
  cat ("\n",total.number.flipped,"specimens re-oriented.")
  if (include.plot) {
    landmark.plot(shape.data[,,1], links = links)
  }

  output <- list(output, coords = shape.data)
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
      output[["provenance"]] <- provenance
    }
  output[["provenance"]][["reorientation"]] <- paste0(
    paste0("## Specimen Re-orientation\n\nSpecimens re-orient by ",toupper(Sys.getenv("LOGNAME"))," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- topLM = ",topLM,"\n"),
    paste0("- bottomLM = ",bottomLM,"\n"),
    paste0("- leftLM = ",leftLM,"\n"),
    paste0("- rightLM = ",rightLM,"\n")
  )

  return(output)
} # End of function
