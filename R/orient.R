#' Consistently orient specimens in an array of coordinate shape values
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' If no values are given for the top, bottom, left or right landmarks, then all specimens
#' will be oriented to match the first specimen in the array.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A 3-dimensional array containing XY shape corrdinates for multiple specimens.
#' @param topLM The row number of the landmark that should be on top.
#' @param bottomLM The row number of the landmark that should be on the bottom.
#' @param leftLM The row number of the landmark that should be on the left.
#' @param rightLM The row number of the landmark that should be on the right.
#' @param include.plot A logical factor specifying whether to include a plot showing th first specimen in the array, after orientation.
#' @param links A matrix with two columns indicating landmarks to connect by lines in the plot. Or \code{"chull"} can be used to draw a convex hull.
#' @param verbose A logical factor specifying whether to report out corrections to each specimen.
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

orient <- function(A, topLM = NULL, bottomLM = NULL, leftLM = NULL, rightLM = NULL, include.plot = TRUE, links = NULL, verbose = TRUE)
{
  if (is.null(topLM)) { topLM <- which.max(A[,2,1]) }
  if (is.null(bottomLM)) { bottomLM <- which.min(A[,2,1]) }
  if (is.null(leftLM)) { leftLM <- which.min(A[,1,1]) }
  if (is.null(rightLM)) { rightLM <- which.max(A[,1,1]) }

  landmark.number <- dim(A)[1]
  total.number.flipped <- 0
  for (i in 1:(dim(A)[3])) {
    OKright <- (A[rightLM,1,i] > A[leftLM,1,i])
    OKup    <- (A[topLM,2,i] > A[bottomLM,2,i])
    if (OKright & OKup) { if (verbose) { cat(dimnames(A)[[3]][i],' ok\n') } }
    else { total.number.flipped <- total.number.flipped +1 }
    if (!OKright) { # If not, mirror the left side to the right.)
      mean.x <- mean(A[,1,i])
      centered.x <- A[,,i] - cbind(rep(mean.x,landmark.number),
                                   rep(0,landmark.number))
      centered.x <- centered.x * cbind(rep(-1,landmark.number),
                                       rep(1,landmark.number))
      A[,,i] <- centered.x + cbind(rep(mean.x,landmark.number),
                                   rep(0,landmark.number))
      if (verbose) { cat(dimnames(A)[[3]][i],' X-flipped\n') }
    }
    if (!OKup) { # If not, mirror the top to the bottom.)
      mean.y <- mean(A[,2,i])
      centered.y <- A[,,i] - cbind(rep(0,landmark.number),
                                   rep(mean.y,landmark.number))
      centered.y <- centered.y * cbind(rep(1,landmark.number),
                                       rep(-1,landmark.number))
      A[,,i] <- centered.y + cbind(rep(0,landmark.number),
                                   rep(mean.y,landmark.number))
      if (verbose) { cat(dimnames(A)[[3]][i],' Y-flipped\n') }
    }
  }
  cat ("\n",total.number.flipped,"specimens re-oriented.")
  if (include.plot) {
    landmark.plot(A[,,1], links = links)
  }
  return(A)
} # End of function



