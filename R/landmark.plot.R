#' Plot the relative position of landmarks
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param xy A 2D matrix of X and Y corrdinates.
#' @param square A logical factor specifying whether the aspect ratio of the plot should be equal.
#' @param links A matrix with two columns indicating landmarks to connect by lines. Or \code{"chull"} can be used to draw a convex hull.
#' @param text.color Color names or value for the text.
#' @param line.color Color names or value for lines.
#'
#' @export
#'
#' @examples
#' landmark.plot(plethodon$land[,,1])
#'
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,4,3,5),
#'                        ncol = 2, byrow = TRUE)
#' landmark.plot(plethodon$land[,,1], links = pletho.links )
#'
#' landmark.plot(plethodon$land[,,1], links = "chull")
#'

landmark.plot <- function (xy, square = TRUE, links = NULL, text.color = "darkred", line.color = "darkgray") {
  if (dim(xy)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }
  plot(xy, type='n', asp = square, xlab = 'x', ylab = 'y')
  if (!is.null(links)) {
    if (links[[1]] == "chull") {
      links <- grDevices::chull(xy)
      links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
      for (i in 1:(dim(links)[1])) {
        segments(xy[links[i,1],1], xy[links[i,1],2], xy[links[i,2],1], xy[links[i,2],2], col = line.color )
      }
    } else {
      if (dim(links)[2] != 2) {
        cat("Warning: Links must be a matrix with two columns of landmark numbers")
      } else {
        for (i in 1:(dim(links)[1])) {
          segments(xy[links[i,1],1], xy[links[i,1],2], xy[links[i,2],1], xy[links[i,2],2], col = line.color )
        }
      }
    }
  }
  for (i in 1:(dim(xy)[1])) { text(xy[i,1], xy[i,2], labels=i, col = text.color) }
}

