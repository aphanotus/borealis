#' Plot the relative position of landmarks
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A 2D matrix of X and Y shape corrdinates,
#'     a 3-dimensional array containing XY corrdinates for multiple specimens, or
#'     a list containing such as an array.
#' @param specimen.number If an array is provided, the specimen number to plot.
#' @param square A logical factor specifying whether the aspect ratio of the plot should be equal.
#' @param links A matrix with two columns indicating landmarks to connect by lines. Or \code{"chull"} can be used to draw a convex hull.
#' @param text.color Color names or value for the text.
#' @param line.color Color names or value for lines.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' # The function will detect whether the input object is
#' # a 2D set of coordinates or a 3D array
#' # The following are all equivalant
#' landmark.plot(plethodon$land)
#' landmark.plot(plethodon$land, specimen.number = 1)
#' landmark.plot(plethodon$land[,,1])
#'
#' # Convex hull
#' landmark.plot(plethodon$land, links = "chull")
#'
#' # Custom landmark connections
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#' landmark.plot(plethodon$land, links = pletho.links )
#'

landmark.plot <- function (A, specimen.number = 1, square = TRUE, links = NULL, text.color = "darkred", line.color = "darkgray", ...)
{

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      landmarks <- A$coords[,,specimen.number]
    } else {
      if (any(grepl("land",names(A)))) {
        landmarks <- A$land[,,specimen.number]
      } else {
        return(cat("Error: Input is not a recognized type. (See the help entry: '?landmark.plot'.)"))
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      landmarks <- A[,,specimen.number]
    } else {
      if ((class(A)[1] == "matrix") & (dim(A)[2] == 2)) {
        landmarks <- A
      } else {
        return(cat("Error: Input is not a recognized type. (See the help entry: '?landmark.plot'.)"))
      }
    }
  }

  # Fail safes
  if (length(dim(landmarks)) == 3) { landmarks <- landmarks[,,specimen.number] }
  if (dim(landmarks)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }
  if (!is.null(links)) {
    if ((max(links) > dim(landmarks)[1]) | (min(links) < 1)) {
      cat("Warning: Provided links are out of bounds. (See the help entry: '?landmark.plot'.)")
      links <- NULL
    }
  }

  # Plot
  plot(landmarks, type='n', asp = square, xlab = 'x', ylab = 'y', ...)
  if (!is.null(links)) {
    if (links[[1]] == "chull") {
      links <- grDevices::chull(landmarks)
      links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
      for (i in 1:(dim(links)[1])) {
        segments(landmarks[links[i,1],1], landmarks[links[i,1],2], landmarks[links[i,2],1], landmarks[links[i,2],2], col = line.color )
      }
    } else {
      if (dim(links)[2] != 2) {
        cat("Warning: Links must be a matrix with two columns of landmark numbers")
      } else {
        for (i in 1:(dim(links)[1])) {
          segments(landmarks[links[i,1],1], landmarks[links[i,1],2], landmarks[links[i,2],1], landmarks[links[i,2],2], col = line.color )
        }
      }
    }
  } # End  if (!is.null(links))

  # Landmark labels
  for (i in 1:(dim(landmarks)[1])) { text(landmarks[i,1], landmarks[i,2], labels=i, col = text.color) }

} # End of function

