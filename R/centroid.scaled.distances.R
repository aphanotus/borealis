#' Calculate the matrix of distances between shape coordinates, scaled by centroid size
#'
#' @param m A matrix containing XY shape corrdinates for \emph{p} landmarks
#' @param scale A logical argument for whether to scale the distances by centroid size
#' @param symmetrical A logical argument for whether to keep the output matrix symmetrical.
#'     If \code{symmetrical = FALSE} (the default) then the upper triangle and diagonal cells
#'     will be \code{NA}.
#'
#' @return Returns a matrix, with dimensions \emph{p} x \emph{p}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' x <- matrix(c(9,49, 24,54, 51,28, 81,54, 81,23, 86,32), ncol = 2, byrow = TRUE)
#'
#' df <- data.frame(matrix(nrow = dim(x)[1], ncol = dim(x)[1]))
#' for (i in (1:dim(x)[1])) {
#'   df[i,] <- unlist(apply(x, 1, distance, XY=x[i,]))
#' }
#' df
#'
#' centroid.scaled.distances(x, symmetrical = TRUE)
#' centroid.scaled.distances(x, symmetrical = TRUE, scale = FALSE)
#'

centroid.scaled.distances <- function(m, scale = TRUE, symmetrical = FALSE)
{
  n <- dim(m)[1]
  df <- data.frame(matrix(nrow = n, ncol = n))
  for (i in 1:n) {
    df[i,] <- unlist(apply(m, 1, function(x) { distance(x,m[i,]) }) )
  }
  if (!symmetrical) { df[upper.tri(df, diag = TRUE)] <- NA }
  if (scale) {
    centroid <- apply(m,2,mean)
    CS <- mean(apply(m,1, distance, XY=centroid))
    df <- df / CS
  }
  return(df)
} # End function
