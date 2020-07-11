#' Euclidean distance between points in 2D space
#'
#' @param xy A numerical vector of length 2, representing a point in Cartesian space
#' @param XY A numerical vector of length 2, representing a point in Cartesian space
#'
#' @return Returns the Euclidean distance
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' distance(c(9,49),c(24,54))
#'
#' distance(c(2,5),c(8,1))
#'
#' x <- matrix(c(9,49, 24,54, 51,28, 81,54, 81,23, 86,32), ncol = 2, byrow = TRUE)
#'
#' df <- data.frame(matrix(nrow = dim(x)[1], ncol = dim(x)[1]))
#'
#' for (i in (1:dim(x)[1])) {
#'   df[i,] <- unlist(apply(x, 1, distance, XY=x[i,]))
#' }
#'
#' df
#'

distance <- function(xy,XY) { sqrt((xy[1]-XY[1])^2+(xy[2]-XY[2])^2) }
