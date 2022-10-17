#' Procrustes distance between shapes
#'
#' @param s1 A 2-D matrix of shape coordinates
#' @param s2 A 2-D matrix of shape coordinates
#'
#' @return Returns the Procrustes distance
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#'
#' procrustes.distance(
#'   Bombus.forewings$coords[,,1],
#'   Bombus.forewings$coords[,,2]
#' )
#'
#'

procrustes.distance <- function(s1,s2) {

  # Vet the input
  if (dim(s1)[1]!=dim(s2)[1] | dim(s1)[2]!=dim(s2)[2] |
      length(dim(s1))!=2 | length(dim(s2))!=2) {
    stop("Error: The two shapes have different dimensions. (See the help entry: `?procrustes.distance`.)\n")
  }

  number.of.landmarks <- dim(s1)[1]

  # Procrustes distance is the square root of the sum of squared distances
  # between each landmark on the two specimens
  d <- unlist(lapply(1:number.of.landmarks, function (i) {
    distance(s1[i,],s2[i,])
  }))
  return(sqrt(sum(d^2)))

}
