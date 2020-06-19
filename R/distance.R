#' Euclidean distance between points in 2D space
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'

distance <- function(x1,y1,x2,y2) { d <- sqrt((x1-x2)^2+(y1-y2)^2); return(d) }
