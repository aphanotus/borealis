#' Plot the proportion of variance captured by each component axis of a gm.prcomp object
#'
#' @references  Geomorph: Software for geometric morphometric analyses.
#' R package version 3.2.1. D.C. Adams and M.L. Collyer and A. Kaliontzopoulou. 2020.
#' (\href{https://cran.r-project.org/package=geomorph}{Link})
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x An object of class \code{gm.prcomp}.
#' @param npcs The number of components to be plotted.
#' @param threshold An optional threshold to display. Default is 0.05.
#' @param threshold.color Color for the threshold line.
#' @param method A string indicating how to represent the proportion of variance.
#'     Allowable values are \code{"variance"}, \code{"proportion"}, and \code{"percentage"}. May be abbreviated.
#' @param type A string indicating the type of plot. Options are \code{"barplot"} and \code{"lines"}. May be abbreviated.
#'
#' @export
#'
#' @examples
#' library(geomorph)
#' data(plethodon)
#'
#' # GPA
#' Y.gpa <- gpagen(plethodon$land)
#' # Create a gm.prcomp object
#' PCA <- gm.prcomp(Y.gpa$coords)
#'
#' # scree plot
#' scree.plot(PCA)
#'

scree.plot <- function (x, npcs = NULL, threshold = 0.05, threshold.color = "darkred",
                        method = "percent", type = "barplot", ...) {

  # Check that the input is a PCA object
  if (!any(grepl("prcomp",class(x)))) {
    stop("Error: 'x' must be an object of class 'prcomp' or 'gm.prcomp'.")
  }
  if (!any(grepl("sdev",names(x)))) {
    stop("Error: 'x' must be an object of class 'prcomp' or 'gm.prcomp'.")
  }
  method <- strtrim(method,2)
  if (!method %in% c("va","pr","pe")) {
    stop("Error: 'method' must be one of 'variance', 'proportion' or 'percentage'. Abbreviation to 2 characters is allowed.")
  }
  type <- strtrim(type,1)
  if (!type %in% c("b","l")) {
    stop("Error: 'type' must be one of 'barplot', or 'lines'. Abbreviation to 1 characters is allowed.")
  }

  sqx <- x$sdev^2
  if (is.null(npcs)) { npcs <- length(sqx) }
  if (npcs > length(sqx)) { npcs <- length(sqx) }
  if (npcs < 1) { npcs <- length(sqx) }

  if (method == "va") {
    if (type=="b") { barplot(x$sdev, ylab = "variance", ...) }
    else {
      plot(x=1:npcs, y=x$sdev[1:npcs], type = "b", xlab = NA, xaxt = "n", ylab = "variance", ...)
      axis(1, at = 1:npcs, labels = names(x$sdev)[1:npcs])
    }
    abline(h=threshold, col = threshold.color)
  } else {
    if (method == "pr") {
      if (type=="b") { barplot(sqx[1:npcs]/sum(sqx), ylab = "proportion of variance", ...) }
      else {
        plot(x=1:npcs, y=sqx[1:npcs]/sum(sqx), type = "b", xlab = NA, xaxt = "n", ylab = "proportion of variance", ...)
        axis(1, at = 1:npcs, labels = names(x$sdev)[1:npcs])
      }
      abline(h=threshold, col = threshold.color)
    } else {
      sqx <- 100 * sqx[1:npcs]/(sum(sqx))
      if (type=="b") { barplot(sqx, ylab = "percentage of variance", ...) }
      else {
        plot(x=1:npcs, y=sqx, type = "b", xlab = NA, xaxt = "n", ylab = "percentage of variance", ...)
        axis(1, at = 1:npcs, labels = names(x$sdev)[1:npcs])
      }
      threshold <- 100*threshold
      abline(h=threshold, col = threshold.color)
    }
  }

} # End of function
