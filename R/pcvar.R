#' Proprotional Variance from Principal Components
#'
#' The function returns the proportion of variance explained by each axis in a
#' principal component analysis (PCA).
#'
#' This function outputs a named vetor with decimal (numeric) or percent (character)
#' proportions of variance from a PCA. It will take either an object of class \code{prcomp}
#' or standard deviations can be passed to \code{sdev}. The function also works with PCA
#' objects created by \code{geomorph::gm.prcomp}.
#'
#' The same information can be found by running \code{summary} on a \code{prcomp} object
#' but the simpler output of \code{pcvar} makes it convenient to be passes to plotting
#' functions, such various \code{geom}'s in \code{ggplot2}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x An object of class \code{prcomp}
#' @param sdev An alternative way to input standard deviations, as a numeric vector
#' @param dimensions The number of dimensions to return
#' @param rounding The number of decimal places to return
#' @param as.percent A logical factor specifying whether to report values as a percentage
#'
#' @export
#'
#' @examples
#' # PCA
#' car.pca <- prcomp(mtcars[,-c(2,8:11)], center = TRUE, scale. = TRUE)
#'
#' # Report proprotional variance from each PC axis
#' pcvar(car.pca)
#' pcvar(car.pca, as.percent = FALSE)
#' pcvar(car.pca, rounding = 2, as.percent = FALSE)
#' pcvar(car.pca, dimensions = dim(car.pca$x)[2])
#'
#' # Standard deviations can be passed directly
#' pcvar(sdev = car.pca$sdev)
#'
#' # Use the pcvar output to annotate the axes of a PC-space
#' car.pcvar <- pcvar(car.pca)
#' dim1 <- 1
#' dim2 <- 2
#' plot(x = car.pca$x[,dim1], y = car.pca$x[,dim2],
#'      xlab = paste0(names(mtcars.pcvar[dim1])," (",mtcars.pcvar[dim1],")"),
#'      ylab = paste0(names(mtcars.pcvar[dim2])," (",mtcars.pcvar[dim2],")"),
#'      col = as.factor(strtrim(rownames(mtcars),3))
#' )

pcvar <- function (x = NULL, sdev = NULL, dimensions = 5, rounding = 4, as.percent = TRUE )
{
  if (is.null(sdev) & is.null(x)) {
    return(cat("Error: 'x' must be an object of class 'prcomp', or standard deviations must be passed to 'sdev'."))
  }
  if (is.null(sdev) & any(grepl("prcomp",class(x)))) {
    sdev <- x$sdev
  } else {
    if (is.null(sdev) & !any(grepl("prcomp",class(x)))) {
      return(cat("Error: 'x' must be an object of class 'prcomp', or standard deviations must be passed to 'sdev'."))
    }
  }
  if (dimensions > length(sdev)) { dimensions <- length(sdev) }
  proportion.of.variance <- (sdev^2)/(sum(sdev^2))
  if (as.percent) { proportion.of.variance <- paste0(round(proportion.of.variance[1:dimensions]*100,rounding-2),'%') }
  else { proportion.of.variance <- round(proportion.of.variance[1:dimensions],rounding) }
  names(proportion.of.variance) <- paste0('PC',1:length(proportion.of.variance))
  return(proportion.of.variance)
}




