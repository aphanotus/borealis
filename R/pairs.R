#' Scatterplot Matrices
#'
#' An enhanced version of the \code{graphics::pairs} function,
#'     incorporating some of the example code from the \code{graphics::pairs} help page
#'     to create upper and lower triangular split showing correlation coefficients
#'     and a trend line.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x  A matrix or data/frame.
#'     Logical and factor columns are converted to numeric.
#'     Character columns are ignored.
#' @param cor.method A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param trendline A logical argument specifying whether to include a linear trend line.
#' @param loess A logical argument specifying whether to include a smoothed trend line.
#' @param col.smooth A color to be used by \code{lines} for drawing the smooths.
#' @param gap A distance between subplots, in margin lines.
#' @param ... Arguments to be passed to \code{graphics::pairs}.
#'
#' @export
#'
#' @examples
#'
#' pairs(ChickWeight)
#'

pairs <- function(x,
                  cor.method = "pearson",
                  trendline = TRUE,
                  loess = FALSE,
                  span = 2/3,
                  line.col = "darkred",
                  gap = 0.5,
                  ...)
{ #  Begin function

  x <- as.data.frame(x)

  panel.cor <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, method = cor.method))
    txt <- format(c(r, 0.123456789), digits = 2)[1]
    text(0.5, 0.5, txt, cex = 0.75/strwidth(txt) * r)
  }
  panel.smooth <- function(x, y) {
    graphics::panel.smooth(x, y, lwd = 3, span = span, col.smooth = line.col)
  }
  panel.lm <- function (x, y) {
    points(x, y)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) { abline(lm(y[ok]~x[ok]), lwd = 3, col = line.col) }
  }

  i <- unlist(lapply(x, function(n) {!is.character(n)}))

  if (trendline) {
    graphics::pairs(x[,i], lower.panel = panel.lm, upper.panel = panel.cor,
                    gap = gap, ...)
  }

  if (loess) {
    graphics::pairs(x[,i], lower.panel = panel.smooth, upper.panel = panel.cor,
                    gap = gap, ...)
  }

  if (!(trendline | loess)) {
    graphics::pairs(x[,i], upper.panel = panel.cor, gap = gap, ...)
  }

} # End function

