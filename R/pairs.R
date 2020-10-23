#' Scatterplot Matrices
#'
#' An enhanced version of the \code{graphics::pairs} function,
#'     incorporating the suggested in the example code to create upper and lower
#'     triangular split showing correlation coefficients and a smoothed trend line.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x  A matrix or data/frame.
#'     Logical and factor columns are converted to numeric.
#'     Character columns are ignored.
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

pairs <- function(x, col.smooth = "darkred", gap = 0.5, ...) {

  panel.cor <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = 2)[1]
    text(0.5, 0.5, txt, cex = 0.75/strwidth(txt) * r)
  }
  panel.smooth <- function(x, y) {
    graphics::panel.smooth(x, y, lwd = 3, col.smooth = col.smooth)
  }

  i <- unlist(lapply(x, function(n) {!is.character(n)}))

  graphics::pairs(x[,i], lower.panel = panel.smooth, upper.panel = panel.cor,
                  gap = gap, ...)
}

