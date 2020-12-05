#' Generalized AIC for adonis models
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'

adonis.AIC <- function (fit) {
  # Generalized AIC = 2k + n [Ln(2(pi) RSS/n)+1]
  k <- ncol(fit$model.matrix)
  n <- nrow(fit$model.matrix)
  x <- dim(fit$aov.tab)[1] -1
  rss <- fit$aov.tab$SumsOfSqs[x]
  return((  (2*k) + n*(log(2*pi*rss/n) + 1)  ))
}
