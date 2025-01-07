#' A plot method for `hce` objects
#'
#' @param x an object of class `hce` created by `as_hce()`.
#' @param ... additional arguments to be passed to [base::plot()] function.
#'
#' @return no return value, called for plotting.
#' @export
#' @md
#' @references Bamber D. "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4 (1975): 387-415. <doi:10.1016/0022-2496(75)90001-2>
#' @examples
#' d <- as_hce(KHCE)
#' res <- calcWO(AVAL ~ TRTP, data = d)
#' plot(d, col = 2, type = 'l', ylab = "Active", xlab = "Control", lwd = 2)
#' polygon(c(0, 1, 1, 0), c(1, 1, 1, 0), col = 2)
#' legend("bottomright", legend = paste0("WP = ", round(res$WP, 5)))

plot.hce <- function(x, ...){
  x <- as_hce(x)
  xran <- seq(min(x$AVAL), max(x$AVAL), length.out = 10000)
  FUN <- tapply(x$AVAL, x$TRTP, stats::ecdf)
  d <- data.frame(x = FUN$P(xran), y = FUN$A(xran))
  d <- unique(d)
  plot(y ~ x, data = d, ...)
}



