#' A plot method for `hce` objects
#' @description
#' Ordinal dominance graph for `hce` objects
#' @param x an object of class `hce` created by `as_hce()`.
#' @param fill logical; if `TRUE` fill the area above the graph.
#' @param ... additional arguments to be passed to [base::plot()] function.
#'
#' @return no return value, called for plotting.
#' @export
#' @md
#' @references Bamber D. "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4 (1975): 387-415. <doi:10.1016/0022-2496(75)90001-2>
#' @examples
#' d <- as_hce(KHCE)
#' d$TRTP <- factor(d$TRTP, levels = c("P", "A"))
#' res <- calcWO(AVAL ~ TRTP, data = d)
#' # Ordinal Dominance Graph
#' plot(d, col = 3, type = 'l')
#' grid()
#' # Area above the Ordinal Dominance Graph
#' plot(d, fill = TRUE, col = "#865A4F", type = 'l', 
#'      lwd = 2, xlab = "Control", ylab = "Active")
#' legend("bottomright", legend = paste0("WP = ", round(res$WP, 5)))
#' abline(a = 0, b = 1, lwd = 2, lty = 2, col = "#999999")
plot.hce <- function(x, fill = FALSE, ...){
  l <- list(...)
  if(is.null(l$type))
    l$type <- "l"
  x <- as_hce(x)
  xran <- seq(min(x$AVAL), max(x$AVAL), length.out = 10000)
  FUN <- tapply(x$AVAL, x$TRTP, stats::ecdf)
  NAM <- names(FUN)
  d <- data.frame(x = FUN[[NAM[1]]](xran), y = FUN[[NAM[2]]](xran))
  d0 <- data.frame(x = c(0), y = c(0))
  d <- rbind(d0, d)
  d <- unique(d)
  y_upper <- rep(max(d$y), length(d$x))
  x_poly <- c(d$x, rev(d$x))
  y_poly <- c(d$y, rev(y_upper))
  names(d) <- NAM
  plot(d, ...)
  if(fill){
    if(!is.null(l$col))
      color <- l$col
    else color <- "#999999"
    graphics::polygon(x_poly, y_poly, col = color)
  }
}



