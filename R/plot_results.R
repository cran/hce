#' A print method for `hce_results ` objects
#'
#' @param x an object of class `hce_results`.
#' @param ... additional arguments to be passed to [base::plot()] function.
#' @return no return value, called for plotting.
#' @export
#' @md
#' @examples
#' WO <- minWO(N = 100:1000)
#' plot(WO)
#' POW <- powerWO(N = 100:1000, WO = 1.2)
#' plot(POW, ylim = c(0, 1))
plot.hce_results <- function(x, ...){
  x <- attr(x, "res")
  Args <- base::list(...)
  Args[["x"]] <- x$input
  Args[["y"]] <- x$value

  if ( is.null(Args[["xlab"]]) & is.null(Args[["ylab"]])) {
    Args[["xlab"]] <- x$input_name
    Args[["ylab"]] <- x$value_name
  } else if (is.null(Args[["xlab"]])) {
    Args[["xlab"]] <- x$input_name
  } else if (is.null(Args[["ylab"]])) {
    Args[["ylab"]] <- x$value_name
  }


 if (is.null(Args[["type"]])) Args[["type"]] <- "l"

 base::do.call(base::plot, Args)

}





