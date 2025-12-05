#' Coerce a data frame to an `hce` object
#'
#' @param x an object.
#' @param ... additional parameters.
#'
#' @return an `hce` object.
#' @export
#' @md
#' @seealso [hce::as_hce()], [hce::as_hce.data.frame()].
#' @examples
#' dat <- KHCE
#' class(dat) <- "moo" # non-existent class
#' as_hce(dat) # tries to convert to an hce object
#' ## It still works because the inheritance converted it to a data frame
as_hce.default <- function(x, ...){
  ##Sometimes, it is not possible to convert an object directly to a data frame. 
  ##However, you can often add inheritance to an object so that it behaves like a data frame. 
  x <- base::structure(x, class = c(class(x), "data.frame"))
  ## call the data frame method of the function
  as_hce(x)
}