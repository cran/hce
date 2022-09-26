#' Constructor function for the hce object
#'
#' @param x a data frame.
#'
#' @return an object of class hce.
#' @export
#' @md
#' @seealso [hce::hce()], [hce::validate_hce()]  for the helper and validator functions of hce.
#' @examples
#' data(HCE1)
#' dat <- new_hce(x = HCE1)
#' class(dat)
#' calcWO(dat)
new_hce <- function(x = data.frame()){
  base::stopifnot(base::is.data.frame(x))
  base::structure(x, class = c("hce", "data.frame"))
}
