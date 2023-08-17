#' Coerce a data frame to an `hce` object
#'
#' @param x a data frame.
#' @param ... additional parameters.
#'
#' @return an `hce` object.
#' @export
#'
#' @examples
#' KHCE <- as_hce(KHCE)
#' calcWO(KHCE)
as_hce.data.frame <- function(x, ...){
  ## Validator function for hce objects
  .validate_hce <- function(x){
    x <- base::as.data.frame(x)
    Names <- base::names(x)
    if(base::any(base::dim(x) == 0))
      stop("The dimension of the dataset should be non-zero")
    if(!"AVAL" %in% Names)
      stop("The dataset should contain the column AVAL for the analysis values")
    if(! "TRTP" %in% Names)
      stop("The dataset should contain the column TRTP for the planned treatment")
    if(base::length(base::unique(x$TRTP)) != 2)
      stop("The TRTP column should have exactly 2 levels")
    if(!is.numeric(x$AVAL))
      stop("AVAL must be numeric")
    x
  }
  ## Constructor function for hce objects
  .new_hce <- function(x = data.frame()){
    base::stopifnot(base::is.data.frame(x))
    x <- .validate_hce(x)
    base::structure(x, class = c("hce", "data.frame"))
  }
  
  .new_hce(x)
}