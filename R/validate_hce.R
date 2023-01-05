#' Validator function for `hce` objects
#'
#' @param x an object used for the `hce` validation.
#'
#' @return a validated data frame that can be used for creating an `hce` object.
#' @export
#' @md
#' @seealso [hce::hce()], [hce::new_hce()]  for the helper and constructor functions of `hce` objects.
#' @examples
#' data(HCE1)
#' validate_hce(HCE1)
validate_hce <- function(x){
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
