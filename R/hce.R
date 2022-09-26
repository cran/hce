#' Helper function for the hce object
#'
#' @param AVAL a numeric vector of analysis values.
#' @param TRTP a character vector of the same length as AVAL containing assigned treatment groups.
#' @returns an object of class hce. Its is a subject-level data frame (each row corresponds to one subject), containing the following columns:
#' * SUBJID subject ID.
#' * GROUP a character vector specifying the type of the outcome the patient experienced - either a TTE (time-to-event) or C (continuous).
#' * GROUPN a numeric vector version of the `GROUP` column.
#' * AVAL0 original analysis values - time of the time-to-event outcomes or the continuous outcome.
#' * AVAL derived analysis value `AVAL = AVAL0 + GROUPN`.
#' * TRTP assigned treatment groups.
#' @export
#' @md
#' @seealso [hce::new_hce()], [hce::validate_hce()]  for the helper and validator functions of hce.
#' @examples
#' set.seed(2022)
#' d <- hce(AVAL = c(rnorm(5, mean = 1), rnorm(5)), TRTP = rep(c("A", "P"), each = 5))
#' calcWO(d)
hce <- function(AVAL = numeric(), TRTP = character()){

  if(length(AVAL) != length(TRTP))
    stop("The AVAL and TRTP vectors should have the same length")

  d <- data.frame(AVAL = AVAL, TRTP = TRTP)
  d <- validate_hce(d)
  new_hce(d)
}
