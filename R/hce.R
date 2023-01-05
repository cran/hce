#' Helper function for `hce` objects
#'
#' @param GROUP a character vector of the same length as `AVAL` containing events.
#' @param TRTP a character vector of the same length as `AVAL` containing assigned treatment groups.
#' @param AVAL0 a numeric vector of analysis values within each category. The default is 0.
#' @param ORD a character vector containing ordered unique values of the `GROUP` variable for determining the hierarchy of events.
#' @returns an object of class `hce`. Its is a subject-level data frame (each row corresponds to one subject), containing the following columns:
#' * SUBJID subject ID.
#' * GROUP a character vector specifying the type of the outcome the patient experienced - either a TTE (time-to-event) or C (continuous).
#' * GROUPN a numeric vector version of the `GROUP` column.
#' * AVAL0 original analysis values - time of the time-to-event outcomes or the continuous outcome.
#' * AVAL derived analysis value `AVAL = AVAL0 + GROUPN`.
#' * TRTP assigned treatment groups.
#' @export
#' @md
#' @seealso [hce::new_hce()], [hce::validate_hce()]  for the helper and validator functions of `hce` objects.
#' @examples
#' # Example 1
#' set.seed(2022)
#' d <- hce(GROUP = sample(x = c("A", "B", "C"), size = 10, replace = TRUE), 
#' TRTP = rep(c("Active", "Control"), each = 5), 
#' AVAL0 = c(rnorm(5, mean = 1), rnorm(5)), ORD = c("A", "B", "C"))
#' calcWO(d, ref = "Control")

hce <- function(GROUP = character(), TRTP = character(), AVAL0 = 0, ORD = sort(unique(GROUP))){
  if(length(TRTP) == 0)
    stop("TRTP is required.")
  if(length(GROUP) == 0)
    stop("GROUP is required.")
  
  if(!any(ORD %in% GROUP))
    stop("ORD should contain events from the variable GROUP")
  
  ORD <- ORD[ORD %in% GROUP]
  EVENT <- ordered(GROUP, levels = ORD) 
  
  EVENTN <- as.numeric(EVENT) - 1
  ord <- length(ORD)*max(abs(AVAL0))
  SEQ <- c(0, 1, 10, 100, 1000, 1000, 10000, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10)
  i <- which(ord < SEQ)[1]
  ord <- SEQ[i]
  GROUPN <- EVENTN*ord
  d <- data.frame(TRTP = TRTP, GROUP = as.character(GROUP), GROUPN = GROUPN, AVAL = AVAL0 + GROUPN, AVAL0 = AVAL0, ord = ord)
  d <- validate_hce(d)
  new_hce(d)
}