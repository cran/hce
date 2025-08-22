#' Helper function for `hce` objects
#'
#' @param GROUP a character vector or a factor containing events. 
#' If a factor, its levels are used to define the hierarchy. Otherwise, the vector is
#' converted to a factor.
#' @param TRTP a character vector of the same length as `GROUP`, indicating assigned treatment groups.
#' @param AVAL0 a numeric vector of the same length as `GROUP`, indicating containing analysis values within each category. The default is 0.
#' @param PADY numeric specifying the length of follow-up in years.
#' @returns an object of class `hce` or `adhce` (if `AVAL0` is provided). The result 
#' is a subject-level data frame, where each row corresponds to one subject, 
#' @export
#' @md
#' @seealso [hce::as_hce()] for coercing to `hce` objects.
#' @examples
#' # Example 1 - Both `AVAL0` and `PADY` are provided. The output is an `adhce` object.
#' GROUP <- COVID19$GROUP
#' TRTP <- rep(c("A", "P"), each = 531)
#' dat <- hce(GROUP, TRTP, PADY = 10, AVAL0 = rnorm(1062))
#' class(dat)
#' calcWO(dat)
#' summaryWO(dat) # Uses the `GROUP` variable for summary.
#' # Example 2 - Only `AVAL0` is provided, `PADY` is calculated as the maximum of `AVAL0`. 
#' # The output is an `adhce` object.
#' set.seed(2022)
#' d <- hce(GROUP = sample(x = c("A", "B", "C"), size = 10, replace = TRUE), 
#' TRTP = rep(c("Active", "Control"), each = 5), 
#' AVAL0 = c(rnorm(5, mean = 1), rnorm(5)))
#' calcWO(d, ref = "Control")
#' ## modify the hierarchy by proving a factor for the GROUP variable. 
#' ## calcWO() applied to an hce rederives `AVAL` based on the `GROUP` variable.
#' d$GROUP <- factor(d$GROUP, levels = c("C", "B", "A"))
#' calcWO(d, ref = "Control")
#' # Example 3 - Provide only `PADY` and not `AVAL0` will not make any difference.
#' GROUP <- COVID19$GROUP
#' TRTP <- rep(c("A", "P"), each = 531)
#' dat <- hce(GROUP, TRTP, PADY = 10)
#' class(dat)
#' calcWO(dat)
#' dat <- hce(GROUP, TRTP)
#' class(dat)
#' calcWO(dat)
hce <- function (GROUP, TRTP, AVAL0 = NULL, PADY = NULL) 
{
  if (length(TRTP) == 0) 
    stop("TRTP is required.")
  if (length(GROUP) == 0) 
    stop("GROUP is required.")
  d <- data.frame(TRTP = TRTP, GROUP = GROUP)
  PADY <- PADY[1]
  d$AVAL0 <- AVAL0
  if(is.null(PADY) & !is.null(AVAL0))
    PADY <- max(AVAL0)
  d$GROUP <- as.factor(d$GROUP)
  d$AVAL <- d$PARAMN <- as.numeric(d$GROUP)
  d$PADY <- PADY
  as_hce(d)
}