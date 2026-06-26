#' Win odds summary using formula syntax
#'
#' @param x an object of class formula.
#' @param data a data frame.
#' @param ... additional parameters.
#' @returns a list containing the summary of wins, losses, and ties. It contains the following named objects:
#' * summary a data frame containing number of wins, losses, and ties by treatment group and the overall number of comparisons.
#' * WO calculated WO (win odds) and WP (win probability) and their standard errors.
#' * formula returning the specified formula in the `x` argument.
#' * ref showing how the reference group was selected. Can be modifying by specifying the `ref` argument.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::summaryWO()], [hce::summaryWO.data.frame()], [hce::summaryWO.hce()]  methods.
#' @examples
#' # Example 1
#' summaryWO(data = COVID19, GROUP ~ TRTP)
#' # Example 2 - Individual wins, losses, and ties using conditional variable
#' dat <- COVID19
#' dat$ID <- 1:nrow(dat)
#' summaryWO(data = dat, GROUP ~ TRTP | ID, ref = "Placebo")
summaryWO.formula <- function (x, data, ...) 
{
  Args <- base::list(...)
  x <- as_formulae(x)
  Level <- base::unique(data[, x$TRTP])
  Levellength <- base::length(Level)
  
  if (Levellength != 2) {
    message1 <- base::paste("The variable", x$TRTP, 
                            "should have exactly 2 unique values, but has", Levellength)
    stop(message1)
  }
  if (!base::is.null(Args[["ref"]])) 
    ref <- Args[["ref"]]
  else if (base::all(base::sort(Level) == c("A", "P"))) 
    ref <- "P"
  else ref <- Level[1]
  if (!ref %in% Level) 
    stop(base::paste("Choose the reference from the values", 
                     base::paste(Level, collapse = ", ")))
  res <- summaryWO.data.frame(x = data, AVAL = x$AVAL, 
                              TRTP = x$TRTP, ref = ref, GROUP = x$GROUP)
  res$formula <- x$formula
  res$ref <- base::paste(Level[Level != ref], "vs", ref)
  res
}