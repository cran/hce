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
#'
#' @examples
#' summaryWO(data = COVID19, GROUP ~ TRTP)
#' summaryWO(data = COVID19, GROUP ~ TRTP, GROUP = "GROUP", ref = "Placebo")

summaryWO.formula <- function(x, data, ...){
  Args <- base::list(...)
  formulavars <- base::all.vars(x)
  formula0 <- stats::reformulate(formulavars[2], response = formulavars[1])
  mf <- stats::model.frame(formula = formula0, data = data)
  Level <- base::unique(mf[[formulavars[2]]])
  Levellength <- base::length(Level)
  
  if(Levellength != 2){
    message1 <- base::paste("The variable", formulavars[2], "should have exactly 2 unique values, but has", Levellength)
    stop(message1)
  }
  if(!base::is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if (base::all(Level == c("A", "P"))) ref <- "P"
  else ref <- Level[1]
  if(! ref %in% Level) stop(base::paste("Choose the reference from the values",
                                        base::paste(Level, collapse = ", ")))
  if(!base::is.null(Args[["GROUP"]])) {
    GROUP <- Args[["GROUP"]]
    mf$GROUP <- data[, "GROUP", drop = T]
  }
  else GROUP <- NULL
  res <- summaryWO.data.frame(x = mf, AVAL = formulavars[1], TRTP = formulavars[2], ref = ref, GROUP = GROUP)
  res$formula <- base::deparse(formula0)
  res$ref <- base::paste(Level[Level != ref],"vs", ref)
  res
}