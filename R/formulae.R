#' Decompose formula objects 
#'
#' Decompose formula objects to treatment, response, covariate, and grouping variables using standardized names
#' `TRTP`, `AVAL`, `COVAR`, and `GROUP` respectively. 
#'
#' @param x a formula object.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns A list with standardised components named `AVAL`, `TRTP`,
#' `COVAR`, and `GROUP` where present, containing the original variable
#' names extracted from `x`, plus a reconstructed formula.
#' @export
#' @name formulae
#' @md
#' @seealso [hce::summaryWO.formula()], [stratWO.formula()] for use.
#' @examples
#' # Example 1: All variables present
#' as_formulae(x = visit4 ~ treatment + age | sex)
#' # Example 2: Extra covariates, no grouping variable will drop the extra variable
#' as_formulae(x = visit4 ~ treatment + age + sex)
#' # Example 3: When more than two grouping variables, the last one will be used.
#'  as_formulae(x = visit4 ~ treatment + age | sex |site)
#' @export
#' @rdname formulae
as_formulae <- function(x, ...){
  UseMethod("as_formulae")
}
#' @export
#' @rdname formulae
as_formulae.formula <- function(x, ...){
  # clean up all inheritances and apply any built-in checks for formula class
  x <- stats::as.formula(x)
  stopifnot("The formula should be two-sided." = length(x) == 3L)
  
  lhs <- x[[2L]] # The left-hand side is always indexed as 2  
  rhs <- x[[3L]] # The right-hand side is always indexed as 3
  # all.vars() returns a character vector with the extracted names from a `name` object.
  AVAL <- base::all.vars(lhs)
  stopifnot("The response variable should be a single variable." = length(AVAL) == 1L)
  # Check for the presence of a conditioning variable (i.e., a variable on the right-hand side of the formula)
  # If right-hand side contains only one variable, then is.call() is FALSE (it is a `name` object) and subsetting will not work.
  ## && ensures that the rhs is a call before subsetting. In that case, 
  ### 1L index provides the operator (|) and 2L index provides the conditioning variable.
  if(base::is.call(rhs) && base::identical(rhs[[1L]], base::as.name("|"))){
    TRTP <- base::all.vars(rhs[[2L]]) #whatever is before `|`
    GROUP <- base::all.vars(rhs[[3L]]) # whatever is after `|`. Uses the last `|` in case there are several.
    if(length(TRTP) > 1){
      COVAR <- TRTP[[2L]]
    } else {
      COVAR <- NULL
    }
    TRTP <- TRTP[[1L]] # Keep one in case several      
    GROUP <- GROUP[[1L]] # keep one in case several
    value <- list(AVAL = AVAL, TRTP = TRTP, GROUP = GROUP, COVAR = COVAR)
  } else {
    TRTP <- base::all.vars(rhs) ## here indexing will not works since this is not a call object but a name object
    if(length(TRTP) > 1){
      COVAR <- TRTP[[2L]]
    } else {
      COVAR <- NULL
    }
    TRTP <- TRTP[[1L]]
    value <- list(AVAL = AVAL, TRTP = TRTP, COVAR = COVAR)
  }
  AVAL <- as.name(value$AVAL)
  TRTP <- as.name(value$TRTP)
  formula1 <- as.name(TRTP)
  if(!is.null(value$COVAR)){
    formula1 <-  call("+", formula1, as.name(value$COVAR))
  }
  if(!is.null(value$GROUP)){
    formula1 <-  call("|", formula1, as.name(value$GROUP))
  }
  formula1 <- call("~", AVAL, formula1)
  value$formula <- base::deparse(formula1)
  return(value)
}