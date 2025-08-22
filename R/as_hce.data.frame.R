#' Coerce a data frame to an `hce` object
#'
#' @param x a data frame.
#' @param ... additional parameters.
#'
#' @return an `hce` object.
#' @export
#' @md
#' @seealso [hce::as_hce()], [hce::as_hce.default()].
#' @examples
#' # The case when all required variables `AVAL0`, `GROUP`, `PADY`, and `TRTP` are present.
#' KHCE <- as_hce(KHCE)
#' ## Converts to an `adhce` object
#' class(KHCE)
#' calcWO(KHCE)
#' # The case when only `AVAL` and `TRTP`.
#' ## Converts to an `hce` object
#' dat <- KHCE[, c("TRTP", "AVAL")]
#' dat <- as_hce(dat)
#' class(dat)
#' summaryWO(dat)
as_hce.data.frame <- function(x, ...){
  ## Validator function for hce objects
  .validate_hce <- function(x){
    x <- base::as.data.frame(x)
    Names <- base::names(x)
    if(base::any(base::dim(x) == 0))
      stop("The dimension of the dataset should be non-zero.")
    if(!"AVAL" %in% Names)
      stop("The dataset should contain the column AVAL for the analysis values.")
    if(! "TRTP" %in% Names)
      stop("The dataset should contain the column TRTP for the planned treatment.")
    if(base::length(base::unique(x$TRTP)) != 2)
      stop("The TRTP column should have exactly 2 levels.")
    if(!is.numeric(x$AVAL))
      stop("AVAL must be numeric.")
    x
  }
  ## Validator function for adhce objects
  .validate_adhce <- function(x){
    Names <- base::names(x)
    C1 <- c("GROUP") %in% Names & is.numeric(x$AVAL0) & is.numeric(x$PADY) & length(unique(x$PADY)) == 1
    if(C1) {
      C0 <- !is.factor(x$GROUP) && is.character(x$GROUP) &&  all(startsWith(x$GROUP[x$GROUP != "C"], "TTE")) && any(x$GROUP == "C")
      if(C0){
        x$GROUP <- as.factor(x$GROUP)
        UNIQ <- unique(x$GROUP)
        LEV <- c(sort(UNIQ[UNIQ != "C"]), UNIQ[UNIQ == "C"])
        x$GROUP <- factor(x$GROUP, levels = LEV)
      } else {
        x$GROUP <- as.factor(x$GROUP)  
      }
      x$PARAMN <- as.numeric(x$GROUP)
      LEVELS <- levels(x$GROUP)
      LAST <- LEVELS[length(LEVELS)]
      stopifnot("All timepoints in `AVAL0` for the time-to-event endpoints should be less than or equal to the evaluation timeframe specified in `PADY`." = all(x$AVAL0[x$GROUP != LAST] <= x$PADY[x$GROUP != LAST]))
      m <- min(x$AVAL0[x$GROUP == LAST], na.rm = TRUE)
      x$GROUPN <- x$PARAMN*x$PADY
      x$AVAL <- ifelse(x$GROUP != LAST, x$AVAL0 + x$GROUPN, x$AVAL0 - m + 1 + x$GROUPN)
      return(x)
    } else {
      return(x)
    }
  }
  ## Constructor function for hce and adhce objects
  .new_hce <- function(x = data.frame()){
    base::stopifnot(base::is.data.frame(x))
    x1 <- .validate_adhce(x)
    if(identical(x, x1)){
      x <- .validate_hce(x)
      y <- base::structure(x, class = c("hce", "data.frame"))  
    } else {
      x1 <- .validate_hce(x1)
      y <- base::structure(x1, class = c("adhce", "hce", "data.frame"))  
    }
    y
  }
  
  .new_hce(x)
}
