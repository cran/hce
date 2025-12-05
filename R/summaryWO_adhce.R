#' Win odds summary for `adhce` objects
#'
#' @param x an `adhce` object.
#' @param ... additional parameters.
#' @returns a list containing the summary of wins, losses, and ties. It contains the following named objects:
#' * summary a data frame containing number of wins, losses, and ties by treatment group and the overall number of comparisons.
#' * summary_by_GROUP a summary data frame by `GROUP`.
#' * WO calculated WO (win odds) and WP (win probability) and their standard errors.
#' * cumsummary_by_GROUP a cumulative summary data frame by `GROUP`.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::summaryWO()], [hce::summaryWO.data.frame()], [hce::summaryWO.formula()], [hce::summaryWO.hce()] methods.
#' @examples
#' ## Example 1 - using an `hce` object
#' HCE5 <- HCE4
#' HCE5$PADY <- NULL
#' dat <- as_hce(HCE5)
#' ## `PADY` is not present in the dataset, hence converts it to an `hce` object 
#' ## instead of an `adhce` object.
#' ## Example 2 - Using an `adhce` object
#' class(dat)
#' summaryWO(dat, ref = "P")
#' ## The class is `adhce` hence will use the variable `GROUP`.
#' HCE5$PADY <- 1080
#' dat <- as_hce(HCE4) 
#' class(dat)
#' summaryWO(dat, ref = "P")
#' ## Example 3 - Plotting cumulative components of an `adhce` object
#' dat <- as_hce(KHCE)
#' res0 <- summaryWO(dat, ref = "P")
#' res <- res0$cumsummary_by_GROUP
#' barplot(PROP ~ WINS + GROUPN, data = res, 
#' col = c("darkgreen", "darkred", "darkblue"), 
#' xlab = "Proportions", xlim = c(0, 1), 
#' ylab = "Cumulative components by prioritization", 
#' legend.text = unique(res$WINS), beside = TRUE, horiz = TRUE)
#' grid()
summaryWO.adhce <- function(x, ...){
  Args <- base::list(...)
  x <- as_hce(x)
  x <- base::as.data.frame(x)
  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]
  r0 <- summaryWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", ref = ref, GROUP = "GROUP")
  r1_ <- r0$summary_by_GROUP
  # Check that when some GROUPS levels are not present, they are added as zeros
  rx <- expand.grid(TRTP = c("A", "P"), GROUP = levels(x$GROUP))
  r1 <- merge(rx, r1_, by = c("TRTP", "GROUP"), all.x = TRUE)
  r1[is.na(r1)] <- 0
  ###########
  r2 <- tapply(r1$LOSS, r1$TRTP, cumsum)
  TOTAL <- r0$summary$TOTAL[1]
  GROUP <- levels(x$GROUP)
  LAB <- c(paste(unique(x$TRTP)[unique(x$TRTP) != ref], "wins"), paste(ref, "wins"), "Ties")
  r3 <- data.frame(GROUPN = rep(1:length(GROUP), GROUP = rep(GROUP, times = 3), times = 3), 
                   WINS = rep(LAB, each = length(GROUP)),
                   COUNT = c(r2$P, r2$A, TOTAL - r2$A - r2$P), TOTAL = TOTAL)
  r3$PROP <- r3$COUNT/r3$TOTAL
  r3 <- r3[order(r3$GROUPN), ]
  r0$cumsummary_by_GROUP <- r3
  r0
}
