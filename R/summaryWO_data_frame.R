#' Win odds summary for a data frame
#'
#' @param x a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values.
#' @param TRTP the treatment variable in the data.
#' @param ref the reference treatment group.
#' @param GROUP an optional variable for grouping.
#' @param ... additional parameters.
#' @returns a list containing the summary of wins, losses, and ties. It contains the following named objects:
#' * summary a data frame containing number of wins, losses, and ties by treatment group and the overall number of comparisons.
#' * summary_by_GROUP (if `GROUP` variable is specified) a summary data frame by `GROUP`.
#' * WO calculated WO (win odds) and WP (win probability) and their standard errors.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::summaryWO()], [hce::summaryWO.data.frame()]  methods.
#' @examples
#' summaryWO(x = HCE3, AVAL = "AVAL", TRTP = "TRTP", ref = "P", GROUP = "GROUP")
summaryWO.data.frame <- function(x, AVAL, TRTP, ref, GROUP = NULL, ...){
  data <- as.data.frame(x)
  ref <- ref[1]
  AVAL <- AVAL[1]
  TRTP <- TRTP[1]
  GROUP <- GROUP[1]
  if(! AVAL %in% base::names(data))
    stop(paste0("The variable ", AVAL, " is not in the dataset."))
  if(! TRTP %in% base::names(data))
    stop(paste0("The variable ", TRTP, " is not in the dataset."))
  data$AVAL <- data[, base::names(data) == AVAL]
  data$TRTP <- data[, base::names(data) == TRTP]
  if(!is.null(GROUP)){
    if(! GROUP %in% base::names(data))
      stop(paste0("The variable ", GROUP, " is not in the dataset."))
    data$GROUP <- data[, base::names(data) == GROUP]
   }
  if (length(unique(data$TRTP)) != 2) 
    stop("The dataset should contain two treatment groups.")
  if(! ref %in% unique(data$TRTP)) stop("Choose the reference from the values in TRTP.")

  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")
  A <- base::rank(c(data$AVAL[data$TRTP == "A"], data$AVAL[data$TRTP == "P"]), ties.method = "average")
  B <- base::tapply(data$AVAL, data$TRTP, base::rank, ties.method = "average")
  n <- base::tapply(data$AVAL, data$TRTP, base::length)
  n1 <- n[["A"]]
  n0 <- n[["P"]]
  d <- base::data.frame(R1 = A, R2 = base::c(B$A, B$P), TRTP = base::c(base::rep("A", n1), base::rep("P", n0)), AVAL = data$AVAL)
  
  d$IWTN <- d$R1 - d$R2
  a1 <- base::rank(c(data$AVAL[data$TRTP == "A"], data$AVAL[data$TRTP == "P"]), ties.method = "max")
  a2 <- base::rank(c(data$AVAL[data$TRTP == "A"], data$AVAL[data$TRTP == "P"]), ties.method = "min")
  b1 <- base::tapply(data$AVAL, data$TRTP, base::rank, ties.method = "max")
  b2 <- base::tapply(data$AVAL, data$TRTP, base::rank, ties.method = "min")
  
  d$ITN <- (a1 - a2) - c(b1$A - b2$A, b1$P - b2$P)
  d$IWN <- d$IWTN - d$ITN*0.5
  d$ILN <- ifelse(d$TRTP == "A", n0 - d$IWN - d$ITN, n1 - d$IWN - d$ITN)
  
  DATA <- rbind(data[data$TRTP == "A", ], data[data$TRTP == "P", ])
  DATA[, c("IWN", "ITN", "ILN")] <- rbind(d[d$TRTP == "A", c("IWN", "ITN", "ILN")], d[d$TRTP == "P", c("IWN", "ITN", "ILN")])
  DATA$IWP <- base::ifelse(d$TRTP == "A", d$IWTN/n0, d$IWTN/n1)
  
  res <- stats::aggregate(x = DATA[, c("IWN", "ILN", "ITN")], 
                          by = list (TRTP = DATA$TRTP),  FUN = "sum")
  names(res) <- c("TRTP", "WIN", "LOSS", "TIE")
  res$TOTAL <- res$WIN + res$LOSS + res$TIE
  res$WR <- res$WIN / res$LOSS
  res$WO <- (res$WIN + 0.5*res$TIE)/ (res$LOSS + 0.5*res$TIE)
  
  WP0 <- base::tapply(DATA$IWP, DATA$TRTP, base::mean)
  VAR <- base::tapply(DATA$IWP, DATA$TRTP, function(x) (base::length(x) - 1)*stats::var(x)/base::length(x))
  SE_WP <- base::sqrt(base::sum(VAR/n))
  WP = WP0[["A"]]
  WO = WP/(1 - WP)
  SE <- SE_WP/(WP*(1 - WP))
  out <- base::data.frame(WO = WO, SE = SE, WP = WP, SE_WP = SE_WP)
  
  
  
  if(!is.null(GROUP)){
    res0 <- stats::aggregate(x = DATA[, c("IWN", "ILN", "ITN")], 
                             by = list (TRTP = DATA$TRTP, GROUP = DATA$GROUP),  FUN = "sum")
    names(res0) <- c("TRTP", "GROUP", "WIN", "LOSS", "TIE")
    res0$TOTAL <- res0$WIN + res0$LOSS + res0$TIE
    l <- list(summary = res, summary_by_GROUP = res0, WO = out)
  } else 
    l <- list(summary = res, WO = out)
  
  l
}


