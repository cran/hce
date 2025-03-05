#' Calculates patient-level individual win proportions
#'
#' @param data a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values.
#' @param TRTP the treatment variable in the data.
#' @param ref the reference treatment group.
#' @return the input data frame with a new column of individual win proportions named using the input `AVAL` value with `_`.   
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::calcWO.hce()], [hce::calcWO.formula()].
#' @references Gasparyan SB et al. "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2 (2021): 580-611. <doi:10.1177/0962280220942558>
#' @examples
#' KHCE1 <- IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)
#' WP <- tapply(KHCE1$EGFRBL_, KHCE1$TRTPN, mean)
#' VAR <- tapply(KHCE1$EGFRBL_, KHCE1$TRTPN, function(x) (length(x)-1)*var(x)/length(x))
#' N <- tapply(KHCE1$EGFRBL_, KHCE1$TRTPN, length)
#' SE <- sqrt(sum(VAR/N))
#' c(WP = WP[[1]], SE = SE)
#' calcWO(EGFRBL ~ TRTP, data = KHCE)[c("WP", "SE_WP")]
IWP <- function(data, AVAL, TRTP, ref){
  data <- as.data.frame(data)
  AVAL <- AVAL[1]
  ref <- ref[1]
  TRTP <- TRTP[1]
  
  if (!AVAL %in% base::names(data)) 
    stop(paste0("The variable ", AVAL, " is not in the dataset."))
  if (!TRTP %in% base::names(data)) 
    stop(paste0("The variable ", TRTP, " is not in the dataset."))
  data$AVAL <- data[, AVAL]
  data$TRTP <- data[, TRTP]
  if (length(unique(data$TRTP)) != 2) 
    stop("The dataset should contain two treatment groups.")
  if (!ref %in% unique(data$TRTP)) 
    stop("Choose the reference from the values in TRTP.")
  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")
  data$ID <- 1:nrow(data)
  ID0 <- c(data$ID[data$TRTP == "A"], data$ID[data$TRTP == "P"])
  A <- base::rank(c(data$AVAL[data$TRTP == "A"], data$AVAL[data$TRTP == 
                                                             "P"]), ties.method = "average")
  B <- base::tapply(data$AVAL, data$TRTP, base::rank, ties.method = "average")
  n <- base::tapply(data$AVAL, data$TRTP, base::length)
  n1 <- n[["A"]]
  n0 <- n[["P"]]
  d <- base::data.frame(R1 = A, R2 = base::c(B$A, B$P), TRTP = base::c(base::rep("A", 
                                                                                 n1), base::rep("P", n0)))
  d$R <- d$R1 - d$R2
  d$R0 <- base::ifelse(d$TRTP == "A", d$R/n0, d$R/n1)
  data[ID0, paste0(AVAL, "_")] <- d$R0
  data
}


