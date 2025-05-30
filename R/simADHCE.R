#' Simulate `adhce` object with given event rates of time-to-event outcomes (Weibull), mean and SD of the continuous outcome (normal or log-normal) by treatment group
#'
#' @param n sample size in the active treatment group.
#' @param n0 sample size in the placebo group.
#' @param TTE_A event rates per year in the active group for the time-to-event outcomes.
#' @param TTE_P event rates per year in the placebo group for the time-to-event outcomes. Should have the same length as `TTE_A`.
#' @param CM_A mean value for the continuous outcome of the active group.
#' @param CM_P mean value for the continuous outcome of the placebo group.
#' @param CSD_A standard deviation for the continuous outcome of the active group.
#' @param CSD_P standard deviation for the continuous outcome of the placebo group.
#' @param fixedfy length of follow-up in years.
#' @param yeardays number of days in a year.
#' @param pat scale of provided event rates (per `pat`-years).
#' @param shape shape of the Weibull distribution for time-to-event outcomes. Default is exponential distribution with `shape = 1`.
#' @param logC logical, whether to use log-normal distribution for the continuous outcome.
#' @param seed for generating random numbers.
#' @param dec decimal places for the continuous outcome used for rounding. The default is `dec = 2`.
#' @return an object of class `adhce` containing an `hce` object with its source datasets `ADET` (event-time dataset for all time-to-event outcomes per patient)
#' and `BDS` (basic data structure for the continuous outcome for all patients). The `hce` object has the following columns:
#' * ID subject identifier.
#' * TRTP planned treatment group - "A" for active, "P" for Placebo.
#' * GROUP type of the outcome, either "TTE" for time-to-event outcomes or "C" for continuous. 
#' Only one continuous outcome is possible, but no restriction on the number of "TTE" outcomes.
#' * GROUPN order of outcomes in `GROUP`, with a higher value signifying a better outcome.
#' * AVALT the timing of the time-to-event outcomes.
#' * AVAL0 numeric values of the continuous outcome and the timing of "TTE" outcomes.
#' * AVAL analysis values derived as `AVAL0 + GROUPN`. For the continuous outcome the values of `AVAL0` are shifted to start always from 0.
#' * seed the seed of the random sample. If not specified in `seed` argument will be selected based on system time.
#' * PADY primary analysis day, the length of fixed follow-up in days calculated as `yeardays` multiplied by `fixedfy`.
#' @export
#' @md
#' @seealso [hce::simHCE()] for directly simulating `hce` objects.
#' @examples
#' # Example 1
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' l <- simADHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'               CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3, seed = 2024)
#' names(l)
#' head(l$HCE)
#' head(l$ADET)
#' head(l$BDS)
#' 
#' # Example 2
#' Rates_A <- 10
#' Rates_P <- 15
#' l <- simADHCE(n = 1000, n0 = 500, TTE_A = Rates_A, TTE_P = Rates_P, 
#' CM_A = 0.1, CM_P = 0, seed = 5, shape = 0.2, logC = TRUE, dec = 0)
#' summaryWO(l$HCE)

simADHCE <- function(n, n0 = n, TTE_A, TTE_P, CM_A, CM_P, CSD_A = 1, CSD_P = CSD_A, fixedfy = 1, yeardays = 360, pat = 100, shape = 1, logC = FALSE, seed = NULL, dec = 2 ){
  if(base::length(TTE_P) != base::length(TTE_A))
    stop("Event rate vectors for active and placebo groups should have the same length")
  
  n <- n[1]
  n0 <- n0[1]
  CM_A <- CM_A[1]
  CM_P <- CM_P[1]
  CSD_A <-  CSD_A[1]
  CSD_P <-  CSD_P[1]
  fixedfy <- fixedfy[1]
  yeardays <- yeardays[1]
  pat <- pat[1]
  seed <- seed[1]
  logC <- as.logical(logC[1])
  shape <- shape[1]
  dec <- dec[1]
  
  fixedf <- yeardays*fixedfy
  TTE_P <- TTE_P/yeardays/pat
  TTE_A <- TTE_A/yeardays/pat
  
  l <- base::length(TTE_A)
  if(is.null(seed)){
    seed <- base::as.numeric(base::Sys.time())
  }
  seed <- seed[1]
  base::set.seed(seed)
  
  
  M_P <- base::sapply(1/TTE_P, stats::rweibull, n = n0, shape = shape)
  M_P <- round(M_P)
  M_P <- base::as.data.frame(M_P)
  base::names(M_P) <- base::paste0("TTE", 1:l)
  M_P$TRTP <- "P"
  M_P$ID <- (n + 1):(n0 + n)
  
  M_A <- base::sapply(1/TTE_A, stats::rweibull, n = n, shape = shape)
  M_A <- round(M_A)
  M_A <- base::as.data.frame(M_A)
  base::names(M_A) <- base::paste0("TTE", 1:l)
  M_A$TRTP <- "A"
  M_A$ID <- 1:n
  
  dat <- base::rbind(M_A, M_P)
  dat$PADY <- fixedf
  dat$seed <- seed
  
  
  dat$AVALT <- base::apply(dat[, base::startsWith(names(dat), "TTE"), drop = FALSE], 1, function(x) x[x <= fixedf][1])
  dat$AVALT[is.na(dat$AVALT)] <- fixedf
  dat$EVENTN <- base::apply(dat[, base::startsWith(names(dat), "TTE"), drop = FALSE], 1, function(x) which(x <= fixedf)[1])
  dat$EVENTN[is.na(dat$EVENTN)]  <- l + 1
  
  dat$EVENT <- base::factor(dat$EVENTN, levels = 1:(l+1))
  base::levels(dat$EVENT) <- base::c(paste0("TTE", 1:l), "C")
  dat$GROUP <- base::as.character(dat$EVENT)
  dat$GROUPN <- dat$EVENTN*dat$PADY
  
  lXA <- round(stats::rlnorm(n, meanlog = CM_A, sdlog = CSD_A), dec)
  lXP <- round(stats::rlnorm(n0, meanlog = CM_P, sdlog = CSD_P), dec)
  XA <- round(stats::rnorm(n, mean = CM_A, sd = CSD_A), dec)
  XP <- round(stats::rnorm(n0, mean = CM_P, sd = CSD_P), dec)
  
  if(logC) {
    dat[dat$TRTP == "A", "AVAL0"] <- lXA
    dat[dat$TRTP == "P", "AVAL0"] <- lXP
  } else {
    dat[dat$TRTP == "A", "AVAL0"] <- XA
    dat[dat$TRTP == "P", "AVAL0"] <- XP
  }
  
  BDS <- dat[, c("ID", "TRTP", "AVAL0")]
  names(BDS)[names(BDS) == "AVAL0"] <- "AVAL"
  
  dat[dat$GROUP != "C", "AVAL0"] <- dat[dat$GROUP != "C", "AVALT"]
  mcont <- min(dat[dat$GROUP == "C", "AVAL0"])
  dat$AVAL <- ifelse(dat$GROUP != "C", dat$AVAL0 + dat$GROUPN, 
                     dat$AVAL0 - mcont + 1 + dat$GROUPN)
  
  TRANSPOSE <- function(x) {
    d <- as.data.frame(t(x))
    d$ID <- as.numeric(names(d))
    names(d)[1] <- "AVAL"
    d$EVENT <- row.names(d)
    row.names(d) <- NULL
    d  
  }
  l <- lapply(split(dat[, base::startsWith(names(dat), "TTE"), drop = FALSE], dat$ID), TRANSPOSE)
  res <- do.call(rbind, l)
  row.names(res) <- NULL
  res <- res[res$AVAL <= fixedf, c("ID", "AVAL", "EVENT")]
  res$PADY <- fixedf
  
  
  dat <- dat[, c("ID", "TRTP", "GROUP", "GROUPN", "AVALT", "AVAL0", "AVAL", "seed")]
  dat$PADY <- fixedf
  dat <- as_hce(dat)
  l <- list(ADET = res, BDS = BDS, HCE = dat)
  return(l)
}



