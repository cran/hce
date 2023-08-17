#' Simulate `hce` object with given event rates of time-to-event outcomes, mean and SD of the continuous outcome by treatment group
#'
#' @param n sample size in the active treatment group.
#' @param n0 sample size in the placebo group.
#' @param TTE_A event rates per year in the active group for the time-to-event outcomes.
#' @param TTE_P event rates per year in the placebo group for the time-to-event outcomes. Should have the same length as TTE_A.
#' @param CM_A mean value for the continuous outcome of the active group.
#' @param CM_P mean value for the continuous outcome of the placebo group.
#' @param CSD_A standard deviation for the continuous outcome of the active group.
#' @param CSD_P standard deviation for the continuous outcome of the placebo group.
#' @param fixedfy length of follow-up in years.
#' @param yeardays number of days in a year.
#' @param pat scale of provided event rates (per pat-years).
#' @param seed the seed for generating random numbers.
#' @return an object of class hce containing the following columns:
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
#' @seealso [hce::hce()], [hce::as_hce()] for the helper a coerce function to hce objects.
#' @examples
#' # Example 1
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' dat <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#' CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3)
#' head(dat)
#' 
#'# Example 2
#' Rates_A <- c(5, 7)
#' Rates_P <- c(7, 10)
#' dat <- simHCE(n = 1000, n0 = 500, TTE_A = Rates_A, TTE_P = Rates_P, CM_A = 2, CM_P = 0)
#' summaryWO(dat)

simHCE <- function(n, n0 = n, TTE_A, TTE_P, CM_A, CM_P, CSD_A = 1, CSD_P = 1, fixedfy = 1, yeardays = 360, pat = 100, seed = NULL ){
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



  fixedf <- yeardays*fixedfy
  TTE_P <- TTE_P/yeardays/pat
  TTE_A <- TTE_A/yeardays/pat
  ####################################
  l <- base::length(TTE_A)
  if(is.null(seed)){
    seed <- base::as.numeric(base::Sys.time())
  }
  seed <- seed[1]
  base::set.seed(seed)


  M_P <- base::sapply(TTE_P, stats::rexp, n = n0)
  M_P <- round(M_P)
  M_P <- base::as.data.frame(M_P)
  base::names(M_P) <- base::paste0("TTE", 1:l)
  M_P$TRTP <- "P"
  M_P$ID <- (n + 1):(n0 + n)

  M_A <- base::sapply(TTE_A, stats::rexp, n = n)
  M_A <- round(M_A)
  M_A <- base::as.data.frame(M_A)
  base::names(M_A) <- base::paste0("TTE", 1:l)
  M_A$TRTP <- "A"
  M_A$ID <- 1:n

  dat <- base::rbind(M_A, M_P)
  dat$PADY <- fixedf
  dat$seed <- seed
  
  
  dat$AVALT <- base::apply(dat[, base::startsWith(names(dat), "TTE")], 1, function(x) x[x <= fixedf][1])
  dat$AVALT[is.na(dat$AVALT)] <- fixedf
  dat$EVENTN <- base::apply(dat[, base::startsWith(names(dat), "TTE")], 1, function(x) which(x <= fixedf)[1])
  dat$EVENTN[is.na(dat$EVENTN)]  <- l + 1
  ###############################################################
  dat$EVENT <- base::factor(dat$EVENTN, levels = 1:(l+1))
  base::levels(dat$EVENT) <- base::c(paste0("TTE", 1:l), "C")
  dat$GROUP <- base::as.character(dat$EVENT)
  dat$GROUPN <- dat$EVENTN*dat$PADY

  lencont_A <- dat$GROUP=="C" & dat$TRTP=="A"
  lencont_P <- dat$GROUP=="C" & dat$TRTP=="P"
  

  dat[lencont_A, "AVAL0"] <- round(stats::rnorm(base::sum(lencont_A), mean = CM_A, sd = CSD_A), 2)
  dat[lencont_P, "AVAL0"] <- round(stats::rnorm(base::sum(lencont_P), mean = CM_P, sd = CSD_P), 2)
  dat[base::is.na(dat$AVAL0), "AVAL0"] <- dat[base::is.na(dat$AVAL0), "AVALT"]
  mcont <- min(dat[dat$GROUP == "C", "AVAL0"])
  dat$AVAL <- ifelse(dat$GROUP != "C", dat$AVAL0 + dat$GROUPN, dat$AVAL0 - mcont + 1 + dat$GROUPN)

  dat <- dat[ , c("ID", "TRTP", "GROUP", "GROUPN", "AVALT", "AVAL0", "AVAL", "seed")]
  dat$PADY <- fixedf
  
  dat <- as_hce(dat)
  return(dat)

}



