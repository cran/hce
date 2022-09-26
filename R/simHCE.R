#' Simulate an HCE with given event rates of time-to-event outcomes, mean and SD of the continuous outcome by treatment group
#'
#' @param n sample size per treatment group
#' @param TTE_A event rates in the active group for the time-to-event outcomes.
#' @param TTE_P event rates in the placebo group for the time-to-event outcomes. Should have the same length as TTE_A.
#' @param CM_A mean value for the continuous outcome of the active group.
#' @param CM_P mean value for the continuous outcome of the placebo group.
#' @param CSD_A standard deviation for the continuous outcome of the active group.
#' @param CSD_P standard deviation for the continuous outcome of the placebo group.
#' @param fixedfy length of follow-up in years.
#' @param yeardays number of days in a year.
#' @param pat scale of provided event rates (per pat-years).
#' @param ord the coefficient for creating ordinal values
#' @param seed the seed for generating random numbers.
#' @return an object of class hce.
#' @export
#' @md
#' @seealso [hce::hce()], [hce::new_hce()], [hce::validate_hce()]  for the helper, constructor, and validtaor functions of hce.
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' dat0 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#' CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3)
#' str(dat0)

simHCE <- function(n, TTE_A, TTE_P, CM_A, CM_P, CSD_A = 1, CSD_P = 1, fixedfy = 1, yeardays = 360, pat = 100, ord = 10000, seed = NULL ){
  if(base::length(TTE_P) != base::length(TTE_A))
    stop("Event rate vectors for active and placebo groups should have the same length")

  n <- n[1]
  CM_A <- CM_A[1]
  CM_P <- CM_P[1]
  CSD_A <-  CSD_A[1]
  CSD_P <-  CSD_P[1]
  fixedfy <- fixedfy[1]
  yeardays <- yeardays[1]
  pat <- pat[1]
  ord <- ord[1]


  fixedf <- yeardays*fixedfy
  TTE_P <- TTE_P/yeardays/pat
  TTE_A <- TTE_A/yeardays/pat
  ####################################
  l <- base::length(TTE_A)
  if(is.null(seed))
    seed <- base::as.numeric(base::Sys.time())
  seed <- seed[1]
  base::set.seed(seed)


  M_P <- base::sapply(TTE_P, stats::rexp, n = n)
  M_P <- base::as.data.frame(M_P)
  base::names(M_P) <- base::paste0("TTE", 1:l)
  M_P$TRTP <- "P"
  M_P$ID <- (n + 1):(2*n)

  M_A <- base::sapply(TTE_A, stats::rexp, n = n)
  M_A <- base::as.data.frame(M_A)
  base::names(M_A) <- base::paste0("TTE", 1:l)
  M_A$TRTP <- "A"
  M_A$ID <- 1:n

  dat <- base::rbind(M_A, M_P)
  dat$TTEfixed <- fixedf
  dat$seed <- seed
  dat$AVALT <- base::apply(dat[, base::startsWith(names(dat), "TTE")], 1, base::min)
  dat$EVENTN <- base::apply(dat[, base::startsWith(names(dat), "TTE")], 1, base::which.min)
  ###############################################################
  dat$EVENT <- base::factor(dat$EVENTN, levels = 1:(l+1))
  base::levels(dat$EVENT) <- base::c(paste0("TTE", 1:l), "C")
  dat$GROUP <- base::as.character(dat$EVENT)
  dat$GROUPN <- dat$EVENTN*ord

  lencont_A <- dat$GROUP=="C" & dat$TRTP=="A"
  lencont_P <- dat$GROUP=="C" & dat$TRTP=="P"

  dat[lencont_A, "AVAL0"] <- stats::rnorm(base::sum(lencont_A), mean = CM_A, sd = CSD_A)
  dat[lencont_P, "AVAL0"] <- stats::rnorm(base::sum(lencont_P), mean = CM_P, sd = CSD_P)
  dat[base::is.na(dat$AVAL0), "AVAL0"] <- dat[base::is.na(dat$AVAL0), "AVALT"]
  dat$AVAL <- dat$AVAL0 + dat$GROUPN
  dat$ord <- ord
  dat <- dat[ , c("ID", "TRTP", "GROUP", "GROUPN", "AVALT", "AVAL0", "AVAL", "TTEfixed", "ord", "seed")]

  base::class(dat) <- c("hce", "data.frame")
  return(dat)

}


