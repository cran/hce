#' Simulate a kidney disease `hce` dataset
#'
#' Simulate a kidney disease `hce` dataset, capturing eGFR (Estimated Glomerular Filtration Rate) progression over time, along with 
#' a competing and dependent terminal event: KFRT (Kidney Failure Replacement Therapy)
#' 
#' @param n sample size in the active treatment group.
#' @param CM_A annualized eGFR slope in the active group.
#' @param CM_P annualized eGFR slope in the control group.
#' @param n0 sample size in the control treatment group.
#' @param TTE_A event rate per year in the active group for KFRT.
#' @param TTE_P event rate per year in the placebo group for KFRT. 
#' @param fixedfy length of follow-up in years.
#' @param Emin lower limit of eGFR at baseline.
#' @param Emax upper limit of eGFR at baseline.
#' @param sigma within-patient standard deviation.
#' @param Sigma between-patient standard deviation.
#' @param m number of equidistant visits.
#' @param theta coefficient of dependence of eGFR values and the risk of KFRT.
#' @param phi coefficient of proportionality (between 0 and 1) of the treatment effect. The case of 0 corresponds to the uniform treatment effect.
#' @details
#' The default setting is `TTE_A = TTE_P` because, conditional on eGFR level, 
#' the treatment effect does not influence the event rate of KFRT. In this model,
#' the effect of treatment on KFRT operates entirely through its impact on eGFR decline.
#' 
#' The parameters `TTE_A` and `theta` are chosen so that when GFR is 10, the event rate 
#' is 1 per year, and when GFR is 30, the event rate is 0.01 per year. These
#' parameter values are obtained by solving the equation `rate0*exp(GFR*theta) = rate` for `rate0`
#' and `theta`.
#' @return a list containing the dataset `GFR` for longitudinal measurements of 
#' eGFR and the competing KFRT events, the dataset `ADET` for the time-to-event 
#' kidney outcomes (sustained declines or sustained low levels of eGFR), 
#' and the combined `HCE` dataset for the kidney hierarchical composite endpoint.
#' @export
#' @md
#' @seealso [hce::simHCE()] for a general function of simulating `hce` datasets.
#' @examples
#' # Example 1
#' set.seed(2022)
#' L <- simKHCE(n = 1000, CM_A = -3.25)
#' dat <- L$HCE
#' calcWO(dat)
simKHCE <- function(n, CM_A, CM_P = - 4, n0 = n, TTE_A = 10, TTE_P = TTE_A,  
                   fixedfy = 2, Emin = 20, Emax = 100, 
                   sigma = 8, Sigma = 3,
                   m = 10, theta = -0.23, phi = 0){
  n <- n[1] 
  CM_A <- CM_A[1] 
  CM_P <- CM_P[1] 
  n0 <- n0[1]
  TTE_A <- TTE_A[1]
  TTE_P <- TTE_P[1]  
  fixedfy <- fixedfy[1] 
  Emin <- Emin[1]
  Emax <- Emax[1]
  sigma <- sigma[1]
  Sigma <- Sigma[1]
  m <- round(m)[1]
  theta <- theta[1]
  phi <- phi[1]
  stopifnot("The standard deviations `sigma` and `Sigma` should be positive." 
            = all(c(sigma > 0, Sigma > 0)), 
            "`phi` should be in the interval [0, 1]" = all(c(phi >= 0, phi <= 1)),
            "GFR range `Emin` and `Emax` should be non-negative" = all(c(Emin >= 0, Emax >= 0)))
  N <- n0 + n
  b0 <- CM_P
  b1 <- CM_A - CM_P
  c0 <- log(TTE_P)
  c1 <- log(TTE_A/TTE_P)
  c2 <- theta
  # Patient ID and treatment group
  d <- data.frame(ID = 1:N, TRTPN = c( rep(1, n),  rep(0, n0)))
  # Random patient-level slope with a given between-patient variability Sigma^2
  d$SLOPE <- b0 + stats::rnorm(N, mean = 0, sd = Sigma)
  # Implement proportionality for patients in the active group and negative (true) random slope
  d$SLOPE <- ifelse(d$TRTPN == 1 & d$SLOPE < 0, d$SLOPE*(1 - phi), d$SLOPE)
  # Implement additive effect (slope difference) for the patients in the active group
  d$SLOPE <-  d$SLOPE + b1*d$TRTPN 
  # Uniform random (true) baseline GFR from a given range. The observed baseline BASE will be different because of the sampling error (within-patient variability).
  d$BASE0 <- stats::runif(N, Emin, Emax)
  # Equidistant visit times. The number of visits is m + 1. Includes both the baseline visit and the visit at the end of the timeframe.
  tj <- (0:m)*fixedfy/m
  # Generate visits for all patients
  VISITS <- data.frame(ID = rep(1:N, each = m + 1), ADAY = rep(tj, times = N)) 
  # Merge to get the treatment groups
  d1 <- merge(VISITS, d, all.x = TRUE, by = "ID")
  # Random true slope is observed with a measurement error (within-patient variability is sigma^2)
  d1$AVAL <- d1$SLOPE*d1$ADAY + d1$BASE0 + stats::rnorm(nrow(d1), sd = sigma)
  # Extract time 0 measurements as baseline (observed baseline with sampling error)
  d2 <- d1[d1$ADAY == 0, c("ID", "AVAL")]
  names(d2) <- c("ID", "BASE")
  # Merge with the baseline variables (observed = BASE, true = BASE0)
  d3 <- merge(d1, d2, by = "ID", all.x = T)
  # Create patient-level KFRT incidence rates based on the true random slope
  ## Constant event-rates per group
  d3$RATE0 <- exp(c0 + c1*d3$TRTPN)
  ## Multiply the group-level incidence rates depending on the patient-level random (true) GFR slope at each visit interval.
  d3$RATE <- d3$RATE0*exp(c2*(d3$SLOPE*d3$ADAY + d3$BASE0))
  ## Simulate events using difference event rates. The rate argument is a vector hence each event will be simulated from different event rates.
  d3$AVALT0 <- stats::rexp(nrow(d3), rate = d3$RATE)
  d3$PADY <- fixedfy
  ## Check that each event of a given rate occurs in the given risk interval. If it is not, assign PADY + 1 to filter it out later when keeping only events.
  d3$AVALT0 <- ifelse(d3$AVALT0 <= d3$ADAY, d3$AVALT0 + d3$ADAY, d3$PADY + 1)
  ## Keep only events happening in the given risk interval and before end of follow-up.
  d4 <- d3[d3$AVALT0 <= d3$PADY, c("ID", "AVALT0")]
  ## If there are events generated
  if(nrow(d4) > 0){
    ## select first event of each patient
    l <- lapply(split(d4, d4$ID), function(x) x[1, ])
    d4 <- do.call(rbind, l)
    ## Keep the event times
    names(d4)[names(d4) == "AVALT0"] <- "AVALT"
    d5 <- merge(d3, d4, all.x = TRUE, by = "ID")  
    ## if event time is missing, then assign censoring.
    d5$EVENT <- NA
    d5$EVENT <- ifelse(is.na(d5$AVALT), 0, 1)
    d5[is.na(d5$AVALT), "AVALT"] <- fixedfy
  } else {
    d5 <- d3
    d5$EVENT <- 0
    d5$AVALT <- fixedfy
  }
  d5[, c("SLOPE", "BASE0", "RATE", "RATE0", "AVALT0")] <- NULL
  # Calculate change and percent changes from baseline
  d5$CHG <- d5$AVAL - d5$BASE
  d5$PCHG <- (d5$AVAL / d5$BASE - 1)*100
  # Remove observations after the generated event (the event is kidney failure hence precludes measurements of GFR)
  d5 <- d5[d5$ADAY <= d5$AVALT, ]
  # Sort the dataset
  ADLB <- d5[order(d5$ID, d5$ADAY), ]
  # Derive sustained decline based on the laboratory measurements 
  d <- ADLB
  d$E40 <- ifelse(d$PCHG <= -40, 1, 0)
  d$E50 <- ifelse(d$PCHG <= -50, 1, 0)
  d$E57 <- ifelse(d$PCHG <= -57, 1, 0)
  d$E15 <- ifelse(d$AVAL <= 15, 1, 0)
  sustained <- function(dat, x){
    thr0 <- as.numeric(substr(x, 2, 3))
    thr <- ifelse(x == "E15", thr0, -thr0)
    i <- which(dat[, x] == 1)[1]
    t0 <- dat[i, "ADAY"]
    # use one month for the confirmation of the sustained decline
    t1 <- t0 + 1/12
    # selects the analysis day immediately after the 1-month window
    j <- which(dat$ADAY > t1)[1]
    if(is.na(i) | is.na(j)) 
      return(NULL)
    # Requires that all measurements during the window satisfy the criteria
    if(x %in% c("E57", "E50", "E40") & all(dat[i:j, "PCHG"] <= thr)){
      dat0 <- dat[i, c("ID", "ADAY", x)]
      names(dat0) <- c("ID", "ADAY", "EVENT")
      dat0$EVENT <- x
      return(dat0)
    }
    if(x %in% c("E15") & all(dat[i:j, "AVAL"] <= thr)){
      dat0 <- dat[i, c("ID", "ADAY", x)]
      names(dat0) <- c("ID", "ADAY", "EVENT")
      dat0$EVENT <- x
      return(dat0)
    }
  }
  l <- lapply(split(d, d$ID), sustained, x = "E40")
  E1 <- do.call(rbind, l)
  l <- lapply(split(d, d$ID), sustained, x = "E50")
  E2 <- do.call(rbind, l)
  l <- lapply(split(d, d$ID), sustained, x = "E57")
  E3 <- do.call(rbind, l)
  l <- lapply(split(d, d$ID), sustained, x = "E15")
  E4 <- do.call(rbind, l)
  ADET0 <- rbind(E1, E2, E3, E4)
  if(!is.null(ADET0)){
    names(ADET0) <- c("ID", "AVAL0", "PARAMCD")  
  }
  E3 <- E3[!E3$ID %in% E4$ID, ]
  E2 <- E2[!E2$ID %in% c(E3$ID, E4$ID), ]
  E1 <- E1[!E1$ID %in% c(E2$ID, E3$ID, E4$ID), ]
  E <- rbind(E3, E2, E1, E4)
  if(!is.null(E)){
    names(E) <- c("ID", "AVAL0", "PARAMCD")  
  }
  
  PARAM <- data.frame(PARAMCD = c("KFRT", "E15", "E57", "E50", "E40", "GFR"), PARAMN = 1:6)
  d0 <- d[!d$ID %in% E$ID & d$ADAY == d$PADY, c("ID", "CHG")]
  
  if(nrow(d0) > 0){
    names(d0) <- c("ID", "AVAL0")
    d0$PARAMCD <- "GFR"  
  }
  
  d1 <- rbind(d0, E)
  d2 <- unique(d[d$EVENT == 1, c("ID", "AVALT")])
  
  if(nrow(d2) > 0){
    d2$PARAMCD <- "KFRT"
    names(d2) <- c("ID", "AVAL0", "PARAMCD")  
  }
  d3 <- rbind(d1[!d1$ID %in% d2$ID, ], d2)
  d4 <- merge(d3, PARAM, by = "PARAMCD", all.x = TRUE)
  d4 <- d4[, c("ID", "PARAMCD", "PARAMN", "AVAL0")]
  d4 <- d4[order(d4$ID), ]
  d5 <- unique(d[, c("ID", "TRTPN", "PADY", "BASE")])
  d6 <- merge(d5, d4, by = "ID")
  ADLB$TRTP <- ifelse(ADLB$TRTPN == 1, "A", "P")
  HCE <- d6
  HCE$TRTP <- ifelse(HCE$TRTPN == 1, "A", "P")
  HCE$GROUP <- factor(HCE$PARAMCD, levels = c("KFRT", "E15", "E40", "E50", "E57", "GFR"))
  levels(HCE$GROUP) <- c("Kidney Failure Replacement Therapy", "Sustained eGFR < 15 (mL/min/1.73 m2)", 
                         "Sustained >= 57% decline in eGFR", "Sustained >= 50% decline in eGFR", 
                         "Sustained >= 40% decline in eGFR", "eGFR slope")
  ADET <- rbind(ADET0, d2)
  ADET <- ADET[order(ADET$ID), ]
  L <- list(GFR = ADLB, ADET = ADET, HCE = as_hce(HCE))
  return(L)
}
