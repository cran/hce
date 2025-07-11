#' Simulate `hce` object with given event rates of time-to-event outcomes (Weibull), mean and SD of the continuous outcome (normal or log-normal) by treatment group
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
#' @param theta Gumbel dependence coefficient of the Weibull distributions for time-to-event outcomes. Default is `theta = 1` which assumes independence of time-to-event outcomes. Must be above or equal to 1.
#' @param logC logical, whether to use log-normal distribution for the continuous outcome.
#' @param seed for generating random numbers.
#' @param dec decimal places for the continuous outcome used for rounding. The default is `dec = 2`.
#' @param all_data logical, whether to return source datasets `ADET` (an event-time dataset for all time-to-event outcomes per patient) and `BDS` (a basic data structure for the continuous outcome for all patients).
#' @return an object of class `hce` containing the following columns:
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
#' 
#' If `all_data = TRUE`, the function returns a list containing the `hce` dataset, along with its source datasets: `ADET` (an event-time dataset for all time-to-event outcomes per patient) and `BDS` (a basic data structure for the continuous outcome for all patients).
#' @export
#' @md
#' @seealso [hce::hce()], [hce::as_hce()] for the helper a coerce function to `hce` objects.
#' @examples
#' # Example 1
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' dat <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'               CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3)
#' head(dat)
#' 
#'# Example 2
#' Rates_A <- 10
#' Rates_P <- 15
#' dat <- simHCE(n = 1000, n0 = 500, TTE_A = Rates_A, TTE_P = Rates_P, 
#'               CM_A = 0.1, CM_P = 0, seed = 5, shape = 0.2, logC = TRUE, dec = 0)
#' summaryWO(dat)
#' 
#' # Example 3: Comparison of dependent and independent outcomes
#' Rates_A <- c(10, 20)
#' Rates_P <- c(20, 20)
#' dat1 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P, 
#' CM_A = -3, CM_P = -6, CSD_A = 15, fixedfy = 3, theta = 1, seed = 1)
#' dat2 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P, 
#' CM_A = -3, CM_P = -6, CSD_A = 15, fixedfy = 3, theta = 1.0001, seed = 1)
#' calcWO(dat1)
#' calcWO(dat2)


simHCE <- function(n, n0 = n, TTE_A, TTE_P, CM_A, CM_P, CSD_A = 1, CSD_P = CSD_A, fixedfy = 1, yeardays = 360, pat = 100, shape = 1, theta = 1, logC = FALSE, seed = NULL, dec = 2, all_data = FALSE){
  if(base::length(TTE_P) != base::length(TTE_A))
    stop("Event rate vectors for active and placebo groups should have the same length.")
  stopifnot("`theta` must be greater than or equal to 1." = theta >= 1)
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
  all_data <- as.logical(all_data[1])
  shape <- shape[1]
  dec <- dec[1]
  theta <- theta[1]

  fixedf <- yeardays*fixedfy
  TTE_P <- TTE_P/yeardays/pat
  TTE_A <- TTE_A/yeardays/pat
  l <- base::length(TTE_A)
  if(is.null(seed)){
    seed <- base::as.numeric(base::Sys.time())
    seed <- base::round(seed)
  }
  seed <- seed[1]
  base::set.seed(seed)
  invW <- function(u, scale, shape) {
    scale * (- log( 1 - u))^(1/shape)
  }
  if(theta == 1){
    M_P <- base::sapply(1/TTE_P, stats::rweibull, n = n0, shape = shape)
    M_A <- base::sapply(1/TTE_A, stats::rweibull, n = n, shape = shape)
  } else if(theta > 1){
    Y <- rGumbel(n, theta = theta, dim = l)
    X <- rGumbel(n0, theta = theta, dim = l)
    
    d <- data.frame(AVAL = as.numeric(Y), RATE = rep(TTE_A, each = n), TTE = rep(1:l, each = n))
    d0 <- lapply(split(d, d$TTE), function(x) invW(x[, 1], scale = 1/x[, 2], shape = shape))
    M_A <- do.call(cbind, d0)
    dimnames(M_A) <- NULL
    d <- data.frame(AVAL = as.numeric(X), RATE = rep(TTE_P, each = n0), TTE = rep(1:l, each = n0))
    d0 <- lapply(split(d, d$TTE), function(x) invW(x[, 1], scale = 1/x[, 2], shape = shape))
    M_P <- do.call(cbind, d0)
    dimnames(M_P) <- NULL
  }
  M_P <- round(M_P)
  M_P <- base::as.data.frame(M_P)
  base::names(M_P) <- base::paste0("TTE", 1:l)
  M_P$TRTP <- "P"
  M_P$ID <- (n + 1):(n0 + n)
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
  ## Needed only for all_data = TRUE
  BDS <- dat[, c("ID", "TRTP", "AVAL0")]
  names(BDS)[names(BDS) == "AVAL0"] <- "AVAL"
  ###############
  dat[dat$GROUP != "C", "AVAL0"] <- dat[dat$GROUP != "C", "AVALT"]
  mcont <- min(dat[dat$GROUP == "C", "AVAL0"])
  dat$AVAL <- ifelse(dat$GROUP != "C", dat$AVAL0 + dat$GROUPN, dat$AVAL0 - mcont + 1 + dat$GROUPN)
  if(!all_data){
    dat <- dat[, c("ID", "TRTP", "GROUP", "GROUPN", "AVALT", "AVAL0", "AVAL", "seed")]
    dat$PADY <- fixedf
    dat <- as_hce(dat)
    return(dat)  
  } else{
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
}

phi <- function(t, theta = 1) {
  exp(- t^(1/theta))
}

#' Simulate from a Gumbel copula (version 1)
#'
#' @param n number of observations. 
#' @param dim dimension of the copula.
#' @param theta coefficient of dependence. 
#'
#' @return a random vector.
#' @noRd
rgumbel <- function(n, dim = 2, theta = 1){
  stopifnot("`theta` must be greater than or equal to 1." = theta >= 1)
  if(length(n) > 1)
    n <- length(n)
  if(length(dim) > 1)
    dim <- length(dim)
  exprand <- matrix(stats::rexp(dim * n), c(n, dim))
  unifpirand <- stats::runif(n, 0, pi)
  exprand2 <- stats::rexp(n)
  beta <- 1/theta
  stablerand <- sin((1 - beta) * unifpirand)^((1 - beta)/beta) * 
    (sin(beta * unifpirand))/(sin(unifpirand))^(1/beta)
  stablerand <- stablerand/(exprand2^(theta - 1))
  unifrand <- phi(exprand/stablerand, theta)
  return(unifrand)  
}

#' Simulate from a Gumbel copula (version 2)
#'
#' @param n number of observations. 
#' @param dim dimension of the copula.
#' @param theta coefficient of dependence. 
#'
#' @return a random vector.
#' @noRd
rGumbel <- function(n, dim = 2, theta = 1) {
  stopifnot("`theta` must be greater than or equal to 1." = theta >= 1)
  if(length(dim) > 1)
    dim <- length(dim)
  if(length(n) > 1)
    n <- length(n)
  rSTBL <- function(n, theta){
    Theta <- stats::runif(n, -pi/2, pi/2)
    W <- stats::rexp(n)
    beta <- 1
    alpha <- 1/theta
    gamma <- (cos(pi/(2*theta)))^theta
    delta <- ifelse(theta == 1, 1, 0)
    btan <- beta*tan(alpha*pi/2)
    theta0 <- atan(btan)/alpha
    Ctan <- (1 + btan^2)^(1/(2*alpha))
    alpha0 <- alpha*(theta0 + Theta)
    if(alpha != 1){
      Z <- sin(alpha0)*Ctan/cos(Theta)^(1/alpha)*(cos(alpha0 - Theta)/W)^((1 - alpha)/alpha)
    } else {
      pb <- pi/2 + beta*Theta
      Z <- 2/pi*(pb*tan(Theta) - beta*log(pi/2*W*cos(Theta)/pb))
    }
    Z * gamma + delta
  }
  exprand <- matrix(stats::rexp(dim * n), c(n, dim))
  stablerand <- rSTBL(n, theta = theta)
  unifrand <- phi(exprand/stablerand, theta)
  return(unifrand)
}
