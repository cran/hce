#' Simulate an `adhce` dataset with two correlated outcomes (illness - death model)
#' 
#' Simulate an `adhce` dataset with two correlated outcomes - death and hospitalization - from a heterogeneous population. The correlation between these outcomes arises from population heterogeneity. Models the risk of death following hospitalization as dependent on the timing 
#' of the hospitalization, reflecting strong dependence between the times to the first and second events (i.e., event clustering).
#'
#' @param n sample size in the active treatment group.
#' @param n0 sample size in the placebo treatment group.
#' @param TTE_A event rates in the active group for the time-to-event outcomes; a numeric vector of length two.
#' @param TTE_P event rates in the placebo group for the time-to-event outcomes; a numeric vector of length two.
#' @param shape shape parameter of the Weibull distribution for time-to-event outcomes in the active group. Default is 1 (exponential distribution).
#' @param shape0 shape parameter of the Weibull distribution for time-to-event outcomes in the placebo group. Default is 1 (exponential distribution).
#' @param fixedfy length of follow-up.
#' @param theta heterogeneity coefficient for the first event, modeled via a gamma distribution with mean 1; `theta` controls the variance. When `theta = 0`, there is no heterogeneity, which implies that death and hospitalization are independent.
#' @param alpha0 exponential heterogeneity coefficient for modeling the heterogeneity of risk of death as the first event.
#' @param alpha exponential heterogeneity coefficient for modeling the heterogeneity of risk of death after hospitalization; the heterogeneity of the second event is the inverse of the time of the first event.
#' @param rHR recurrence hazard ratio comparing the active group to the control group for the second event, based on gap time measured from the first event.
#' @param m categorization number used for to discretize time to death and time to hospitalization. Defaults to `Inf` (no discretization).
#' @param hce_type the type of the hierarchical composite endpoint: use `mi` for the most-important outcome or `md` for the move-down approach. This parameter only affects results when `m` is finite.
#' @details
#' The default setting assumes `TTE_A = TTE_P`. Both `TTE_A` and `TTE_P` must be numeric vectors of length two, corresponding to the event rates 
#' (Weibull distribution) for the first event of hospitalization and death. The parameters `shape` and `shape0` identify the shape parameters 
#' of Weibull distributions for the first event, simulated from a distribution with a cumulative hazard of 
#' \deqn{\gamma\cdot rate\cdot t^{shape}} 
#' for hospitalization and 
#' \deqn{\gamma^{\alpha_0}\cdot rate\cdot t^{shape}} 
#' for death, where \eqn{\gamma} (`gamma`) is a patient-specific frailty drawn from a gamma distribution with mean 1 and 
#' variance \eqn{\theta}, shared between death and hospitalization for a given patient. The parameter \eqn{\theta} (`theta`) represents population heterogeneity and also induces 
#' correlation between death and hospitalization as competing first events. The parameter \eqn{\alpha_0} (`alpha0`) controls the heterogeneity of time to death through its 
#' effect on heterogeneity. Death after hospitalization is simulated from an exponential distribution with a constant hazard that depends on the timing \eqn{t_1} 
#' of the first event (hospitalization) as 
#' \deqn{\frac{TTE_A[2] + TTE_P[2]}{2}\cdot \left(\frac{t_1}{fixedfy}\right)^{\alpha}\cdot \gamma^{\alpha_0}}
#' for the placebo arm and 
#' \deqn{\frac{TTE_A[2] + TTE_P[2]}{2}\cdot rHR \cdot \left(\frac{t_1}{fixedfy}\right)^{\alpha}\cdot \gamma^{\alpha_0}}
#' for the active arm where `rHR` is the recurrence hazard ratio. When \eqn{\alpha < 0}, earlier hospitalization (smaller \eqn{t_1}) 
#' increases the risk of death following hospitalization.
#' 
#' By default, events are simulated in continuous time. When `m` is specified as a positive numeric value, the event times are discretized into `m` intervals over the follow-up period.
#' @return an object of class `adhce`.
#' @export
#' @md
#' @seealso [hce::simHCE()] for a general `adhce` dataset simulation, and [hce::simKHCE()] for kidney disease-specific `adhce` simulation.
#' @examples
#' ## Example 1 - positive correlation
#' i <- 1764002323
#' set.seed(i)
#' PADY <- 2
#' D <- simTTE(n = 1000, TTE_A = c(0.1, 0.04), 
#' TTE_P = c(.15, 0.045), theta = 4, alpha0 = 2, alpha = -1, shape = 2, 
#' fixedfy = PADY, rHR = 1)
#' ####### Summary of first events by treatment group ########
#' table(D$EVENT1, D$TRTP)
#' ####### Summary of second events by treatment group ########
#' table(D$EVENT2, D$TRTP)
#' ######## Calculate win odds #########################
#' calcWO(D, ref = "P")
#' ## Plot the ordinal dominance graph ######
#' D$TRTP <- factor(D$TRTP, levels = c("P", "A"))
#' plot(D, type = "l", col = 2, fill = TRUE)
#' abline(a = 0, b = 1, lwd = 2, lty = 3, col = "darkgreen")
#' grid()
#' ################################################################
#' ## Example 2 - Move-down approach (discrete‑time case only)
#' PADY <- 2
#' # Continuous-time
#' set.seed(2)
#' D <- simTTE(n = 1000, TTE_A = c(0.1, 0.04), 
#' TTE_P = c(.15, 0.045), theta = 4, alpha0 = 2, alpha = -1, shape = 2, 
#' fixedfy = PADY, rHR = 1, m = Inf)
#' summaryWO(D, ref = "P")$summary_by_GROUP
#' # Discrete-time (more-ties)
#' D0 <- simTTE(n = 1000, TTE_A = c(0.1, 0.04), 
#' TTE_P = c(.15, 0.045), theta = 4, alpha0 = 2, alpha = -1, shape = 2, 
#' fixedfy = PADY, rHR = 1, m = 5)
#' summaryWO(D0, ref = "P")$summary_by_GROUP
#' # Discrete-time and move-down approach (less ties on death)
#' D1 <- simTTE(n = 1000, TTE_A = c(0.1, 0.04), 
#' TTE_P = c(.15, 0.045), theta = 4, alpha0 = 2, alpha = -1, shape = 2, 
#' fixedfy = PADY, rHR = 1, m = 5, hce_type = "md")
#' summaryWO(D1, ref = "P")$summary_by_GROUP
#' 
simTTE <- function (n, n0 = n, TTE_A, TTE_P = TTE_A, shape = 1, shape0 = shape,
                    fixedfy = 2, theta = 1, alpha0 = 1, alpha = 1, rHR = 1, m = Inf, hce_type = c("mi", "md"))
{ 
hce_type <- match.arg(hce_type)
# Normalize scalar inputs (in case vectors are passed)
n       <- n[1]
n0      <- n0[1]
fixedfy <- fixedfy[1]
theta   <- theta[1]
shape   <- shape[1]
shape0  <- shape0[1]
alpha0   <- alpha0[1]
alpha   <- alpha[1]
rHR <- rHR[1]
m <- m[1]
# Input validation with informative messages
stopifnot(
  `Heterogeneity theta must be non-negative.` = theta >= 0,
  `The recurrence hazard ratio must be non-negative.`               = rHR >= 0,
  `Shape parameters must be positive.`        = all(c(shape > 0, shape0 > 0)),
  `Sample sizes n and n0 must be positive integers.` =
    all(c(n > 0, n0 > 0, n %% 1 == 0, n0 %% 1 == 0)),
  `Follow-up duration fixedfy must be positive.` = fixedfy > 0,
  `TTE_A must contain exactly two elements.` = length(TTE_A) == 2,
  `TTE_P must contain exactly two elements.` = length(TTE_P) == 2,
  `TTE_A and TTE_P must be positive.` = all(c(TTE_A, TTE_P) > 0),
  `The categorization number m must be a positive numeric value.` = 
    is.numeric(m) && m > 0,
  `The recurrence hazard ratio must be a positive numeric value.` = 
    rHR > 0
)

# Parameterisation: convert incoming rate-like TTE_* to Weibull scale parameters
scale_A <- 1 / TTE_A  # Vector length 2: scale_A[1]=HOSP scale, scale_A[2]=DEATH scale for Arm A
scale_P <- 1 / TTE_P  # Same for Arm P

# Construct baseline dataset with treatment assignment
N <- n + n0
d <- data.frame(
  ID   = 1:N,
  TRTP = c(rep("A", n), rep("P", n0))
)

# Subject-level frailty term:
# - If theta = 0, frailty is degenerate at 1 (no heterogeneity).
# - If theta > 0, use gamma frailty with mean 1 and variance theta.
if (theta == 0) {
  Gm <- rep(1, N)
} else {
  Gm <- stats::rgamma(N, shape = 1 / theta, scale = theta)
}
gm  <- Gm[1:n]         # Frailty for Arm A
gm0 <- Gm[(n + 1):N]   # Frailty for Arm P

# Simulate first event: Hospitalisation (HOSP)
# Weibull inverse-CDF: T = ( -log(U) * scale / frailty )^(1/shape)
U  <- stats::runif(n)
U0 <- stats::runif(n0)
X  <- (-log(U)  * scale_A[1] / gm )^(1 / shape)   # Arm A HOSP time
X0 <- (-log(U0) * scale_P[1] / gm0)^(1 / shape0)  # Arm P HOSP time

# Simulate competing event: Death (DEATH)
# Frailty enters as gm^alpha0 for death
U  <- stats::runif(n)
U0 <- stats::runif(n0)
Y  <- (-log(U)  * scale_A[2] / gm^alpha0 )^(1 / shape)    # Arm A DEATH time
Y0 <- (-log(U0) * scale_P[2] / gm0^alpha0)^(1 / shape0)   # Arm P DEATH time

# Store raw event times
d$DEATH <- c(Y,  Y0)
d$HOSP  <- c(X,  X0)

# Determine first observed event within fixed follow-up (fixedfy)
d$EVENT1 <- ifelse(d$DEATH <= pmin(d$HOSP, fixedfy), "DEATH",
                   ifelse(d$HOSP <= fixedfy, "HOSP", "CEN"))

# AVAL1 is the observed time of EVENT1 (or censoring time if CEN)
d$AVAL1 <- ifelse(d$EVENT1 == "DEATH", d$DEATH,
                  ifelse(d$EVENT1 == "HOSP", d$HOSP, fixedfy))

# Keep frailty for second-event modelling
d$FRAILTY <- c(gm, gm0)

# Subset to subjects with first event = HOSP; they are at risk for post-hospitalization death
d0 <- d[d$EVENT1 == "HOSP", ]

if (nrow(d0) > 0) {
  # SECOND EVENT MODEL: Death after hospitalization using individual frailty
  # Individual-specific post-HOSP rate:
  #   rate_i = baseline_rate * (fixedfy / AVAL1_i)^alpha * FRAILTY_i
  # Convert rate to scale (scale = 1 / rate_i) for exponential parameterization.
  # Use a baseline rate similar to original behaviour: mean of the first-event rates (inverse of scales)
  baseline_rate <- mean(c(1 / scale_A[2], 1 / scale_P[2]))
  
  rate3 <- baseline_rate * (d0$AVAL1/fixedfy )^alpha * d0$FRAILTY^alpha0
  scale3 <- 1 /ifelse(d0$TRTP == "A", rHR*rate3, rate3)
  
  # Use exponential distribution (Weibull shape = 1) for time from HOSP to DEATH2
  shape_h <- 1
  DEATH2  <- (-log(stats::runif(nrow(d0))) * scale3)^(1 / shape_h)
  
  # Calendar time of second event
  d0$DEATH2 <- DEATH2 + d0$AVAL1
  
  # Second event indicators and observed time within fixed follow-up
  d0$EVENT2 <- ifelse(d0$DEATH2 <= fixedfy, "DEATH", "CEN")
  d0$AVAL2  <- pmin(d0$DEATH2, fixedfy)
} else {
  # No subjects had hospitalisation first; create empty second-event columns to merge back
  d0 <- d[, c("ID")]
  d0$EVENT2 <- character(0)
  d0$AVAL2  <- numeric(0)
}

# Merge second-event info back to the full cohort
d1 <- merge(d, d0[, c("ID", "EVENT2", "AVAL2")], by = "ID", all.x = TRUE)

# For subjects without second-event records, set as censored at fixedfy
d1[is.na(d1$EVENT2), "EVENT2"] <- "CEN"
d1[is.na(d1$AVAL2),  "AVAL2"]  <- fixedfy

# PADY: planned analysis day = fixed follow-up time
d1$PADY <- fixedfy

## Most-important outcome HCE (by default)
  # Overall endpoint group
  d1$GROUP <- ifelse(d1$EVENT1 == "DEATH" | d1$EVENT2 == "DEATH", "DEATH",
                     ifelse(d1$EVENT1 == "HOSP", "HOSP", "CEN"))
  
  # AVAL0: final observed time relevant for composite endpoint ranking
  d1$AVAL0 <- ifelse(d1$EVENT1 == "DEATH", d1$AVAL1,
                     ifelse(d1$EVENT2 == "DEATH", d1$AVAL2, d1$AVAL1))
  
  # Factorize GROUP to ensure consistent ordering
  d1$GROUP <- factor(d1$GROUP, levels = c("DEATH", "HOSP", "CEN"))


# Drop intermediate columns not needed in final output
d1$DEATH   <- NULL
d1$HOSP    <- NULL
d1$FRAILTY <- NULL
# If discrete time is specified
if(!is.infinite(m)){
  d1$AVAL1 <- fixedfy/m*floor(d1$AVAL1/(fixedfy/m))
  d1$AVAL2 <- fixedfy/m*floor(d1$AVAL2/(fixedfy/m))
  d1$AVAL0 <- ifelse(d1$EVENT1 == "DEATH", d1$AVAL1, ifelse(d1$EVENT2 == 
                                                              "DEATH", d1$AVAL2, d1$AVAL1))
} 
if(hce_type == "md"){
  d1$AVAL0[d1$EVENT2 == "DEATH"] <- d1$AVAL0[d1$EVENT2 == "DEATH"] + d1$AVAL1[d1$EVENT2 == "DEATH"]/(m + 2)
  d1$AVAL0[d1$EVENT1 == "DEATH"] <- d1$AVAL0[d1$EVENT1 == "DEATH"] + d1$PADY[d1$EVENT1 == "DEATH"]/(m + 2)
}
d1$Type <- hce_type
# Convert to an hce dataset
as_hce(d1)
}

