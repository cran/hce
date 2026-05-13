#' Simulate random numbers from a Weibull distribution with gamma frailty
#'
#' @param n number of observations.
#' @param rate rate parameter of the Weibull distribution.
#' @param shape shape parameter of the Weibull distribution, with a default value of 1 (exponential).
#' @param theta variance parameter of the gamma frailty distribution, with a default value of 1. 
#' @details
#' Let \eqn{\gamma} denote a frailty term following a gamma distribution with
#' \eqn{shape = 1 / \theta} and \eqn{scale = \theta}. Then \eqn{\gamma} has mean 1
#' and variance \eqn{\theta}.
#'
#' Conditional on \eqn{\gamma}, the survival function is
#' \deqn{
#' S(t \mid \lambda, \alpha, \gamma) =
#' \exp\left(-\gamma \lambda t^\alpha\right), \qquad \alpha > 0,\ \lambda > 0,
#' }
#' where \eqn{\alpha} is the shape parameter and \eqn{\lambda} is the rate parameter.
#'
#' Integrating over the gamma frailty distribution gives the marginal survival
#' function
#' \deqn{
#' S(t \mid \lambda, \alpha, \theta) =
#' \left(1 + \theta \lambda t^\alpha\right)^{-1 / \theta},
#' \qquad \alpha > 0,\ \lambda > 0,\ \theta > 0.
#' }
#' This corresponds to the generalized log-logistic distribution (GLL) as implemented in [hce::GLL()].
#' 
#' As \eqn{\theta \to 0}, this reduces to the standard Weibull distribution
#' without frailty, with survival function 
#' \deqn{
#' S(t \mid \lambda, \alpha) = \exp\left(-\lambda t^\alpha\right).
#' }
#' This corresponds to the Weibull distribution in [stats::pweibull()] with `shape = shape` and `scale = rate^(-1 / shape)`.
#' 
#' When \eqn{\theta = 1}, the survival function becomes
#' \deqn{
#' S(t \mid \lambda, \alpha, 1) = \left(1 + \lambda t^\alpha\right)^{-1},
#' }
#' which is the survival function of a log-logistic distribution.
#' @return a vector of random numbers of length `n`.
#' @export
#' @md
#' @references Wienke A. "Frailty Models in Survival Analysis." Chapman and Hall/CRC (2010).
#' @seealso [hce::simTTE()] for simulation of a two-event time-to-event endpoint
#'   from this distribution under the illness-death model.
#'
#'   [hce::pGLL()] provides the cumulative distribution function of the
#'   generalized log-logistic distribution.
#' @examples
#' ## Example 1: each patient has a sampled time with potentially different
#' ## shape, rate, and theta values.
#' set.seed(123)
#' n <- 1000
#' d <- data.frame(ID = 1:(2*n), TRTP = rep(c("A", "P"), each = n), 
#' rate = rep(c(0.1, 0.15), each = n), shape = 1.5, theta = 1)
#' d$time1 <- rweibullGF(n = 2*n, rate = d$rate, shape = d$shape, theta = d$theta)
#' tapply(d$time1, d$TRTP, summary)
#' ## Example 2: all patients share the same parameter values.
#' d$time2 <- rweibullGF(n = 2*n, rate = 0.1, theta = 0)
#' tapply(d$time2, d$TRTP, summary)
rweibullGF <- function(n, rate, shape = 1, theta = 1) {
  n <- n[1]
  stopifnot("`n` must be a positive integer." = is.numeric(n) && n > 0,
            "`shape` must be a positive vector of length 1 or `n`." = is.numeric(shape) && all(shape > 0) && length(shape) %in% c(1, n),
            "`rate` must be a positive vector of length 1 or `n`." = is.numeric(rate) && all(rate > 0) && length(rate) %in% c(1, n),
            "`theta` must be a non-negative vector of length 1 or `n`." = is.numeric(theta) && all(theta >= 0) && length(theta) %in% c(1, n))
  n <- as.integer(n)
  #### Explicitly recycle scalar inputs to length n.
  shape <- rep_len(shape, n)
  rate  <- rep_len(rate, n)
  theta <- rep_len(theta, n)
  #################
  U <- stats::runif(n)                                      
  G <- rep(1, n)
  idx <- theta > 0
  ## Weibull proportional hazards parameterisation with gamma frailty:
  ## S(t | G) = exp(-G * rate * t^shape),
  ## where G is a gamma frailty term with mean 1 and variance theta.
  ## If theta = 0, then G = 1 and the model reduces to the standard Weibull PH model.
  ## In stats::rweibull(), the equivalent scale parameter is:
  ## scale = rate^(-1 / shape)
  G[idx] <- stats::rgamma(sum(idx), shape = 1/theta[idx], scale = theta[idx])   
  (- log(U) / (G * rate))^(1/shape)                 
}