#' Simulate ordinal variables for two treatment groups using categorization of beta distributions
#'
#' @param n sample size in the active treatment group.
#' @param n0 sample size in the placebo group.
#' @param M number of ordinal values to be simulated.
#' @param alpha1 shape1 parameter for the beta distribution in the active group.
#' @param beta1 shape2 parameter for the beta distribution in the active group.
#' @param alpha0 shape1 parameter for the beta distribution in the placebo group.
#' @param beta0 shape2 parameter for the beta distribution in the placebo group.
#' @return a data frame containing the following columns:
#' * ID subject identifier.
#' * TRTP planned treatment group - "A" for active, "P" for Placebo.
#' * GROUPN ordinal values. The number of unique values is specified by the variable `M0`.
#' * tau the theoretical win odds.
#' * theta the theoretical win probability.
#' @export
#' @md
#' @seealso [hce::simHCE()], [hce::simADHCE()] for simulating `hce` objects.
#' @examples
#' # Example 1
#' set.seed(2024)
#' d <- simORD(n = 100, n0 = 50, M = 2)
#' d_hce <- hce(GROUP = d$GROUPN, TRTP = d$TRTP)
#' calcWO(d_hce)
#' ### compare with the theoretical values of the continuous distributions 
#' c(tau = unique(d$tau), theta = unique(d$theta))
#' # Example 2 - Convergence of the win odds to its theoretical value
#' set.seed(2024)
#' N <- NULL
#' size <- c(seq(10, 500, 1))
#' for(i in size){
#'   d <- simORD(n = i, M = 2)
#'   d_hce <- hce(GROUP = d$GROUPN, TRTP = d$TRTP)
#'   TAU <- calcWO(d_hce)
#'   D <- data.frame(WO = TAU$WO, n = i, tau = unique(d$tau))
#'   N <- rbind(N, D)
#' }
#' plot(N$n, N$WO, log = "y", ylim = c(0.5, 2), ylab = "Win Odds", xlab = "Sample size", type = "l")
#' lines(N$n, N$tau, col = "darkgreen", lty = 2, lwd = 2)
#' abline(h = 1, lty = 4, col = "red")
#' legend("bottomright", legend = c("Null", "Theoretical Win Odds", "Win Odds Estimate"), 
#' lty = c(4, 2, 1), col = c("darkgreen", "red", "black"))
#' title("Convergence of the win odds to its theoretical value")

simORD <- function(n, n0 = n, M, alpha1 = 8, beta1 = 7, alpha0 = 5, beta0 = 5){
  n <- n[1]
  n0 <- n0[1]
  M <- M[1]
  alpha0 <- alpha0[1]
  alpha1 <- alpha1[1]
  beta0 <- beta0[1]
  beta1 <- beta1[1]

  x <- seq(from = 0, to = 1, length.out = M + 1)
  tildeY <- 1 - (stats::pbeta(x[-1], shape1 = alpha1, shape2 = beta1) + stats::pbeta(x[-length(x)], shape1 = alpha1, shape2 = beta1))/2
  tildeX <- (stats::pbeta(x[-1], shape1 = alpha0, shape2 = beta0) - stats::pbeta(x[-length(x)], shape1 = alpha0, shape2 = beta0))
  theta <- sum(tildeX*tildeY)
  tau <- theta/(1 - theta)
  X0 <- stats::rbeta(n = n0, shape1 = alpha0, shape2 = beta0)
  Y0 <- stats::rbeta(n = n, shape1 = alpha1, shape2 = beta1)
  x0 <- seq(from = 0, to = 1, length.out = M + 1)
  X1 <- cut(X0, breaks = x0)
  levels(X1) <- 1:M
  X1 <- as.numeric(X1)
  Y1 <- cut(Y0, breaks = x0)
  levels(Y1) <- 1:M
  Y1 <- as.numeric(Y1)
  d <- data.frame(ID = 1:(n0 + n), GROUPN = c(Y1, X1), TRTP = c(rep("A", n), rep("P", n0)), tau = tau, theta = theta)
  return(d)
}
