## Validate the WO and WINS calculations for KHCE against the published results from SAS
library(hce)

set.seed(1)

## WO for KHCE, hce - SAS
result <- calcWO(as_hce(KHCE))$WO
stopifnot(isTRUE(all.equal(result, 1.3199846571681)))

## WO for KHCE 2, hce - SAS
result <- calcWINS(as_hce(KHCE))$WO$WO
stopifnot(isTRUE(all.equal(result, 1.3199846571681)))

## WO for KHCE 3, hce - direct
result <- calcWINS(as_hce(KHCE))$WO$WO
stopifnot(isTRUE(all.equal(result, (319841 + 0.5 * 401) / (242258 + 0.5 * 401))))

## WR for KHCE, hce - direct
result <- calcWINS(as_hce(KHCE))$WR1$WR
stopifnot(isTRUE(all.equal(result, 319841 / 242258)))

## WP for KHCE, hce - SAS
result <- calcWO(as_hce(KHCE))$WP
stopifnot(isTRUE(all.equal(result, 0.5689626666667)))

## WP for KHCE 2, hce - SAS
result <- calcWINS(as_hce(KHCE))$WP$WP
stopifnot(isTRUE(all.equal(result, 0.5689626666667)))

## WP for KHCE 3, hce - direct
result <- calcWINS(as_hce(KHCE))$WP$WP
stopifnot(isTRUE(all.equal(result, (319841 + 0.5 * 401) / 562500)))

## WINS for KHCE, hce - direct
result <- as.numeric(calcWINS(as_hce(KHCE))$summary[c("WIN", "LOSS", "TIE")])
stopifnot(isTRUE(all.equal(result, c(319841, 242258, 401))))

## SE for WP for KHCE, hce - SAS
result <- calcWO(as_hce(KHCE))$SE_WP
stopifnot(isTRUE(all.equal(result, 0.0147431746263)))

## SE for WP for KHCE 2, hce - SAS
result <- calcWINS(as_hce(KHCE))$SE$WP_SE
stopifnot(isTRUE(all.equal(result, 0.0147431746263)))

## p-value for WP for KHCE, hce - SAS
result <- calcWO(as_hce(KHCE))$Pvalue
stopifnot(isTRUE(all.equal(result, 0.000002902527050)))

## p-value for WP for KHCE 2, hce - SAS
result <- calcWINS(as_hce(KHCE))$WP$Pvalue
stopifnot(isTRUE(all.equal(result, 0.000002902527050)))

## p-value for WP for KHCE, hce - hce
result <- calcWINS(as_hce(KHCE), SE_WP_Type = "unbiased")$WP$Pvalue
expected <- calcWINS(
  x = KHCE,
  AVAL = "AVAL",
  TRTP = "TRTP",
  ref = "P",
  SE_WP_Type = "unbiased"
)$WP$Pvalue
stopifnot(isTRUE(all.equal(result, expected)))

## p-value for WP for KHCE 2, hce - hce
result <- calcWINS(
  data = COVID19,
  GROUP ~ TRTP,
  ref = "Placebo",
  SE_WP_Type = "unbiased"
)$WP$Pvalue
expected <- calcWINS(
  x = COVID19,
  AVAL = "GROUP",
  TRTP = "TRTP",
  ref = "Placebo",
  SE_WP_Type = "unbiased"
)$WP$Pvalue
stopifnot(isTRUE(all.equal(result, expected)))

## gamma for WP for KHCE, SAS
result <- calcWINS(as_hce(KHCE))$gamma$gamma
stopifnot(isTRUE(all.equal(result, 0.1380237289161)))

## SE for gamma for WP for KHCE, SAS
result <- calcWINS(as_hce(KHCE))$SE$gamma_SE
stopifnot(isTRUE(all.equal(result, 0.0295069564073)))

## Order does not affect the WO regression
result <- regWO(
  x = KHCE,
  AVAL = "GROUP",
  TRTP = "TRTP",
  COVAR = "EGFRBL",
  ref = "P"
)
expected <- regWO(
  x = KHCE[sample(seq_len(nrow(KHCE)), replace = FALSE), ],
  AVAL = "GROUP",
  TRTP = "TRTP",
  COVAR = "EGFRBL",
  ref = "P"
)
stopifnot(isTRUE(all.equal(result, expected)))

## Order does not affect the WO stratified regression
result <- stratWO(
  x = KHCE,
  AVAL = "AVAL",
  COVAR = "EGFRBL",
  TRTP = "TRTP",
  STRATA = "STRATAN",
  ref = "P"
)
expected <- stratWO(
  x = KHCE[sample(seq_len(nrow(KHCE)), replace = FALSE), ],
  AVAL = "AVAL",
  COVAR = "EGFRBL",
  TRTP = "TRTP",
  STRATA = "STRATAN",
  ref = "P"
)
stopifnot(isTRUE(all.equal(result, expected)))

## Order does not affect the WO stratification
result <- stratWO(
  x = KHCE,
  AVAL = "AVAL",
  TRTP = "TRTP",
  STRATA = "STRATAN",
  ref = "P"
)
expected <- stratWO(
  x = KHCE[sample(seq_len(nrow(KHCE)), replace = FALSE), ],
  AVAL = "AVAL",
  TRTP = "TRTP",
  STRATA = "STRATAN",
  ref = "P"
)
stopifnot(isTRUE(all.equal(result, expected)))

## Order does not affect the IWP calculations
result <- IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)
expected <- IWP(
  data = KHCE[sample(seq_len(nrow(KHCE)), replace = FALSE), ],
  AVAL = "EGFRBL",
  TRTP = "TRTPN",
  ref = 2
)
stopifnot(isTRUE(all.equal(result, expected)))

## WP based on the IWP calculation matches the calcWO results
iwp_result <- IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)
result <- mean(iwp_result$EGFRBL_[iwp_result$TRTP == "A"])
expected <- calcWO(EGFRBL ~ TRTP, data = KHCE)[c("WP", "SE_WP")]$WP
stopifnot(isTRUE(all.equal(result, expected)))

## SE for the adjusted WP comparison to the sanon package
result <- regWO(
  AVAL ~ TRTP + EGFRBL,
  data = KHCE[sample(seq_len(nrow(KHCE)), replace = FALSE), ]
)$SE_beta * sqrt(nrow(KHCE) / (nrow(KHCE) - 1))
stopifnot(isTRUE(all.equal(result, 0.0147476911574)))

## Adjusted WP comparison to the sanon package
result <- regWO(AVAL ~ TRTP + EGFRBL, data = KHCE)$beta
stopifnot(isTRUE(all.equal(result, 0.569107747079)))

FREQ <- c(16, 5, 0, 1, 0, 4, 1, 5, 7, 2)
dat0 <- data.frame(AVAL = rep(5:1, 2), TRTP = rep(c("A", "P"), each = 5))
dat <- dat0[rep(seq_len(nrow(dat0)), FREQ), , drop = FALSE]

## Brunner-Konietschke variance comparison with a published value
result <- calcWINS(AVAL ~ TRTP, data = dat, SE_WP_Type = "unbiased")$SE$WP_SE^2
stopifnot(isTRUE(all.equal(result, 0.0041708562)))

## size is correct
result <- sizeWO(WO = 1.23, power = 0.9)$SampleSize
stopifnot(isTRUE(all.equal(result, 1318)))

## size is correct, k = 1/3
result <- sizeWO(WO = 1.5, power = 0.9, k = 1/3)$SampleSize
stopifnot(isTRUE(all.equal(result, 395)))