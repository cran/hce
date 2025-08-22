test_that("WO for KHCE, hce - SAS", {
  expect_equal(calcWO(as_hce(KHCE))$WO, 1.3199846571681)
})
test_that("WO for KHCE 2, hce - SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$WO$WO, 1.3199846571681)
})

test_that("WO for KHCE 3, hce - direct", {
  expect_equal(calcWINS(as_hce(KHCE))$WO$WO, (319841 + 0.5*401)/(242258 + 0.5*401))
})


test_that("WR for KHCE, hce - direct", {
  expect_equal(calcWINS(as_hce(KHCE))$WR1$WR, (319841 )/(242258 ))
})

test_that("WP for KHCE, hce - SAS", {
  expect_equal(calcWO(as_hce(KHCE))$WP, 0.5689626666667)
})

test_that("WP for KHCE 2, hce - SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$WP$WP, 0.5689626666667)
})

test_that("WO for KHCE 3, hce - direct", {
  expect_equal(calcWINS(as_hce(KHCE))$WP$WP, (319841 + 0.5*401)/(562500))
})

test_that("WINS for KHCE, hce - direct", {
  expect_equal(as.numeric(calcWINS(as_hce(KHCE))$summary[c("WIN", "LOSS", "TIE")]), 
               c(319841, 242258, 401))
})

test_that("SE for WP for KHCE, hce - SAS", {
  expect_equal(calcWO(as_hce(KHCE))$SE_WP, 0.0147431746263)
})

test_that("SE for WP for KHCE 2, hce - SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$SE$WP_SE, 0.0147431746263)
})

test_that("p-value for WP for KHCE, hce - SAS", {
  expect_equal(calcWO(as_hce(KHCE))$Pvalue, 0.000002902527050)
})

test_that("p-value for WP for KHCE 2, hce - SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$WP$Pvalue, 0.000002902527050)
})

test_that("p-value for WP for KHCE, hce - hce", {
  expect_equal(calcWINS(as_hce(KHCE), SE_WP_Type = "unbiased")$WP$Pvalue, calcWINS(x = KHCE, AVAL = "AVAL", TRTP = "TRTP", ref = "P", SE_WP_Type = "unbiased")$WP$Pvalue)
})


test_that("p-value for WP for KHCE 2, hce - hce", {
  expect_equal(calcWINS(data = COVID19, GROUP ~ TRTP, ref = "Placebo", SE_WP_Type = "unbiased")$WP$Pvalue, 
               calcWINS(x = COVID19, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo", SE_WP_Type = "unbiased")$WP$Pvalue)
})

test_that("gamma for WP for KHCE, SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$gamma$gamma, 0.1380237289161)
})

test_that("SE for gamma for WP for KHCE, SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$SE$gamma_SE, 0.0295069564073)
})

test_that("Order does not affect the WO regression", {
  expect_equal(regWO(x = KHCE, AVAL = "GROUP", TRTP = "TRTP", COVAR = "EGFRBL", ref = "P"), 
               regWO(x = KHCE[sample(1:nrow(KHCE), replace = FALSE), ], AVAL = "GROUP", TRTP = "TRTP", COVAR = "EGFRBL", ref = "P"))
})

test_that("Order does not affect the WO stratified regression", {
  expect_equal(stratWO(x = KHCE, AVAL = "AVAL", COVAR = "EGFRBL", 
                       TRTP = "TRTP", STRATA = "STRATAN", ref = "P"), 
               stratWO(x = KHCE[sample(1:nrow(KHCE), replace = FALSE),], AVAL = "AVAL", COVAR = "EGFRBL", 
                       TRTP = "TRTP", STRATA = "STRATAN", ref = "P"))
})


test_that("Order does not affect the WO stratification", {
  expect_equal(stratWO(x = KHCE, AVAL = "AVAL",  
                       TRTP = "TRTP", STRATA = "STRATAN", ref = "P"), 
               stratWO(x = KHCE[sample(1:nrow(KHCE), replace = FALSE),], AVAL = "AVAL", 
                       TRTP = "TRTP", STRATA = "STRATAN", ref = "P"))
})

test_that("Order does not affect the IWP calculations", {
  expect_equal(
    IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2),
    IWP(data = KHCE[sample(1:nrow(KHCE), replace = FALSE),], 
                AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)
  )
})

test_that("WP based on the IWP calculation matches the calcWO results", {
  expect_equal(
    mean(IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)$EGFRBL_[IWP(data = KHCE, AVAL = "EGFRBL", TRTP = "TRTPN", ref = 2)$TRTP=="A"]),
    calcWO(EGFRBL ~ TRTP, data = KHCE)[c("WP", "SE_WP")]$WP
  )
})



test_that("SE for the adjusted WP comparison to the sanon package", {
  expect_equal(
    regWO(AVAL ~ TRTP + EGFRBL, data = KHCE[sample(1:nrow(KHCE), replace = FALSE),])$SE_beta*sqrt(nrow(KHCE)/(nrow(KHCE) - 1)),
    0.0147476911574
  )
})


test_that("Adjusted WP comparison to the sanon package", {
  expect_equal(
    regWO(AVAL ~ TRTP + EGFRBL, data = KHCE)$beta,
    0.569107747079
  )
})

FREQ <- c(16, 5, 0, 1, 0, 4, 1, 5, 7, 2)
dat0 <- data.frame(AVAL = rep(5:1, 2), TRTP = rep(c('A', 'P'), each = 5))
dat <- dat0[rep(row.names(dat0), FREQ),]

test_that("Brunner-Konietschke variance comparison with a published value", {
  expect_equal(
    calcWINS(AVAL ~ TRTP, data = dat,  SE_WP_Type = "unbiased")$SE$WP_SE^2,
    0.0041708562
  )
})