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


