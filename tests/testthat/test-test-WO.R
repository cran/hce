test_that("WO for KHCE, SAS", {
  expect_equal(calcWO(as_hce(KHCE))$WO, 1.3199846571681)
})

test_that("WP for KHCE, SAS", {
  expect_equal(calcWO(as_hce(KHCE))$WP, 0.5689626666667)
})

test_that("SE for WP for KHCE, SAS", {
  expect_equal(calcWO(as_hce(KHCE))$SE_WP, 0.0147431746263)
})

test_that("p-value for WP for KHCE, SAS", {
  expect_equal(calcWO(as_hce(KHCE))$Pvalue, 0.000002902527050)
})

test_that("gamma for WP for KHCE, SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$gamma$gamma, 0.1380237289161)
})

test_that("SE for gamma for WP for KHCE, SAS", {
  expect_equal(calcWINS(as_hce(KHCE))$SE$gamma_SE, 0.0295069564073)
})


