test_that("size is correct", {
  expect_equal(sizeWO(WO = 1.23, power = 0.9)$result$SampleSize, 1318)
})
