test_that("size is correct", {
  expect_equal(sizeWO(WO = 1.23, power = 0.9)$SampleSize, 1318)
})
test_that("size is correct, k = 1/3", {
  expect_equal(sizeWO(WO = 1.5, power = 0.9, k = 1/3)$SampleSize, 395)
})
