library(testthat)
library(MATH4753TYLROMA2024)

test_that("myncurve works as expected", {
  result <- myncurve(5,2,5)
  expect_equal((result$area), 0.5)
})

test_that("myncurve works as expected", {
  result <- myncurve(5,2,5)
  expect_equal((result$sigma), 2)
})

test_that("myncurve works as expected", {
  result <- myncurve(5,2,5)
  expect_equal((result$mu), 5)
})
