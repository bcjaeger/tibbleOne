
test_that("rounding works", {

  vec <- c(0.1234, 1.234, 12.34, 1234)

  expect_equal(adapt_round(vec), c('0.12', '1.23', '12.3', '1,234'))

})


test_that("non-numeric input gets error", {

  vec <- c("0.1234", "1.234", "12.34", "1234")

  expect_error(adapt_round(vec), regexp = "should be numeric")

})

test_that("all NA input is handled correctly", {

  vec <- c(NA, NA)

  expect_equal(adapt_round(vec), c("NA","NA"))

})


test_that("some NA input is handled correctly", {

  vec <- c(NA, NA, .1234)

  expect_equal(adapt_round(vec), c("NA","NA", "0.12"))

})
