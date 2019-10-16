
test_that("correct inputs", {

  vec <- c(0.1234, 0.00001, 1.0)

  expect_equal(edit_pval(vec), c('0.123', "< 0.001", "> 0.999"))

})


test_that("non-numeric input gets error", {

  vec <- c("0.1234", "1.234", "12.34", "1234")

  expect_error(edit_pval(vec), regexp = "should be numeric")

})

test_that("all NA input is handled correctly", {

  vec <- c(NA, NA)

  expect_equal(edit_pval(vec), c("NA","NA"))

})


test_that("some NA input is handled correctly", {

  vec <- c(NA, NA, .1234, 0.00001, 1.0)

  expect_equal(edit_pval(vec), c("NA","NA", '0.123', "< 0.001", "> 0.999"))

})
