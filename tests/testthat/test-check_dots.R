test_that("good arguments are accepted", {

  .dots <- c(a=1, b=2, c=3)

  .checked <- check_dots(.dots, valid_args = c("a", "b", "c"))

  expect_equal(.dots, .checked)


})

test_that("bad arguments are rejected", {

  .dots <- c(a=1, b=2, c=3)

  expect_error(
    check_dots(.dots, valid_args = c("a", "b")),
    regexp = "unrecognized"
  )

})
