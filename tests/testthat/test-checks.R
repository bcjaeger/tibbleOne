
library(tibbleOne, warn.conflicts = F)

df <- data.frame(
  A = c(1,2,3),
  B = factor(c("One", "", "Three"), levels = c("One", "", "Three")),
  C = factor(c("One", "Two", " "), levels = c("One", "Two", " "))
)

tibble_one(df, formula =  ~ B | C)

test_that("check_blanks throws an error for blank factors", {
  expect_error(check_blanks(df))
})

df <- data.frame(
  X = c(1,2,3),
  Y = factor(c("One", "Two", "Three"))
)

test_that("check_blanks doesnt throws an error for normal factors", {
  expect_null(check_blanks(df))
})
