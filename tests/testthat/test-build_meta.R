df <- data.frame(
  A = c(1,2,3),
  B = factor(c("One", "", "Three"), levels = c("One", "", "Three")),
  C = factor(c("One", "Two", " "), levels = c("One", "Two", " "))
)

class(df$C) <- "TestClass"

test_that("build_meta throws an error for a class that is other than 'factor', 'integer', 'numeric'", {
  expect_error(tbl_one <- tibble_one(
    data = df,
    formula = ~ .,
    include_pval = TRUE
  ))
})

class(df$C) <- c("factor", "integer")

test_that("build_meta throws an error for a class that contains more than one of  'factor', 'integer', 'numeric'", {
  expect_error(tbl_one <- tibble_one(
    data = df,
    formula = ~ .,
    include_pval = TRUE
  ))
})
