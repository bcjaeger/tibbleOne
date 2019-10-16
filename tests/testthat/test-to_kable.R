test_that("correct inputs work", {

  n <- 10

  df <- data.frame(
    a = rep(c("X","Y"), n / 2),
    b = rep(c("A", "B"), each = n / 2)
  ) %>%
    set_variable_abbrs(a = c(ABC = "123")) %>%
    set_variable_notes(a = "The abbreviation is irrelevant")

  tb1 <- tibble_one(data = df, formula = ~ a | b)

  kb1 <- to_kable(tb1, use_groups = TRUE)

  attr <- attributes(kb1)

  expect_equal(
    attr$kable_meta$colnames,
    c("", "Overall(N = 10)", "A(N = 5)", "B(N = 5)")
  )

  expect_equal(attr$kable_meta$ncol, 4)

  expect_equal(attr$header_above, 1)

  df <- data.frame(
    a = rep(c("X","Y"), n / 2),
    b = rep(c("A", "B"), each = n / 2),
    c = 1:n,
    d = n:1,
    e = factor(c(letters[1:(n/2)],letters[1:(n/2)]))
  ) %>%
    set_variable_groups(grp1 = c('c','d'))

  tb1 <- tibble_one(data = df, formula = ~ . | a*b, include_pval = TRUE)

  kb1 <- to_kable(tb1, format = 'latex')

  attr <- attributes(kb1)

  expect_equal(
    attr$kable_meta$contents,
    c(
      "  & Overall & X & Y & X & Y & P-value",
      "No. of observations & 10 & 3 & 2 & 2 & 3 & \\$  \\$",
      "E, % &  &  &  &  &  & \\$ 0.616 \\$",
      "\\\\hspace\\{1em\\}a & 20.0 & 33.3 & 0.00 & 0.00 & 33.3 & \\$  \\$",
      "\\\\hspace\\{1em\\}b & 20.0 & 0.00 & 50.0 & 50.0 & 0.00 & \\$  \\$",
      "\\\\hspace\\{1em\\}c & 20.0 & 33.3 & 0.00 & 0.00 & 33.3 & \\$  \\$",
      "\\\\hspace\\{1em\\}d & 20.0 & 0.00 & 50.0 & 50.0 & 0.00 & \\$  \\$",
      "\\\\hspace\\{1em\\}e & 20.0 & 33.3 & 0.00 & 0.00 & 33.3 & \\$  \\$",
      "C & 5.50 \\(3.03\\) & 3.00 \\(2.00\\) & 3.00 \\(1.41\\) & 8.00 \\(1.41\\) & 8.00 \\(2.00\\) & \\$ 0.028 \\$",
      "D & 5.50 \\(3.03\\) & 8.00 \\(2.00\\) & 8.00 \\(1.41\\) & 3.00 \\(1.41\\) & 3.00 \\(2.00\\) & \\$ 0.028 \\$"
    )
  )

  expect_equal(
    attr$kable_meta$colnames,
    c(" ", "Overall", "X", "Y", "X", "Y", "P-value")
  )

  expect_equal(attr$kable_meta$ncol, 7)

  kb1 <- to_kable(tb1, indent_groups = TRUE)

  attr <- attributes(kb1)

  expect_equal(attr$header_above, 3)

  expect_equal(
    attr$kable_meta$colnames,
    c(
      "",
      "Overall(N = 10)",
      "X(N = 3)",
      "Y(N = 2)",
      "X(N = 2)",
      "Y(N = 3)",
      "P-value"
    )
  )


})
