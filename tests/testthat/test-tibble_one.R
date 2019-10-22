test_that("simple continuous input works", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  tb1 <- tibble_one(
    data = df,
    formula = ~ a+b,
    specs_table_vals = 'median'
  )

  expect_equal(
    tb1,
    structure(
      list(
        group = structure(c(1L, 1L, 1L), .Label = "None", class = "factor"),
        variable = structure(1:3, .Label = c("descr", "a", "b"), class = "factor"),
        labels = c("No. of observations", "A", "B, %"),
        Overall = c("10",
                    "5.50 [3.25-7.75]", "50.0")
      ),
      class = c("tbl_df", "tbl",
                "data.frame", "tibble_one"),
      row.names = c(NA, -3L),
      type = "single_decker",
      pvals = FALSE,
      abbrs = "",
      notes = structure(list(), .Names = character(0)),
      descr = "Table values are median [interquartile range] and percent for continuous and categorical variables, respectively.",
      allcats = FALSE
    )
  )

  tb1 <- tibble_one(
    data = df,
    formula = ~ a+b,
    specs_table_vals = 'median',
    include_freq = TRUE
  )

  expect_equal(
    tb1,
    structure(
      list(
        group = structure(c(1L, 1L, 1L), .Label = "None", class = "factor"),
        variable = structure(1:3, .Label = c("descr", "a", "b"), class = "factor"),
        labels = c("No. of observations", "A", "B, n (%)"),
        Overall = c("10",
          "5.50 [3.25-7.75]", "5 (50.0)")
      ),
      class = c("tbl_df", "tbl",
        "data.frame", "tibble_one"),
      row.names = c(NA, -3L),
      type = "single_decker",
      pvals = FALSE,
      abbrs = "",
      notes = structure(list(), .Names = character(0)),
      descr = "Table values are median [interquartile range] and count (percent) for continuous and categorical variables, respectively.",
      allcats = FALSE
    )
  )

  tb1 <- tibble_one(
    data = df,
    formula = ~ a | b,
    include_pval = TRUE,
    specs_table_vals = 'median',
    specs_table_tests = 'noparm'
  )

  expect_equal(
    tb1,
    structure(
      list(
        group = structure(c(1L, 1L), .Label = "None", class = "factor"),
        variable = structure(1:2, .Label = c("descr", "a"), class = "factor"),
        labels = c("No. of observations", "A"),
        Overall = c("10",
                    "5.50 [3.25-7.75]"),
        A = c("5", "3.00 [2.00-4.00]"),
        B = c("5",
              "8.00 [7.00-9.00]"),
        `P-value` = c("", "0.008")
      ),
      class = c("tbl_df",
                "tbl", "data.frame", "tibble_one"),
      row.names = c(NA, -2L),
      type = "double_decker",
      strat = list(
        n_groups = 2,
        n_by = 1L,
        by_table = NULL,
        label = "B"
      ),
      pvals = TRUE,
      abbrs = "",
      notes = structure(list(), .Names = character(0)),
      descr = "Table values are median [interquartile range] and percent for continuous and categorical variables, respectively.",
      allcats = FALSE
    )
  )

  expect_s3_class(object = tb1, class = "tbl_df")

})

test_that("simple categorical input works", {

  n <- 10

  df <- data.frame(
    a = rep(c("X","Y"), n / 2),
    b = rep(c("A", "B"), each = n / 2)
  )

  tb1 <- tibble_one(data = df, formula = ~ a | b)

  expect_equal(as.numeric(tb1$Overall[1]), n)
  expect_equal(as.numeric(tb1$A[1]), n/2)
  expect_equal(as.numeric(tb1$B[1]), n/2)

  expect_equal(nrow(tb1), 2)

  tb1 <- tibble_one(
    data = df,
    formula = ~ a | b,
    expand_binary_catgs = TRUE,
    include_pval = TRUE
  )

  expect_true(all(c("X","Y") %in% tb1$labels))
  expect_equal(nrow(tb1), 4)
  expect_equal(tb1$`P-value`[2], "> 0.999")

  expect_s3_class(object = tb1, class = "tbl_df")

})


test_that("formulas with + errors are identified", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  expect_error(
    tibble_one(data = df, formula = ~ a | b + c),
    regexp = "Only a star"
  )


})

test_that("formulas with interaction errors are identified", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  expect_error(
    tibble_one(data = df, formula = ~ a | b + c),
    regexp = "Only a star"
  )

  expect_error(
    tibble_one(data = df, formula = ~ a*c | b),
    regexp = "Interactions cannot"
  )


})


test_that("formulas with + errors are identified", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  expect_error(
    tibble_one(data = df, formula = ~ a | b + c),
    regexp = "Only a star"
  )


})

test_that("incorrect types are handled correctly", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))
  class(df$a) <- "bizarro"

  expect_error(
    tibble_one(
      data = df,
      formula = ~ a+b,
      specs_table_vals = 'median'
    ),
    regexp = 'tibble_one is compatible with'
  )

  df <- data.frame(a = 1:n, b = NA_real_)

  expect_error(
    tibble_one(
      data = df,
      formula = ~ a+b,
      specs_table_vals = 'median'
    ),
    regexp = 'All values of'
  )

})

test_that("insufficient inputs are identified", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  expect_error(
    tibble_one(data = df),
    regexp = "formula or row_vars"
  )


})

test_that("insufficient row variables are identified", {

  n <- 10

  df <- data.frame(a = 1:n, b = rep(c("A", "B"), each = n / 2))

  expect_error(
    tibble_one(data = df, formula = ~ .-a|b),
    regexp = "at least 1"
  )


})


