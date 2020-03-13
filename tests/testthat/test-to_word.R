test_that("correct inputs work", {

  n <- 10

  df <- data.frame(a = rep(c("X", "Y"), n / 2),
                   b = rep(c("A", "B"), each = n / 2)) %>%
    set_variable_abbrs(a = c(ABC = "123")) %>%
    set_variable_notes(a = "The abbreviation is irrelevant")

  tb1 <- tibble_one(data = df, formula = ~ a | b)

  ms1 <- to_word(tb1)

  expect_equal(ms1$col_keys,
               c("Characteristic", "Overall", "A", "B"))

  # expect_equal(
  #   ms1$body$dataset,
  #   structure(
  #     list(
  #       Characteristic = "A, %",
  #       Overall = "50.0",
  #       A = "40.0",
  #       B = "60.0"
  #     ),
  #     class = "data.frame",
  #     row.names = c(NA, -1L)
  #   )
  # )

  df <- data.frame(
    a = rep(c("X","Y"), n / 2),
    b = rep(c("A", "B"), each = n / 2),
    c = 1:n,
    d = n:1,
    e = factor(c(letters[1:(n/2)],letters[1:(n/2)]))
  ) %>%
    set_variable_groups(grp1 = c('c','d'))

  tb1 <- tibble_one(data = df, formula = ~ . | a*b, include_pval = TRUE)

  ms1 <- to_word(tb1)

  expect_equal(
    ms1$body$dataset,
    structure(
      list(
        group = c(NA, NA, NA, NA, NA, NA, "grp1", NA, NA),
        Characteristic = c("E, %", "a", "b", "c", "d", "e", NA, "C", "D"),
        Overall = c(
          "",
          "20.0",
          "20.0",
          "20.0",
          "20.0",
          "20.0",
          NA,
          "5.50 (3.03)",
          "5.50 (3.03)"
        ),
        X_._A = c(
          "",
          "33.3",
          "0.00",
          "33.3",
          "0.00",
          "33.3",
          NA,
          "3.00 (2.00)",
          "8.00 (2.00)"
        ),
        Y_._A = c("",
                  "0.00",
                  "50.0",
                  "0.00",
                  "50.0",
                  "0.00",
                  NA,
                  "3.00 (1.41)",
                  "8.00 (1.41)"),
        X_._B = c("",
                  "0.00",
                  "50.0",
                  "0.00",
                  "50.0",
                  "0.00",
                  NA,
                  "8.00 (1.41)",
                  "3.00 (1.41)"),
        Y_._B = c("",
                  "33.3",
                  "0.00",
                  "33.3",
                  "0.00",
                  "33.3",
                  NA,
                  "8.00 (2.00)",
                  "3.00 (2.00)"),
        `P-value` = c(
          "0.616",
          "", "", "", "", "", NA, "0.028", "0.028")
      ),
      groups = "group",
      columns = c(
        "Characteristic",
        "Overall",
        "X_._A",
        "Y_._A",
        "X_._B",
        "Y_._B",
        "P-value"
        ),
      row.names = c(
        3L,
        4L,
        5L,
        6L,
        7L,
        8L,
        2L,
        9L,
        10L),
      class = c("grouped_data", "data.frame"))
  )

})
