library(knitr)
library(kableExtra)
library(tibbleOne)
library(tidyverse)

meta <- pbc_tbl1 %>%
  set_variable_labels(
    status = "Status at last contact",
    trt = "Treatment group",
    age = 'Age',
    sex = 'Sex at birth',
    ascites = 'Ascites',
    bili = 'Bilirubin levels',
    edema = 'Edema',
    albumin = 'Serum Albumin'
  ) %>%
  build_meta(add_perc_to_cats = TRUE)


tbl_one <- tibble_one(
  data = pbc_tbl1,
  meta_data = meta,
  formula = ~ . - bili | test,
  include_pval = TRUE
)
