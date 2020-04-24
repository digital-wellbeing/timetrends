# Make all analyses

# Load packages and source functions
source("R/packages.R")
source("R/functions.R")
source("R/mtf.R")
source("R/us.R")
source("R/yrbs.R")

# drake plan specifies the analysis workflow
plan <- drake_plan(
  # Preprocess raw datasets
  mtf = target(read_mtf(file_in("data-raw/mtf/")), format = "fst_tbl"),
  us = target(read_us(file_in("data-raw/us/")), format = "fst_tbl"),
  yrbs = target(read_yrbs(file_in("data-raw/yrbs/sadc_2017_national.sav")), format = "fst_tbl"),
  # Create a harmonised long data frame of all studies for modelling
  # Note: Ys are duplicated across Xs
  data_long = target(
    bind_rows(mtf, us, yrbs, .id = "study") %>%
      mutate(study = factor(study, labels = c("MTF", "US", "YRBS"))) %>%
      mutate(age = cut(age, breaks = c(0, 14, 100), labels = c("<15", ">15"))) %>%
      select(study, year, sex, age, starts_with("y_"), starts_with("x_")) %>%
      pivot_longer(starts_with("y_"), names_to = "outcome", values_to = "y") %>%
      pivot_longer(starts_with("x_"), names_to = "predictor", values_to = "x") %>%
      drop_na(y, x) %>%
      mutate(outcome = str_remove(outcome, "y_"), predictor = str_remove(predictor, "x_")) %>%
      group_by(study, outcome, predictor),
    format = "fst_tbl"
  ),
  # Create output documents
  dashboard = target(
    rmarkdown::render(
      input = knitr_in("dashboard.Rmd"),
      output_file = file_out("output/dashboard.html"),
      output_dir = "output"
    ), quiet = TRUE
  )
)

# Parallel processing settings
# options(clustermq.scheduler = "multicore")
future::plan("multiprocess")

# Run analyses
make(
  plan = plan,
  parallelism = "future",
  lock_envir = FALSE, lock_cache = FALSE,
  jobs = 4
)
