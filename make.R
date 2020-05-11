# Make all analyses
# see https://docs.ropensci.org/drake/index.html

# Load packages and source functions
source("R/packages.R")
source("R/functions.R")
source("R/mtf.R")
source("R/us.R")
source("R/yrbs.R")

# drake plan specifies the analysis workflow
plan <- drake_plan(
  # Preprocess raw datasets
  mtf = target(
    read_mtf(file_in("data-raw/mtf/")),
    format = "fst_tbl"
  ),
  us = target(
    read_us(file_in("data-raw/us/")),
    format = "fst_tbl"
  ),
  yrbs = target(
    read_yrbs(file_in("data-raw/yrbs/sadc_2017_national.sav")),
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
# future::plan("multiprocess")

# Run analyses
make(plan, lock_cache = FALSE, lock_envir = FALSE)
