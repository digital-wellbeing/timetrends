# Make all analyses
# see https://docs.ropensci.org/drake/index.html

# Load packages and source functions
source("R/packages.R")
source("R/functions.R")
source("R/mtf.R")
source("R/us.R")
source("R/yrbs.R")
# Ensure data folder exists for processed data
dir.create("data", showWarnings = FALSE)
# Use this theme throughout
theme_set(
  theme_linedraw() +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "#2780e3", colour = "#2780e3")
    )
)

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
  )
)

# Parallel processing settings
future::plan("multiprocess")

# Run analyses
make(
  plan,
  parallelism = "future",
  jobs = 3,
  lock_cache = FALSE,
  lock_envir = FALSE
)
