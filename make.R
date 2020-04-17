# Make all analyses

# Load packages and source functions
source("R/packages.R")
source("R/functions.R")
source("R/mtf.R")
source("R/us.R")
source("R/yrbs.R")

# drake plan specifies the analysis workflow
plan <- drake_plan(
  # Preprocess raw datasets and then combine to one data frame
  mtf = read_mtf(file_in("data-raw/mtf/")),
  us = read_us(file_in("data-raw/us/")),
  yrbs = read_yrbs(file_in("data-raw/yrbs/sadc_2017_national.sav")),
  dat = bind_rows(mtf, us, yrbs, .id = "study") %>%
    mutate(study = factor(study, labels = c("MTF", "US", "YRBS"))),
  # Fit all models
  # Fit y~x*year separately to every study, x, and y
  fits1 = make_fits(
    group_by(dat, study) %>% mutate(year = year - 2010),
    yvars = starts_with("y_"), xvars = starts_with("x_"), model = ~lm(y ~ x*year, data = .)
  ),
  # Fit y~x*year separately to every study, x, y, sex, and age
  fits2 = make_fits(
    group_by(dat, study, age, sex) %>% mutate(year = year - 2010),
    yvars = starts_with("y_"), xvars = starts_with("x_"), model = ~lm(y ~ x*year, data = .)
  ),
  # Main results output
  results = target(
    rmarkdown::render(
      input = knitr_in("dashboard.Rmd"),
      output_file = file_out("output/dashboard.html"),
      output_dir = "output"
    )
  )
)

# Parallel processing settings
# options(clustermq.scheduler = "multicore")
# future::plan("multiprocess")

# Run analyses
make(
  plan = plan,
  parallelism = "loop",  # or "future" or "clustermq"
  lock_envir = FALSE, lock_cache = FALSE,
  jobs = 1
)
