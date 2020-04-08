# Make all analyses

# Load packages and source functions
source("R/packages.R")
source("R/mtf.R")
source("R/yrbs.R")

# drake plan specifies the analysis workflow
plan <- drake_plan(
  # Prepare MTF data
  mtf = read_mtf(file_in("data-raw/mtf/")),
  mtf_aggregated = aggregate_mtf(mtf),
  # Prep YRBS data
  yrbs = read_yrbs(file_in("data-raw/yrbs/sadc_2017_national.sav")),
  # Main results output
  results = target(
    rmarkdown::render(
      input = knitr_in("results.Rmd"),
      output_file = file_out("output/results.html"),
      output_dir = "output"
    )
  )
)

# Parallel processing settings
# options(clustermq.scheduler = "multicore")
future::plan("multiprocess")

# Run analyses
make(
  plan = plan,
  parallelism = "future",  # or "future" or "clustermq"
  jobs = 4
)
