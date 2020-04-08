# Make all analyses

# Load packages and source functions
source("R/packages.R")
source("R/mtf.R")

# Parallel processing settings
parallelism <- "future"
future::plan("multiprocess")

# drake plan specifies the analysis workflow
plan <- drake_plan(
  # Prepare MTF data
  mtf = read_mtf(file_in("data-raw/mtf/")),
  mtf_aggregated = aggregate_mtf(mtf),
  # Main results output
  results = target(
    rmarkdown::render(
      input = knitr_in("results.Rmd"),
      output_file = file_out("output/results.html"),
      output_dir = "output"
    )
  )
)

# Run analyses
make(
  plan,
  parallelism = parallelism,
  jobs = 4
)
