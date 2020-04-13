# Attach packages required in project
# This script also facilitates running on a cluster by
# installing required packages if not already installed
options(repos = "http://cran.rstudio.com/")
packages <- c(
  "drake",
  "car",
  "rmarkdown",
  "flexdashboard",
  "scales",
  "janitor",
  "broom",
  "knitr",
  "glue",
  "tidybayes",
  "multcomp",
  "labelled",
  "haven",
  "tidyverse"
  )
packages_new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(packages_new)) install.packages(packages_new)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
rm(packages, packages_new)
