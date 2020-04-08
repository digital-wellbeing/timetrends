# Happiness epidemic

## Raw data

The datasets' Terms of Use prevent us for sharing the data files here, so you must download the raw data yourself. Get the data as follows so it is in the format expected by our code: 

- Monitoring the Future
	- Monitoring the Future (MTF) Public-Use Cross-Sectional Datasets
	- https://www.icpsr.umich.edu/icpsrweb/ICPSR/series/35
	- Download each year's data in Stata format to `data-raw/mtf/`
	- Unzip each file so that you end up with directories like `data-raw/mtf/ICPSR_xxxxx`
- Understanding society
	- Understanding Society: Waves 1-9, 2009-2018
	- https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614
	- Download the SPSS format data into `data-raw/us/`
	- Unzip the file so that you end up with `data-raw/us/UKDA-6614-spss/`
- Youth Risk Behavior Survey
	- The combined National YRBS data 
	- https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
	- Download the National ASCII format data and the associated SPSS syntax to `data-raw/yrbs/`
	- Process the ASCII file in place with the SPSS syntax file, so you end up with a file `data-raw/yrbs/sadc_2017_national.sav`

## Run analyses

Reproduce all analyses by running `make.R` (e.g. `Rscript make.R` in your command line.)

The dependencies between files, functions, and scripts are managed by [drake](https://docs.ropensci.org/drake/index.html), which ensures that all intermediate computations are up to date. If you want to run analyses interactively, source `R/packages.R` to make sure you are using the same packages as the other analyses.

### Parallel processing

By default we use the [future](https://cran.r-project.org/web/packages/future/index.html) package for parallel processing on a local computer. If you have the faster [zeroMQ](https://zeromq.org/download/) system library installed, you can use it instead by switching to these settings in `make.R`:

```
# future::plan("multiprocess")
# parallelism <- "future"
options(clustermq.scheduler = "multicore")
parallelism <- "clustermq"
```
