# Time trends in technology effects

Authors:

- Matti Vuorre (matti.vuorre@oii.ox.ac.uk)
- Augustin Mutak (augustin@psihometar.hr)

## Preface

We study the timecourse of technology effects on adolescent wellbeing in three large representative datasets.

## Reproduce analyses

Open the `timetrends.Rproj` in R Studio (ensures correct relative paths), and run `renv::init()` (ensures you have the required R version and packages). If you don't have the [renv](https://rstudio.github.io/renv/articles/collaborating.html) package, install it first; `install.packages("renv")`.

Once you have the required packages, reproduce all analyses by running `make.R` (e.g. `Rscript make.R` in your command line.) To run analyses interactively, follow the steps specified in `make.R`.

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
	- http://doi.org/10.5255/UKDA-SN-6614-13
	- Download the SPSS format data into `data-raw/us/`
	- Unzip the file so that you end up with `data-raw/us/UKDA-6614-spss/`
	
The Understanding Society dataset is an annual longitudinal study of 40,000 UK households. The data collection takes place over a 24 month period, so the waves overlap. The study is run by the Institute for Social and Economic Research at the University of Essex; it began in 2009 and has been going ever since. It might be of interest to some that it is the successor to the British Household Panel Survey. We dont work with the whole dataset but with a special subsample of 10-15 year olds, which are a member of a household interviewed as part of Understanding Society. Adolescent members of the households are interviewed and re-interviewed every year till they graduate into the adult questionnaire. We also merge this adolescent data with data from young adults in the adult sample (16-21 year olds). This allows us to track adolescent development over a longer period of time. In total we load 9 waves of the data as released in April 2020.
	
- Youth Risk Behavior Survey
	- The combined National YRBS data 
	- https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
	- Download the National ASCII format data and the associated SPSS syntax to `data-raw/yrbs/`
	- Process the ASCII file in place with the SPSS syntax file, so you end up with a file `data-raw/yrbs/sadc_2017_national.sav`
