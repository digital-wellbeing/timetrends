# Time trends in technology effects

Authors:

- Matti Vuorre (matti.vuorre@oii.ox.ac.uk)
- Amy Orben
- Augustin Mutak (augustin@psihometar.hr)
- Andrew Przybylski

We study the timecourse of technology effects on adolescent wellbeing in three large representative datasets.

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
	
- Youth Risk Behavior Survey
	- The combined National YRBS data 
	- https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
	- Download the National ASCII format data and the associated SPSS syntax to `data-raw/yrbs/`
	- Process the ASCII file in place with the SPSS syntax file, so you end up with a file `data-raw/yrbs/sadc_2017_national.sav`

## Analyses

Each dataset has its own rmarkdown file for preprocessing, which saves clean data in `data/`. Run those before the analyses (rmarkdown files in root).
