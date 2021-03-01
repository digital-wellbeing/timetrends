# Time trends in technology effects

Analysis code associated with "There is no evidence that associations between adolescents’ digital technology engagement and mental health problems have increased" (Vuorre, Orben, Przybylski)

Authors:

- Matti Vuorre (matti.vuorre@oii.ox.ac.uk)
- Amy Orben
- Andrew Przybylski

We study the timecourse of technology effects on adolescent wellbeing in three large representative datasets.

The preprint, ["There is no evidence that associations between adolescents’ digital technology engagement and mental health problems have increased"](https://psyarxiv.com/nv5qj) is on PsyArxiv, and the article is current in press.

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

The data analyses are organized into separate [R Markdown](https://rmarkdown.rstudio.com/) files. First, there are three files that do preprocessing of the raw datasets (extracting the relevant variables from the raw data). Then, the data is cleaned a bit further and visualized. The main analyses are in the modelling scripts.

The project is organized as a R [bookdown](https://bookdown.org/yihui/bookdown/) project, so you can reproduce all analyses by building the book (e.g. in RStudio click the "Build Book" button). (Be advised, though, that the Bayesian GLMMs take a very long time to run [many days]). The results are rendered to `docs/index.html` and can be viewed in a web browser.
