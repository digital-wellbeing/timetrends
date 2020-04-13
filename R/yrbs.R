# Functions specific to YRBS dataset

#' Read and clean YRBS data
#'
#' Create a clean data frame from the YRBS SPSS data file. All well-being variables are coded so that greater values indicate greater well-being (e.g. greater values of suicide variables mean e.g. fewer suicide attempts).
#'
#' @param path Location of the YRBS data file (character; "data-raw/yrbs/sadc_2017_national.sav")
#'
#' @return Cleaned YRBS data (tibble)
read_yrbs <- function(path = "data-raw/yrbs/sadc_2017_national.sav") {

  # Load data without dichotomized and supplemental items
  data <- read_spss(
    path,
    col_select = c(
      year, age, sex, grade, race7,
      q25:q29, q80, q81,
      )
  )

  # Convert SPSS labels to factor labels and remove other SPSS attributes
  data <- as_factor(data)

  # Choose appropriate years: 2007-2015
  data <- data %>% filter(year > 2006)

  # Recode Variables
  # Read about the variables at
  # https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2017/2017_yrbs_sadc_documentation.pdf

  # Sad/Lonely and Dichotomous suicide variables to 0 (yes) - 1 (no)
  data <- mutate_at(data, vars(q25, q26, q27), ~as.numeric(.) - 1)

  # During the past 12 months, how many times did you actually attempt suicide?
  # Reverse code so greater numbers are fewer attempts
  data <- mutate(data, q28 = 5 - as.numeric(q28))

  # If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
  # Code those who did not attempt suicide or did not need to see a doctor as 1, those who had to see doctor as 0
  data <- mutate(data, q29 = ifelse(q29 == "Yes", 0, 1))

  # Recode age as continuous with correct values
  # Note: 12 and 18 include younger/older individuals
  data <- mutate(data, age = as.numeric(age) + 11)

  # Rename race variable
  data <- rename(data, race = race7)

  # Convert tech variable to continuous
  data <- mutate_at(data, vars(q80, q81), ~as.numeric(.) - 1)

  # Drop cases where grade, age, or sex are unknown
  data <- drop_na(data, grade, age, sex)

  # Give meaningful names
  data <- rename(
    data,
    tv = q80,
    device = q81,
    sad_hopeless = q25,
    suicide_1 = q26,
    suicide_2 = q27,
    suicide_3 = q28,
    suicide_4 = q29
  )

  # This function returns the cleaned dataset
  return(data)
}
