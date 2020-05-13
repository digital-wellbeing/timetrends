# Functions specific to YRBS dataset

#' Read and clean YRBS data
#'
#' Create a clean data frame from the YRBS SPSS data file.
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

  # Sad/Lonely and Dichotomous suicide variables to numeric (0 = no, 1 = yes)
  data <- mutate_at(data, vars(q25, q26, q27), ~1 - (as.numeric(.) - 1))

  # During the past 12 months, how many times did you actually attempt suicide?
  data <- mutate(data, q28 = as.numeric(q28) - 1)

  # If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
  data <- mutate(
    data,
    q29 = factor(q29, levels = c("Did not attempt suicide", "No", "Yes")),
    q29 = as.numeric(q29)-1
    )

  # Recode age as continuous with correct values
  # Note: 12 and 18 include younger/older individuals
  data <- mutate(data, age = as.numeric(age) + 11)

  # Rename race variable
  data <- rename(data, race = race7)

  # Convert tech variable to numeric
  data <- mutate_at(data, vars(q80, q81), ~as.numeric(.) - 1)

  # Drop cases where grade, age, or sex are unknown
  data <- drop_na(data, grade, age, sex)

  # Rename and aggregate
  data <- data %>%
    rename(
      tv = q80,
      device = q81,
      sad_lonely = q25,
      suicide_1 = q26,
      suicide_2 = q27,
      suicide_3 = q28,
      suicide_4 = q29,
    ) %>%
    mutate(
      suicide = rowMeans(select_at(., vars(starts_with("suicide_"))), na.rm = TRUE)
    )

  # Focus on individuals 15 years old or younger
  data <- filter(data, age <= 15)
  data <- data %>% select(-grade)

  # Save data to disk
  saveRDS(data, "data/yrbs.rds")

  # This function returns the cleaned dataset
  return(data)
}
