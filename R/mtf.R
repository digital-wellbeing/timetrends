# Functions specific to MTF dataset

#' Read and clean Monitoring the Future data
#'
#' @param path Path to the directory of the downloaded and unzipped ICPSR_xxxxx directories (character; "data-raw/mtf/")
#'
#' @return A tibble with clean MTF data
read_mtf <- function(path = "data-raw/mtf/") {

  # Get list of individual data files' names
  MTFfiles <- list.files(
    path = path,
    pattern = "\\.dta$",
    recursive = TRUE,
    full.names = TRUE
  )

  # After this, we have to list all the variables we want to examine in our study. We should do this by using labels of variables which can be found in the codebooks. The reason we're doing this is because label naming of the MTF dataset remained more or less consistent throughout the years, while variable naming isn't consistent. Therefore, we can merge the data from different years by using labels, but not by using variable names themselves. For now, let's go with the following variables:
  vars <- c("YEAR OF", "S SEX", "s SEX", "WHAT GRADE LEVL", "VRY HPY THS DAY", "SATISFD W MYSELF", "LIFE MEANINGLESS", "I ENJOY LIFE", "HOPELESS", "GOOD TO BE ALIVE", "FUTR R LIFE WRSE", "AM PRSN OF WORTH", "DO WELL AS OTHRS", "MUCH TO B PROUD", "I AM NO GOOD", "I DO WRONG THING", "MY LIFE NT USEFL", "POS ATT TWD SELF", "OFTN FEEL LONELY", "ALWYS SM1 HELP", "OFTN FL LEFT OUT", "USLY SM1 TALK TO", "OFT WSH MOR FRND", "USLY FRDS BE WTH", "SAT LIFE AS WHL", "CMP SATFD", "PUTR SC", "PUTR OT", "PUTR JO", "DAY HRS WATCH", "END HRS WATCH", "W GAMING", "W INTERNET", "TV/DAY", "TV/WKEND", "WEB FACEBK", "HRS MUSIC")

  # Before proceeding with the merge, we have to create:
  # * An empty dataframe that will contain the merged data after the process is complete, which has the same number of columns as the number of our variables of interest and the columns are named accordingly;
  MTF <- data.frame(matrix(ncol = length(vars)))
  colnames(MTF) <- vars
  # * A list which will contain the data of interest from separate datafiles before it's merged;
  intermediate <- list()
  # * A list which will contain the labels of separate datafiles
  labels <- list()
  # * A vector which will contain column numbers of our variables of interest;
  indices <- vector("numeric")
  # * A vector which will contain column names of our variables of interest;
  names <- vector("character")
  # * A vector which will contain column numbers of columns that are unlabelled and should be removed.

  # Now we can go ahead with the merge. Please see inline comments for detailed descriptions!
  for (i in 1:length(MTFfiles)) {
    # Importing the file and adding it to the intermediate list
    intermediate[[i]] <- read_dta(MTFfiles[i])
    # Checking if the Grade variable is present in the file; if not, it's 12th grade
    if (any(grepl("WHAT GRADE LEVL", unlist(var_label(intermediate[[i]])))) == F) {
      intermediate[[i]][, (ncol(intermediate[[i]]) + 1)] <- 12
      var_label(intermediate[[i]][ncol(intermediate[[i]])]) <- "WHAT GRADE LEVL"
    }
    # Extracting labels while making sure that unlabelled columns are not dropped from the indexing
    labels[[i]] <- var_label(intermediate[[i]])
    labels[[i]][sapply(labels[[i]], is.null)] <- NA
    labels[[i]] <- unlist(labels[[i]])
    for (j in 1:length(vars)) {
      # Extracting the column numbers of variables of interest
      indices <- c(indices, grep(vars[j], labels[[i]]))
      # Extracting the column names of variables of interest
      names <- c(names, regmatches(labels[[i]], regexpr(vars[j], labels[[i]])))
    }
    # Removing all variables from the file except those we have chosen
    intermediate[[i]] <- intermediate[[i]][, indices]
    # Assigning labels as variable names so data can be merged
    colnames(intermediate[[i]]) <- names
    # Cleaning the vectors
    indices <- vector("numeric")
    names <- vector("character")
  }

  # Merging the data in the main dataframe
  MTF <- bind_rows(intermediate)

  # After merging the files, there are some leftover things we still have to do
  #   * Variables with the labels 'SAT LIFE AS WHL' and 'CMP SATFD' are actually the same variable and they should be merged;
  MTF$`SAT LIFE AS WHL` <- rowSums(MTF[, c("SAT LIFE AS WHL", "CMP SATFD")], na.rm = T)
  MTF <- subset(MTF, select = -`CMP SATFD`)
  vars <- vars[-26]

  # * The above also applies to variables with the labels 'S SEX' and 's SEX';
  MTF$`S SEX` <- rowSums(MTF[, c("S SEX", "s SEX")], na.rm = T)
  MTF <- subset(MTF, select = -`s SEX`)
  vars <- vars[-3]

  # * Numerical values which indicate various types of missing data have to be removed (i.e., coded as NA);
  MTF[MTF == 0] <- NA
  MTF[MTF == 8] <- NA
  MTF[MTF == 9] <- NA
  MTF[MTF == -9] <- NA
  MTF[MTF == -8] <- NA

  # * Cases which contain data only for 3 variables (year of administration, gender and grade) have to be removed;
  MTF <- MTF[!rowSums(is.na(MTF[, c(4:length(vars))])) == (length(vars) - 3), ]

  # * Cases which have unidentified labels for the Sex variable (3, 4, 5, 6 and 7) will be dropped;
  MTF <- subset(MTF, `S SEX` == 1 | `S SEX` == 2)

  # * Cases which have unidentified labels for the Grade variable (3, 5, 6 and 7) will be dropped;
  MTF <- subset(MTF, `WHAT GRADE LEVL` == 2 | `WHAT GRADE LEVL` == 4 | `WHAT GRADE LEVL` == 12)

  # * Cases which have missing values in the Year variable should be dropped;
  MTF <- drop_na(MTF, `YEAR OF`)

  # * Initial row numbers should be dropped -- convert to tibble takes care of this
  MTF <- as_tibble(MTF)

  # * Values of years which were coded using only last two digits (e.g. 92 instead of 1992) have to be corrected;
  MTF$`YEAR OF` <- car::recode(MTF$`YEAR OF`, "76=1976;77=1977;78=1978;79=1979;80=1980;81=1981;82=1982;83=1983;84=1984;85=1985;86=1986;87=1987;88=1988;89=1989;90=1990;91=1991;92=1992;93=1993;94=1994;95=1995;96=1996;97=1997;98=1998")

  # * Sex variable has to be recoded from 1 and 2 to Male and Female, respectively;
  MTF$`S SEX`[MTF$`S SEX` == 1] <- "Male"
  MTF$`S SEX`[MTF$`S SEX` == 2] <- "Female"

  # * In the Grade variable, values of 2 and 4 have to ve recoded to 8 and 10, respectively;
  MTF$`WHAT GRADE LEVL` <- car::recode(MTF$`WHAT GRADE LEVL`, "2=8;4=10")

  # * Labels have to be removed from the data (we'll add new, more meaningful ones later);

  # * All unnecessary variables which were used in the merging process have to be removed to save memory and space;
  rm(i, indices, j, labels, MTFfiles, vars, names, intermediate)

  # * Sex will be turned to factor;
  MTF$`S SEX` <- as.factor(MTF$`S SEX`)

  # Convert grade to approximate age (age = grade + 5)
  MTF$`WHAT GRADE LEVL` <- MTF$`WHAT GRADE LEVL` + 5

  # * We will sort the data by year of administration.
  MTF <- arrange(MTF, `YEAR OF`)

  # Rename variables
  colnames(MTF) <- c("Year", "Sex", "age", "H", "S", "SE_R_1", "SE_R_7", "SE_R_4", "SE_R_5", "SE_R_2", "D_B_1", "D_B_2", "SE_R_10", "F", "TV/WEEKDAY", "L_MTF_1", "L_MTF_2", "L_MTF_3", "L_MTF_4", "L_MTF_5", "L_MTF_6", "D_B_3", "D_B_4", "D_B_5", "D_B_6", "TV/WEEKEND", "COMPUTER/SCHOOL", "COMPUTER/OTHER", "COMPUTER JOB", "INTERNET/WEEK", "GAMING/WEEK", "social_media", "music", "SCREENTIME/WEEKDAY", "SCREENTIME/WEEKEND")

  # Let's also recode the items, so that all the responses indicate higher development of the construct - higher self-esteem, higher loneliness, more depressive symptoms. We'll also assign labels to those variables.
  MTF$SE_R_5 <- car::recode(MTF$SE_R_5, "1=5;2=4;4=2;5=1")
  MTF$SE_R_2 <- car::recode(MTF$SE_R_2, "1=5;2=4;4=2;5=1")
  MTF$L_MTF_2 <- car::recode(MTF$L_MTF_2, "1=5;2=4;4=2;5=1")
  MTF$L_MTF_4 <- car::recode(MTF$L_MTF_4, "1=5;2=4;4=2;5=1")
  MTF$L_MTF_6 <- car::recode(MTF$L_MTF_6, "1=5;2=4;4=2;5=1")
  MTF$D_B_4 <- car::recode(MTF$D_B_4, "1=5;2=4;4=2;5=1")
  MTF$D_B_6 <- car::recode(MTF$D_B_6, "1=5;2=4;4=2;5=1")

  # Standardize variable names
  MTF <- janitor::clean_names(MTF)

  # Remove data before 1991 because it only includes 12th graders (and thus if you include it, any change over time in the aggregate may just reflect a change from 12th graders to mean(8th grade, 10th grade, 12th grade)...)
  MTF <- filter(MTF, year > 1990)

  # Focus on individuals 15 years old or younger
  MTF <- filter(MTF, age <= 15)

  # Aggregate, rename, recode
  MTF <- MTF %>%
    # Scales and TV questions are aggregated
    mutate(
      self_esteem = rowMeans(select_at(., vars(contains("se_"))), na.rm = TRUE),
      depression = rowMeans(select_at(., vars(contains("d_b_"))), na.rm = TRUE),
      loneliness = rowMeans(select_at(., vars(contains("l_mtf_"))), na.rm = TRUE),
      tv = rowMeans(select_at(., vars(contains("tv_"))), na.rm = TRUE)
    ) %>%
    # Select variables
    dplyr::select(
      year, sex, age, tv, social_media,
      self_esteem, depression, loneliness,
      contains("se_"), contains("d_b_"), contains("l_mtf_")
    )

  # Save to disk
  saveRDS(MTF, "data/mtf.rds")

  # This function returns the cleaned dataset as a tibble
  return(MTF)
}
