# Functions to process Understanding Society data

#' Read and clean the Understanding Society dataset
#'
#' @param path Path to raw US data (character; "data-raw/us/")
#'
#' @return A clean tibble of Understanding Society data
read_us <- function(path = "data-raw/us/") {

  # Load the adolescent data from the folders downloaded from the UK data service.
  files_adolescent <- list.files(path, pattern = "_youth.sav", recursive = TRUE, full.names = TRUE)
  files_adolescent <- files_adolescent[str_detect(files_adolescent, "ukhls")]
  data_adolescent <- map(files_adolescent, read_adolescent_data)
  # Combine adolescent data from different waves (files) into long format
  data_adolescent <- bind_rows(data_adolescent, .id = "wave")

  # Load young adult's data
  files_pya <- list.files(path, pattern = "_indresp.sav", recursive = TRUE, full.names = TRUE)
  files_pya <- files_pya[str_detect(files_pya, "ukhls")]
  data_ya <- map(files_pya, read_youngadult_data)
  # In waves 3, 6 and 9 the social media use questions were filled out by all adults, not just the young adults (in waves 1 and 2 they were not filled out at all). For those waves where all adults filled in the questionnaire they were named differently (e.g. "c_netcht" instead of "c_ypnetcht"). To make sure there is no variable duplication we therefore renamed those variables so that they are the same for all waves. Note: The young adult and adult social media questions are identical, except that adults can get prompted with an additional phrase if they dont know what social media is: "This might include for business or professional reasons, dating, or just chatting or interacting with friends"."
  data_ya[[3]] <- rename(data_ya[[3]], ypnetcht = "netcht", ypsocweb = "socweb")
  data_ya[[6]] <- rename(data_ya[[6]], ypnetcht = "netcht", ypsocweb = "socweb")
  data_ya[[9]] <- rename(data_ya[[9]], ypnetcht = "netcht", ypsocweb = "socweb")
  # Combine YAs data from all waves into long format
  data_ya <- dplyr::bind_rows(data_ya, .id = "wave")

  # Here we flip the young adult life satisfaction questions as they are in the opposite direction to those filled out by adolescents
  data_ya <- data_ya %>%
    mutate_at(vars(contains("sclfsat")), function(x) 8 - x)

  # Combine young adults' and adolescents' data, excluding any teenagers from the adolescent data that are under 10 or over 15, and excluding any young adults from the YA data that are under 16 or over 21
  data_adolescent <- data_adolescent %>% filter(age_dv > 9) %>% filter(age_dv < 16)
  data_ya <- data_ya %>% filter(age_dv > 15) %>% filter(age_dv < 22)
  data <- full_join(
    data_adolescent,
    data_ya,
    by = c("pidp", "wave", "age_dv", "hidp", "fnspid", "mnspid")
  )

  # Get sex and ethnicity from "stable characteristics" file
  # In Understanding Society that have a specific file "ukhls_wx/xwavedat.sav" which indicates stable characteristics of participants in the survey. We prefer to use this for sex and ethnicity because it is derived from multiple interviews and checked for consistency.
  data_stable <- list.files(path, pattern = "xwavedat.sav", recursive = TRUE, full.names = TRUE) %>%
    read_spss()
  is.na(data_stable[, ]) <- data_stable[, ] < 0 # define NAs as anything under 0
  data_stable <- zap_labels(data_stable)
  data_stable <- data_stable %>% dplyr::select(pidp, sex_dv, ethn_dv)
  data <- left_join(data, data_stable, by = "pidp")

  # Rename and finalise adolescent and YA data
  # To finalise this step of cleaning the adolescent and YA we rename all the variables we cleaned to make them more understandable. We should note here that the names for self-esteem and the SDQ differ from those in the Millennium Cohort Study cleaning script (if provided as part of the project), this is because the questions differ between the two different studies.
  # We do not rename those variables that are already neatly named: sdqa-sdqy (24 items: Strengths and Difficulties Questionnaire), scghqa-scghql (12 items: subjective well-being GHQ scale), scsf1-scsf7 (multiple items for the SF-12 measuring mental and physical health)
  data <- data %>%
    dplyr::rename(
      age = age_dv,
      sex = sex_dv,
      ethnicity = ethn_dv,
      socialmedia = socweb,
      socialmedia_ya = ypsocweb,
      socialmediaamount = netcht,
      socialmediaamount_ya = ypnetcht,
      familysupport = famsup, #odd waves
      tvamount = tvvidhrs,
      tvamount_weekend = tvvidhrw, #even waves
      computer = comp, #odd waves
      computer_hmwrk = pchw, #odd waves
      computer_internet = cintnt, #odd waves
      computer_games = cpgs, #odd waves
      console = consol, #odd waves
      consoleamount = constm, #odd waves
      multiplayer_games = mulpgms, #even waves
      own_mobilephone = mobu, #odd waves
      satisfaction_schoolwork = hsw,
      satisfaction_appearance = hap,
      satisfaction_family = hfm,
      satisfaction_friends = hfr,
      satisfaction_school = hsc,
      satisfaction_life = hlf,
      familyeat = eatlivu,
      truant = truant,
      friends = npal,
      alcohol = evralc,
      alcoholamount = dklm,
      alcoholregular = regalco, #odd waves
      smoking = evrsmo,
      smokingamount = smofrq,
      stay_late = late,
      importance_academic = acvwell,
      selfesteem_a = esta,
      selfesteem_b = esti,
      selfesteem_c = estb,
      selfesteem_d = estj,
      selfesteem_e = estc,
      selfesteem_f = estk,
      selfesteem_g = este,
      selfesteem_h = estf,
      ghq_likert = scghq1_dv,
      ghq_caseness = scghq2_dv,
      satisfaction_health = sclfsat1,
      satisfaction_income = sclfsat2,
      satisfaction_leisure = sclfsat7,
      satisfaction_life_ya = sclfsato,
      sf12_physical_health = sf12pcs_dv,
      sf12_mental_health = sf12mcs_dv,
      sca_lonely_companionship = sclackcom,
      sca_lonely_leftout = scleftout,
      sca_lonely_isolated = scisolate,
      sca_lonely_lonely = sclonely
    )

  # We load and clean household data to merge with our children and YA to provide control variables.
  files_hh <- list.files(path, pattern = "_hhresp.sav", recursive = TRUE, full.names = TRUE)
  files_hh <- files_hh[str_detect(files_hh, "ukhls")]
  data_hh <- map(files_hh, read_household_data)
  data_hh <- bind_rows(data_hh, .id = "wave")

  # Join the adolescent/YA datasets and household level datasets together via the household identifier "hidp"
  data <- left_join(data, data_hh, by = c("hidp", "wave"))

  # Clean parent data
  data_parent <- map(files_pya, read_parent_data)
  data_parent <- bind_rows(data_parent, .id = "wave")

  # In both the youth and young adult data there are variables that denote the parents. fnspid/mnspid "Cross-wave person identifier (PIDP) of natural/adoptive/step father/mother. Based on edited information collected in the household grid. If there is more than one person who meets the criteria, the default is to select the person with the lowest PNO in the household." We can therefore match the mothers and fathers data with both teens and YA to provide further control variables.
  # We need to rename the identifiers so that they dont get wrongly combined with the youth identifiers when we merge the two datasets
  names(data_parent)[names(data_parent) == "pidp"] <- 'pnpid'
  data_parent <- data_parent %>% dplyr::select(-nqfhigh_dv) # delete two variables we do not need

  # We now merge the parents data with the children/YA data. This needs to be done in two steps. Firstly we merge the mother id in the child/YA data to get the mothers, we then rename the variables merged and do the same for the father.

  # We first merge the mother data and change the variable names to mother.
  # combine the datasets matching the participant ids and the year
  data <- left_join(
    data,
    data_parent,
    by = c("mnspid" = "pnpid", "wave" = "wave", "hidp" = "hidp")
  )
  data <- data %>%
    dplyr::rename(
      m_qfhigh_dv = qfhigh_dv,
      m_socialkid = socialkid,
      m_jbnssec8_dv = jbnssec8_dv,
      m_sf12pcs_dv = sf12pcs_dv,
      m_sf12mcs_dv = sf12mcs_dv
    )

  # We now do the same for fathers.
  data <- left_join(
    data,
    data_parent,
    by = c("fnspid" = "pnpid", "wave" = "wave", "hidp" = "hidp")
  )
  data <- data %>%
    dplyr::rename(
      f_qfhigh_dv = qfhigh_dv,
      f_socialkid = socialkid,
      f_jbnssec8_dv = jbnssec8_dv,
      f_sf12pcs_dv = sf12pcs_dv,
      f_sf12mcs_dv = sf12mcs_dv
    )

  # Lastly we clean up the overlap between the young adult questions and the adolescent questions which are currently stored in different variables.
  data$satisfaction_life <- ifelse(data$age < 16, data$satisfaction_life, ifelse(data$age > 15, data$satisfaction_life_ya, NA))
  data$socialmedia <- ifelse(data$age < 16, data$socialmedia, ifelse(data$age > 15, data$socialmedia_ya, NA))
  data$socialmediaamount <- ifelse(data$age < 16, data$socialmediaamount, ifelse(data$age > 15, data$socialmediaamount_ya, NA))
  data <- data %>%
    dplyr::select(-satisfaction_life_ya, -socialmedia_ya, -socialmediaamount_ya)

  # Recoding of variables
  # Now that we have the whole dataset merged and cleaned, and in a handy long format, we can start looking at our variables of interest, we need to inspect, clean and recode some of them.
  # First we rename the rest pf the parental and household variables to make it clearer what they are measuring
  data <- data %>%
    dplyr::rename(
      m_qualification = m_qfhigh_dv,
      m_physical_health = m_sf12pcs_dv,
      m_mental_health = m_sf12mcs_dv,
      m_socialkid = m_socialkid, #odd waves
      m_employment_quality = m_jbnssec8_dv,
      f_qualification = f_qfhigh_dv,
      f_physical_health = f_sf12pcs_dv,
      f_mental_health = f_sf12mcs_dv,
      f_socialkid = f_socialkid, #odd waves
      f_employment_quality = f_jbnssec8_dv,
      h_income = fihhmnnet1_dv,
      h_sib_number = nkids_dv,
      h_live_parent = npn_dv
    ) %>%
    dplyr::select(
      -mnspid,
      -fnspid,
      -hidp
    )

  # We first clean and recode the social media use variable. This variable is complicated as there was a mistake on the side of Understanding Society, who were inconsistent in how they directed children and adults through the study in different years. Sometimes those who said they dont use social media on "netcht" were automatically coded as none on "socweb" and sometimes this wasnt the case. There is no easy solution for this, so we went with a recoding solution detailed below. If a participant said they do not own a social media account or they dont use social media to interact with friends we coded them as 1, for the rest of the participants we took their netcht score. This creates the following scale, which is ordinal in nature: 1 = None, 2 = Less than an hour, 3 = 1-3 hours, 4 = 4-6 hours, 5 = 7 or more hours
  # set 9 to NA to revert coding error
  is.na(data[, c("socialmediaamount", "socialmedia")]) <- data[, c("socialmediaamount", "socialmedia")] == 9
  data$socialmedia <- ifelse((data$socialmedia == 2 | data$socialmediaamount == 1), 1,
                             ifelse(data$socialmediaamount == 2, 2,
                                    ifelse(data$socialmediaamount == 3, 3,
                                           ifelse(data$socialmediaamount == 4, 4,
                                                  ifelse(data$socialmediaamount == 5, 5, NA)))))
  data <- data %>% dplyr::select(-socialmediaamount)

  # Life satisfaction
  # Life satisfaction has some issues with its coding in the data as well. The 7 item scale has a couple of participants who scored 9. As the score for missing data is -9, we assume that these participants were wrongly coded and therefore code them as missing data. We also need to reverse code all of the variables so that high scores show high satisfaciton.
  is.na(data[, c("satisfaction_schoolwork", "satisfaction_appearance", "satisfaction_family", "satisfaction_friends", "satisfaction_school", "satisfaction_life", "satisfaction_health", "satisfaction_leisure", "satisfaction_income")]) <- data[, c("satisfaction_schoolwork", "satisfaction_appearance", "satisfaction_family", "satisfaction_friends", "satisfaction_school", "satisfaction_life", "satisfaction_health", "satisfaction_leisure", "satisfaction_income")] == 9
  data <- data %>%
    mutate_at(vars(contains("satisfaction_")), function(x) 8 - x)

  # Strengths and difficulties
  data <- data %>%
    mutate_at(
      c("sdql", "sdqv", "sdqr", "sdqb",
        "sdqe", "sdqo", "sdqj",
        "sdqf", "sdqm", "sdqs", "sdqc",
        "sdqp", "sdqw", "sdqx", "sdqh"),
      function(x) 4 - x
    )

  # Self esteem
  data <- data %>%
    mutate_at(c("selfesteem_a", "selfesteem_d", "selfesteem_e", "selfesteem_f"), function(x) 5 - x)

  # I also clean the variables for young adults: loneliness (we swap it so that higher scores means more less loneliness, i.e. better outcomes)
  data <- data %>%
    mutate_at(vars(contains("sca_lonely")), function(x) 4 - x)

  # For the general health questionnaire we swap the sides so that higher scores means better outcomes. We do not need to flip the s12 MCS or PCS as there higher scores means higher functioning
  data <- data %>%
    mutate_at(vars(contains("scghq")), function(x) 5 - x)

  # For **sex** we recode the variable so that 1 = male, 0 = female and we make NA the 13 data points who had inconsistent responses about their gender.
  data <- data %>%
    mutate(sex = ifelse(data$sex == 0, NA, ifelse(data$sex == 2, "Female", "Male"))) %>%
    mutate(sex = factor(sex))

  # For **ethnicity** we recode the variable as 1 for non-white and 0 as white. We class as white "british/english/scottish/welsh/northern irish", "irish", "any other white background", we do not count those of mixed white heritage.
  data <- data %>%
    mutate(ethnicity = as.numeric(ethnicity)) %>%
    mutate(ethnicity = as.factor(ifelse(data$ethnicity < 5, "white", "non-white")))

  # For **smoking** we code those 1 who said they never smoked on the "smoking" question. We then used the "smokingamount" to complete the rest (2 = "I have smoked only once or twice", 3 = "I used to smoke but I dont know", 4 = "I sometimes smoke, but not every week", 5 = "I usually smoke between one and six cigarettes a week", 6 = "I usually smoke more than six cigarettes a week").
  data <- data %>%
    mutate(smoking = ifelse(data$smoking == 2, 0, data$smokingamount) + 1) %>%
    dplyr::select(-smokingamount)

  # For **alcohol** we first reverse code dklm so that higher scores mean more alcohol consumption. Similar to smoking we then code all those who said they did not drink alcohol on "evralc" as 1 and if not they got their dklm score (5 = "Most days", 4 = "Once or twice a week", 3 = "2 or 3 times", 2 = "Once only", 1 = "Never")
  data <- data %>%
    mutate(alcoholamount = ifelse(data$alcoholamount == 1, 5,
                                  ifelse(data$alcoholamount == 2, 4,
                                         ifelse(data$alcoholamount == 3, 3,
                                                ifelse(data$alcoholamount == 4, 2,
                                                       ifelse(data$alcoholamount == 5, 1,
                                                              ifelse(data$alcoholamount == 6, 1, NA))))))) %>%
    mutate(alcohol = ifelse(data$alcohol == 2, 1, .$alcoholamount)) %>%
    dplyr::select(-alcoholamount)

  # For income levels we take the log of total household income (but we first transform participants who had 0 income to having 0.1 income), to normalise the variables:
  data$h_income[which(data$h_income==0)] = 0.1
  data <- data %>%
    mutate(h_income_log = log(h_income))

  # Take log of friends
  data$friends[which(data$friends==0)] = 0.1
  data <- data %>%
    mutate(friends_log = log(friends))

  # Reverse academic importance
  data$importance_academic <- 5-data$importance_academic

  # Educational qualification of parents is recoded. The measure is sadly not the same structure as the academic qualification in the MCS, and we need to recode it in some way. We threfore code as "5" = 1-6 ("higher degree"/"1st degree or equivalent"/"diploma in HE"/"teaching qualification not PGCE"/"Nursing or other medical qualification"/"Other higher degree"), "4" = 7-12 (A level, Welsh Baccalaureate, International Baccalaureate, AS Level, Highers (scotland), Cert 6th Year Studies), "3" = 13-14 (GCSE/O levels, CSE), "2" = 15-16 (Standard/o/lower, Other school certificate), "1" = 96 (those who stated they had "none of the above")
  data$m_qualification <- as.ordered(ifelse(data$m_qualification < 7, 5,
                                            ifelse(data$m_qualification < 13 & data$m_qualification > 6, 4,
                                                   ifelse(data$m_qualification == 13 | data$m_qualification == 14, 3,
                                                          ifelse(data$m_qualification == 15 | data$m_qualification == 16, 2,
                                                                 ifelse(data$m_qualification == 96, 1, NA)))
                                            )))
  data$f_qualification <- as.ordered(ifelse(data$f_qualification < 7, 5,
                                            ifelse(data$f_qualification < 13 & data$f_qualification > 6, 4,
                                                   ifelse(data$f_qualification == 13 | data$f_qualification == 14, 3,
                                                          ifelse(data$f_qualification == 15 | data$f_qualification == 16, 2,
                                                                 ifelse(data$f_qualification == 96, 1, NA)))
                                            )))

  # Furthermore we recode employment quality of parents.
  data$f_employment_quality <- as.ordered(data$f_qualification)
  data$m_employment_quality <- as.ordered(data$m_qualification)

  # Natural parent
  # We recode so that there is a score of 1 if both natural parents live with child, and a score of 0 if one or none of the natural parents live with the child.
  data$h_live_parent <- as.factor(ifelse(data$h_live_parent == 2, "both parents", "single or no parents"))

  # Sibling number, we take the children number and subtract one to get the number of siblings for the teenager
  data$h_sib_number <- ifelse(data$h_sib_number == 0, NA, (data$h_sib_number - 1))

  # We don't need to recode **tv use, familyeat, late, m_mental_health, m_physical_health**.

  # Convert wave to year
  # Waves 1-9 to years 2009 ->
  data <- rename(data, year = "wave")
  data$year <- as.character(as.numeric(data$year) + 2008)

  # Aggregate scales and select relevant variables
  data <- data %>%
    mutate(
      satisfaction = rowMeans(select_at(., vars(contains("satisfaction_"))), na.rm = TRUE),
      sdq = rowMeans(select_at(., vars(contains("sdq"))), na.rm = TRUE),
      self_esteem = rowMeans(select_at(., vars(contains("selfesteem_"))), na.rm = TRUE),
      scghq = rowMeans(select_at(., vars(contains("scghq"))), na.rm = TRUE),
      scsf = rowMeans(select_at(., vars(contains("scsf"))), na.rm = TRUE),
      games = rowMeans(select_at(., vars(computer_games, consoleamount)), na.rm = TRUE)
      # sca = rowMeans(select_at(., vars(contains("sca_"))), na.rm = TRUE)  # Only 1 year
    ) %>%
    dplyr::select(
      # Remove individual items
      -contains("satisfaction_"),
      -matches("sdq[a-z]"),
      -contains("selfesteem_"),
      -matches("scghq[a-z]"),
      -matches("scsf[0-9]"),
      -contains("sca_"),
      -tvamount_weekend
    ) %>%
    # Drop all rows where sex, year, or age are unknown
    drop_na(sex, year, age) %>%
    mutate(year = as.numeric(year)) %>%
    # Tag outcomes and predictors
    rename(
      y_satisfaction = satisfaction,
      y_sdq = sdq,
      y_self_esteem = self_esteem,
      y_scghq = scghq,
      y_scsf = scsf,
      # y_sca = sca,
      x_tv = tvamount,  # Weekday only
      x_games = games,
      x_social_media = socialmedia
    )

  # Return clean dataset
  return(data)
}

read_adolescent_data <- function(filename){
  # We create a function that cleans this data: it defines NAs, it removes yp from the beginning of variable names to make them eaiser to naviage and it only selects those variables of interest to proceed to the further analyses. We then apply this function to all seven datasets and remove the original data.
  # Function: read_adolescent_data
  # Inpout: an idata_x file
  # Method: this function cleans the data, it defines NAs (any negative numbers), it removes the yp from the beginning of variable names, and
  #         it also selects those variables of interest for further analyses.
  #         This is important because the datasets are very large and difficult to handle in full
  # Output: smaller data file
  dataset <- read_spss(filename)
  dataset <- dataset %>%
    dplyr::select(-ends_with("_sex")) %>%
    rename_all(~str_replace(., "yp", "")) %>%
    dplyr::select(
      contains("pidp"), #participant ID
      contains("hidp"), #househould identifer
      ends_with("age_dv"), #participant age
      contains("fnspid"),
      contains("mnspid"),
      ends_with("socweb"),
      ends_with("famsup"),
      ends_with("netcht"),
      ends_with("tvvidhrs"),
      ends_with("tvvidhrw"),
      ends_with("comp"),
      ends_with("pchw"),
      ends_with("cintnt"),
      ends_with("cpgs"),
      ends_with("consol"),
      ends_with("constm"),
      ends_with("mulpgms"),
      ends_with("mobu"),
      ends_with("hsw"),
      ends_with("hap"),
      ends_with("hfm"),
      ends_with("hfr"),
      ends_with("hsc"),
      ends_with("hlf"),
      contains("est"),
      contains("sdq"),
      contains("npal"),
      contains("eatlivu"),
      contains("dklm"),
      contains("regalco"),
      contains("evralc"),
      contains("evrsmo"),
      contains("smofrq"),
      ends_with("late"),
      ends_with("acvwell"),
      ends_with("truant"),
      ends_with("npn_dv"),
      -ends_with("_orig"),
      -ends_with("sdqes_dv"),
      -ends_with("sdqcp_dv"),
      -ends_with("sdqha_dv"),
      -ends_with("sdqpp_dv"),
      -ends_with("sdqps_dv"),
      -ends_with("sdqtd_dv")
    )
  dataset <- zap_labels(dataset)
  is.na(dataset[, ]) <- dataset[, ] < 0 #define NAs
  # Remove dataset indicator letter from variable names so that datasets can be merged
  names(dataset) <- str_remove(names(dataset), "[a-z]_")
  return(dataset)
}

read_youngadult_data <- function(filename){
  # Our sample ages out of the youth dataset at age 16, but complete the adult questionnaire, which also includes a certain number of questions only asked of 'young adults' between the ages of 16 and 21. We therefore merge those that completed well-being and social media questionnaires as adults, which they only do if they are between ages 16 and 21. We then clean that data too.
  dataset <- read_spss(
    filename,
    col_select = c(
      contains("pidp"),
      contains("hidp"),
      contains("fnspid"),
      contains("mnspid"),
      contains("scghq"),
      contains("sclfsat"), # 1= health, 2 = income, 7 = amount of leisure time, o = life
      contains("scsf"), # not available in wave 1; 1 = general health, 2a = health limits moderate activities, 2b = health limits several flights of stairs, 3a = last 4 weeks, physical health limits amount of work, 3b = last 4 weeks, physical health limits kind of work, 4a = last 4 weeks, mental health means accomplished less, 4b = last 4 weeks, mental health meant worked less carefully, 5 = last 4 weeks, pain infered with work, 6a = last 4 weeks, felt calm and peaceful, 6b = last 4 weeks, had a lot of energy, 6c = last 4 weeks, felt downhearted and depressed, 7 = last 4 weeks, physical or mental health interfered with social life
      ends_with("netcht"), #only wave 3,6,9 for adults, but waves 3-9 for 16 to 21 year olds
      ends_with("socweb"), #only wave 3,6,9 for adults, but waves 3-9 for 16 to 21 year olds
      ends_with("sclackcom"), #Only wave 9
      ends_with("scleftout"), #Only wave 9
      ends_with("scisolate"), #Only wave 9
      ends_with("sclonely"), #Only wave 9
      ends_with("sf12pcs_dv"),
      ends_with("sf12mcs_dv"),
      ends_with("scghq1_dv"),
      ends_with("age_dv")
    )
  )
  dataset <- zap_labels(dataset)
  is.na(dataset[, ]) <- dataset[, ] < 0 #define NAs
  # Remove dataset indicator letter from variable names so datasets can be merged
  names(dataset) <- str_remove(names(dataset), "[a-z]_")
  return(dataset)
}

read_parent_data <- function(filename){
  # The young adult data used above is stored in the same data file as all the other parent data. As loading such large datasets takes a long time we also extract the variables we want from parents (i.e. our control variables) during this same stage. Below we have therefore written the cleaning script for the parent data.
  # Function: clean_data (second version, now for the mother)
  # Input: an mdata_x file
  # Method: this function cleans the  mothersdata, it defines NAs (any negative numbers) and
  #         it also selects those variables of interest for further analyses.
  #         This is important because the datasets are very large and difficult to handle in full
  # Output: smaller data file
  dataset <- read_spss(
    filename,
    col_select = c(
      contains("pidp"),
      contains("hidp"),
      ends_with("qfhigh_dv"),
      ends_with("socialkid"),
      ends_with("jbnssec8_dv"),
      ends_with("sf12pcs_dv"),
      ends_with("sf12mcs_dv")
    )
  )
  dataset <- zap_labels(dataset)
  is.na(dataset[, ]) <- dataset[, ] < 0 #define NAs
  # Remove dataset indicator letter from variable names
  names(dataset) <- str_remove(names(dataset), "[a-z]_")
  return(dataset)
}

read_household_data <- function(filename){
  # Function: clean_data (third version, now for the household)
  # Inpout: an hdata_x file
  # Method: this function cleans the  household data, it defines NAs (any negative numbers) and
  #         it also selects those variables of interest for further analyses.
  #         This is important because the datasets are very large and difficult to handle in full
  # Output: smaller hdata file
  dataset <- read_spss(
    filename,
    col_select = c(
      contains("pidp"),
      contains("hidp"),
      ends_with("fihhmnnet1_dv"),
      ends_with("nkids_dv")
    )
  )
  is.na(dataset[, ]) <- dataset[, ] < 0 #define NAs
  dataset <- zap_labels(dataset)
  names(dataset) <- str_remove(names(dataset), "[a-z]_")
  return(dataset)
}
