# YRBS preprocessing






```r
# Load data without dichotomized and supplemental items
data <- read_spss(
  "data-raw/yrbs/sadc_2017_national.sav",
  col_select = c(
    year, age, sex,
    q25:q29, q80, q81,
  )
)
```


```r
prop.table(table(data$sex, useNA = "always"))
# Drop cases where age or sex are unknown
data <- drop_na(data, age, sex)
```


```r
# Choose appropriate years: 2007-2015
data <- data %>% filter(year > 2006)
```


```r
# Convert SPSS labels to factor labels and remove other SPSS attributes
data <- as_factor(data)
```

## Recode Variables

Read about the variables at
https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2017/2017_yrbs_sadc_documentation.pdf


```r
# Sad/Lonely and Dichotomous suicide variables to numeric (0 = no, 1 = yes)
data <- mutate_at(data, vars(q25, q26, q27), ~1 - (as.numeric(.) - 1))
```


```r
# During the past 12 months, how many times did you actually attempt suicide?
data <- mutate(data, q28 = as.numeric(q28) - 1)
# This is dichotomized
data <- mutate(data, q28 = ifelse(q28==0, 0, 1))
```


```r
# If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
data <- mutate(
  data,
  q29 = factor(q29, levels = c("Did not attempt suicide", "No", "Yes")),
  q29 = as.numeric(q29)-1
)
```

Aggregate


```r
data <- data %>% 
  mutate(
    Suicide = rowMeans(select(., q25:q28), na.rm=T)
  ) 
```

## Age


```r
# Recode age as continuous with correct values
# Note: 12 and 18 include younger/older individuals
data <- mutate(data, age = as.numeric(age) + 11)
```


```r
# Focus on individuals 15 years old or younger
data <- filter(data, age <= 15)
data <- select(data, -age)
```

## Tech variables


```r
# Convert tech variable to numeric
data <- mutate_at(data, vars(q80, q81), ~as.numeric(.))
```

## Sex


```r
contrasts(data$sex) <- matrix(c(.5, -.5))
```


## Rename and save


```r
# Rename
data <- data %>%
  rename(
    Year = year,
    Sex = sex,
    TV = q80,
    DV = q81,
    sad_lonely = q25,
    suicide_1 = q26,
    suicide_2 = q27,
    suicide_3 = q28,
    suicide_4 = q29,
  )
```


```r
data
# Save data to disk
saveRDS(data, "data/yrbs.rds")
```


```r
library(sessioninfo)
session_info()
```

```
## ─ Session info ───────────────────────────────────────────────────────────────
##  setting  value                       
##  version  R version 4.0.2 (2020-06-22)
##  os       macOS Catalina 10.15.7      
##  system   x86_64, darwin17.0          
##  ui       X11                         
##  language (EN)                        
##  collate  en_GB.UTF-8                 
##  ctype    en_GB.UTF-8                 
##  tz       Europe/London               
##  date     2020-10-02                  
## 
## ─ Packages ───────────────────────────────────────────────────────────────────
##  package     * version date       lib source                              
##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.0)                      
##  bookdown      0.20    2020-06-23 [1] CRAN (R 4.0.2)                      
##  cli           2.0.2   2020-02-28 [1] CRAN (R 4.0.0)                      
##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.0)                      
##  digest        0.6.25  2020-02-23 [1] CRAN (R 4.0.0)                      
##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.0)                      
##  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.0)                      
##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)                      
##  htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.1)                      
##  knitr         1.30    2020-09-22 [1] CRAN (R 4.0.2)                      
##  magrittr      1.5     2014-11-22 [1] CRAN (R 4.0.0)                      
##  rlang         0.4.7   2020-07-09 [1] CRAN (R 4.0.2)                      
##  rmarkdown     2.4.0   2020-09-11 [1] Github (cpsievert/rmarkdown@b79fb4d)
##  sessioninfo * 1.1.1   2018-11-05 [1] CRAN (R 4.0.0)                      
##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)                      
##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.0)                      
##  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.2)                      
##  xfun          0.18    2020-09-29 [1] CRAN (R 4.0.2)                      
##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.0)                      
## 
## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```
