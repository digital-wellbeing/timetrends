# Bayesian Generalized Linear Mixed Model



For each dataset and outcome, we fit a GLMM using brms, in order to do full bayesian inference. These models take a while to run. 

## Data


```r
mtf <- read_rds("data/mtf.rds")
us <- read_rds("data/us.rds")
yrbs <- read_rds("data/yrbs.rds")
```

For US, we also isolate between- and within-person variables.


```r
# Within-person center variables (Vuorre & Bolger, 2017)
us <- bmlm::isolate(us, "pidp", "TV", which = "both", z = TRUE)
us <- bmlm::isolate(us, "pidp", "SM", which = "both", z = TRUE)
us <- us %>% select(-TV, -SM)
# We focus on between-person relations so rename those back
us <- us %>% rename(TV = TV_cb, SM = SM_cb)
```

YRBS is in long format wrt items


```r
yrbs <- yrbs %>% 
  select(-suicide_4, -Suicide) %>% 
  pivot_longer(
    sad_lonely:suicide_3, 
    names_to = "item", values_to = "Suicide"
  )
```

## Models

### Estimate

- MTF: Single level model
- US: Random intercepts for participants. 85% of participants have only 1 or 2 observations, so random slopes without strong priors are not a good idea.
- YRBS: Full item-specific random effects.


```r
fit <- function(data, name, x, y, iter = 1000, chains = 12, cores = 12) {
  data <- drop_na(data, all_of(x), all_of(y))
  data <- mutate(data, Year = Year - 2017)
  if (name=="YRBS") {
    data <- data %>% 
      mutate(across(all_of(x), ~as.numeric(scale(.))))
    ml1 <- brm(
      str_glue("{y} ~ Sex * Year * {x} + (Sex * Year * {x} | item)"), 
      family = bernoulli("probit_approx"),
      control = list(adapt_delta = .98),
      prior = prior(student_t(9, 0, 1), class = "sd") +
        prior(normal(0, 1), class = "b"),
      inits = "0",
      data = data, chains = chains, iter = iter, cores = cores,
      file = str_glue("models/brm-{name}-{x}-{y}-ml1-probit")
    )
  } else if (name=="MTF") {
    data <- data %>% 
      mutate(across(c(all_of(x), all_of(y)), ~as.numeric(scale(.))))
    ml1 <- brm(
      str_glue("{y} ~ Sex * Year * {x}"), 
      control = list(adapt_delta = .9),
      data = data, chains = chains, iter = iter, cores = cores,
      file = str_glue("models/brm-{name}-{x}-{y}-ml1")
    )
  } else if (name=="US") {
    data <- data %>% 
      mutate(across(c(all_of(x), all_of(y)), ~as.numeric(scale(.))))
    ml1 <- brm(
      str_glue("{y} ~ Sex * Year * {x} + (1 | pidp)"), 
      control = list(adapt_delta = .9),
      data = data, chains = chains, iter = iter, cores = cores,
      file = str_glue("models/brm-{name}-{x}-{y}-ml1")
    )
  }
  tibble(
    data = name,
    Technology = x,
    Outcome = y,
    ml1 = list(ml1)
  )
}
```


```r
x1 <- fit(yrbs, "YRBS", "TV", "Suicide")
x2 <- fit(yrbs, "YRBS", "DV", "Suicide")
x3 <- fit(mtf, "MTF", "TV", "Depression")
x4 <- fit(mtf, "MTF", "SM", "Depression")
x5 <- fit(us, "US", "SM", "Emotion")
x6 <- fit(us, "US", "SM", "Conduct")
x7 <- fit(us, "US", "TV", "Emotion")
x8 <- fit(us, "US", "TV", "Conduct")
```


```r
# Rename variables
fits <- bind_rows(x1,x2,x3,x4,x5,x6,x7,x8)
fits <- fits %>% 
  mutate(
    Technology = ifelse(
      Technology %in% c("SM", "DV"), 
      "Social media / device", 
      "Television"
    )
  )
```

### Results

Focus on some YRBS outcomes: plan suicide (suicide_2) on social media


```r
fits
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["data"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Technology"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Outcome"],"name":[3],"type":["chr"],"align":["left"]},{"label":["ml1"],"name":[4],"type":["list"],"align":["right"]}],"data":[{"1":"YRBS","2":"Television","3":"Suicide","4":"<S3: brmsfit>"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"<S3: brmsfit>"},{"1":"MTF","2":"Television","3":"Depression","4":"<S3: brmsfit>"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"<S3: brmsfit>"},{"1":"US","2":"Social media / device","3":"Emotion","4":"<S3: brmsfit>"},{"1":"US","2":"Social media / device","3":"Conduct","4":"<S3: brmsfit>"},{"1":"US","2":"Television","3":"Emotion","4":"<S3: brmsfit>"},{"1":"US","2":"Television","3":"Conduct","4":"<S3: brmsfit>"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
fitted(fits$ml1[[2]], newdata = tibble(item = "suicide_2", Sex = "Male", Year = 0, DV = 0:1), summary = FALSE, re_formula = NULL) %>% 
  as.data.frame() %>% 
  tibble() %>% 
  mutate(delta = V2-V1) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  tidybayes::mean_qi(value)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["value"],"name":[2],"type":["dbl"],"align":["right"]},{"label":[".lower"],"name":[3],"type":["dbl"],"align":["right"]},{"label":[".upper"],"name":[4],"type":["dbl"],"align":["right"]},{"label":[".width"],"name":[5],"type":["dbl"],"align":["right"]},{"label":[".point"],"name":[6],"type":["chr"],"align":["left"]},{"label":[".interval"],"name":[7],"type":["chr"],"align":["left"]}],"data":[{"1":"delta","2":"0.02122244","3":"0.01562249","4":"0.02722063","5":"0.95","6":"mean","7":"qi"},{"1":"V1","2":"0.08537796","3":"0.07855976","4":"0.09273055","5":"0.95","6":"mean","7":"qi"},{"1":"V2","2":"0.10660040","3":"0.09737051","4":"0.11635389","5":"0.95","6":"mean","7":"qi"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>



```r
probs <- c(.025, .1, .9, .975)
pars <- fits %>% 
  mutate(
    p = map(
      ml1, ~posterior_summary(., pars = "^b_", probs = probs) %>%
        as.data.frame() %>% 
        rownames_to_column("Parameter") %>% 
        tibble()
    )
  ) %>%
  unnest(p) %>% 
  select(-ml1)
b_Year <- pars %>% 
  filter(Parameter == "b_Year") %>% 
  mutate(Parameter = "Year")
b_Tech <- pars %>% 
  filter(Parameter %in% c("b_TV", "b_DV", "b_SM")) %>% 
  mutate(Parameter = "Technology")
b_Interaction <- pars %>% 
  filter(Parameter %in% c("b_Year:TV", "b_Year:DV", "b_Year:SM")) %>% 
  mutate(Parameter = "Year by Technology Interaction")
p1 <- bind_rows(b_Year, b_Tech, b_Interaction) %>% 
  mutate(Outcome = fct_rev(Outcome)) %>% 
  # Fill points based on if 95%CI includes zero
  mutate(
    Zero = ifelse(
      sign(Q2.5) == sign(Estimate) & sign(Estimate) == sign(Q97.5), 
      "*", ""
    )
  ) %>% 
  mutate(
    Parameter = factor(Parameter, levels = c("Year", "Technology", "Year by Technology Interaction"))
  ) %>% 
  ggplot(aes(Estimate, Outcome, shape = Technology, fill = Zero)) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("white", "black"), guide = FALSE) +
  scale_x_continuous(
    "Parameter estimate",
    breaks = scales::pretty_breaks(), 
    expand = expansion(.15)
  ) +
  geom_vline(xintercept = 0, lty = 2, size = .25) +
  geom_linerangeh(
    aes(xmin = Q2.5, xmax = Q97.5), size = .25,
    position = position_dodge2v(.5), show.legend = FALSE
  ) +
  geom_linerangeh(
    aes(xmin = Q10, xmax = Q90), size = .75,
    position = position_dodge2v(.5), show.legend = FALSE
  ) +
  geom_point(
    size = 2, position = position_dodge2v(.5),
  ) +
  facet_wrap("Parameter", scales = "free_x", nrow = 1) +
  theme(
    legend.position = "right",
    axis.title.y = element_blank(), 
    panel.spacing.x = unit(12, "pt")
  )

((p1 %+% 
    filter(p1$data, Parameter %in% c("Technology")) +
    theme(axis.title.x = element_blank())) /
  p1 %+% filter(p1$data, Parameter == "Year by Technology Interaction")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
```

<img src="04-BGLMM_files/figure-html/Parameter-figure-1.png" width="576" style="display: block; margin: auto;" />



```r
# Get YRBS item-specific effects
tmp1 <- slice(fits, 1:2) %>% 
  mutate(
    o = map(ml1, ~rownames_to_column(as.data.frame(coef(.)$item[,,7])))
  ) %>% 
  unnest(o)

tmp <- fits %>% 
  mutate(
    p = map(
      ml1, ~posterior_summary(., pars = "b_Year:[TV|SM|DV]") %>%
        as.data.frame() %>% 
        rownames_to_column() %>% 
        tibble()
    )
  ) %>%
  unnest(p)
tmp %>%   
  ggplot(aes(Estimate, Outcome, col = Outcome)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(
    "Linear interaction estimate (±95%CI)",
    breaks = scales::pretty_breaks(), 
    expand = expansion(.2)
  ) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  ggstance::geom_pointrangeh(
    shape = 21, fill = "white", fatten = 3,
    aes(xmin=Q2.5, xmax=Q97.5)
  ) +
  # ggstance::geom_pointrangeh(
  #   data = tmp1, aes(xmin = Q2.5, xmax = Q97.5),
  #   size = .25, fatten = 1,
  #   position = ggstance::position_dodge2v(.25)
  # ) +
  facet_wrap("Technology") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 8)
  )
```


```r
# Sex
pars %>% 
  filter(str_detect(Parameter, "Sex1:Year:")) %>% 
  mutate(Parameter = "Sex x Year x Tech") %>% 
  ggplot(aes(Estimate, Outcome, shape = Technology)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(
    "Estimated parameter",
    breaks = scales::pretty_breaks(), 
    expand = expansion(.25)
  ) +
  geom_vline(xintercept = 0, lty = 2, size = .25) +
  geom_linerangeh(
    aes(xmin = Q2.5, xmax = Q97.5), size = .25,
    position = position_dodge2v(.4), show.legend = FALSE
  ) +
  geom_linerangeh(
    aes(xmin = Q10, xmax = Q90), size = .75,
    position = position_dodge2v(.4), show.legend = FALSE
  ) +
  geom_point(
    size = 2, position = position_dodge2v(.4)
  ) +
  facet_wrap("Parameter", scales = "free_x", nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(), 
    panel.spacing.x = unit(12, "pt")
  )
```

<img src="04-BGLMM_files/figure-html/unnamed-chunk-5-1.png" width="576" style="display: block; margin: auto;" />


```r
options(width = 120)
library(sessioninfo)
session_info()
```

```
## ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
##  setting  value                       
##  version  R version 4.0.3 (2020-10-10)
##  os       macOS Big Sur 10.16         
##  system   x86_64, darwin17.0          
##  ui       X11                         
##  language (EN)                        
##  collate  en_GB.UTF-8                 
##  ctype    en_GB.UTF-8                 
##  tz       Europe/London               
##  date     2021-03-01                  
## 
## ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
##  package        * version    date       lib source                            
##  abind            1.4-5      2016-07-21 [1] CRAN (R 4.0.0)                    
##  arrayhelpers     1.1-0      2020-02-04 [1] CRAN (R 4.0.0)                    
##  assertthat       0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                    
##  backports        1.2.1      2020-12-09 [1] CRAN (R 4.0.2)                    
##  base64enc        0.1-3      2015-07-28 [1] CRAN (R 4.0.0)                    
##  bayesplot        1.8.0      2021-01-10 [1] CRAN (R 4.0.2)                    
##  bmlm             1.3.11     2019-02-21 [1] CRAN (R 4.0.0)                    
##  bookdown         0.21.6     2021-03-01 [1] Github (rstudio/bookdown@ca0145f) 
##  boot             1.3-27     2021-02-12 [1] CRAN (R 4.0.2)                    
##  bridgesampling   1.0-0      2020-02-26 [1] CRAN (R 4.0.0)                    
##  brms           * 2.14.4     2020-11-03 [1] CRAN (R 4.0.2)                    
##  Brobdingnag      1.2-6      2018-08-13 [1] CRAN (R 4.0.0)                    
##  broom            0.7.5.9000 2021-03-01 [1] Github (tidymodels/broom@0b3528b) 
##  bslib            0.2.4      2021-01-25 [1] CRAN (R 4.0.3)                    
##  callr            3.5.1      2020-10-13 [1] CRAN (R 4.0.2)                    
##  cellranger       1.1.0      2016-07-27 [1] CRAN (R 4.0.0)                    
##  cli              2.3.1      2021-02-23 [1] CRAN (R 4.0.3)                    
##  coda             0.19-4     2020-09-30 [1] CRAN (R 4.0.2)                    
##  codetools        0.2-18     2020-11-04 [1] CRAN (R 4.0.2)                    
##  colorspace       2.0-0      2020-11-11 [1] CRAN (R 4.0.2)                    
##  colourpicker     1.1.0      2020-09-14 [1] CRAN (R 4.0.2)                    
##  crayon           1.4.1      2021-02-08 [1] CRAN (R 4.0.3)                    
##  crosstalk        1.1.1      2021-01-12 [1] CRAN (R 4.0.2)                    
##  curl             4.3        2019-12-02 [1] CRAN (R 4.0.0)                    
##  DBI              1.1.1      2021-01-15 [1] CRAN (R 4.0.2)                    
##  dbplyr           2.1.0      2021-02-03 [1] CRAN (R 4.0.2)                    
##  digest           0.6.27     2020-10-24 [1] CRAN (R 4.0.2)                    
##  distributional   0.2.2      2021-02-02 [1] CRAN (R 4.0.2)                    
##  dplyr          * 1.0.4      2021-02-02 [1] CRAN (R 4.0.2)                    
##  DT               0.17       2021-01-06 [1] CRAN (R 4.0.2)                    
##  dygraphs         1.1.1.6    2018-07-11 [1] CRAN (R 4.0.0)                    
##  ellipsis         0.3.1      2020-05-15 [1] CRAN (R 4.0.0)                    
##  emmeans          1.5.4      2021-02-03 [1] CRAN (R 4.0.2)                    
##  estimability     1.3        2018-02-11 [1] CRAN (R 4.0.0)                    
##  evaluate         0.14       2019-05-28 [1] CRAN (R 4.0.0)                    
##  fansi            0.4.2      2021-01-15 [1] CRAN (R 4.0.2)                    
##  farver           2.0.3      2020-01-16 [1] CRAN (R 4.0.0)                    
##  fastmap          1.1.0      2021-01-25 [1] CRAN (R 4.0.3)                    
##  forcats        * 0.5.1      2021-01-27 [1] CRAN (R 4.0.2)                    
##  fs               1.5.0      2020-07-31 [1] CRAN (R 4.0.2)                    
##  future         * 1.21.0     2020-12-10 [1] CRAN (R 4.0.2)                    
##  gamm4            0.2-6      2020-04-03 [1] CRAN (R 4.0.2)                    
##  generics         0.1.0      2020-10-31 [1] CRAN (R 4.0.2)                    
##  ggdist           2.4.0      2021-01-04 [1] CRAN (R 4.0.2)                    
##  ggplot2        * 3.3.3      2020-12-30 [1] CRAN (R 4.0.2)                    
##  ggridges         0.5.3      2021-01-08 [1] CRAN (R 4.0.2)                    
##  ggstance       * 0.3.5      2020-12-17 [1] CRAN (R 4.0.2)                    
##  globals          0.14.0     2020-11-22 [1] CRAN (R 4.0.2)                    
##  glue             1.4.2      2020-08-27 [1] CRAN (R 4.0.2)                    
##  gridExtra        2.3        2017-09-09 [1] CRAN (R 4.0.0)                    
##  gtable           0.3.0      2019-03-25 [1] CRAN (R 4.0.0)                    
##  gtools           3.8.2      2020-03-31 [1] CRAN (R 4.0.0)                    
##  haven            2.3.1      2020-06-01 [1] CRAN (R 4.0.0)                    
##  highr            0.8        2019-03-20 [1] CRAN (R 4.0.0)                    
##  hms              1.0.0      2021-01-13 [1] CRAN (R 4.0.2)                    
##  htmltools        0.5.1.1    2021-01-22 [1] CRAN (R 4.0.2)                    
##  htmlwidgets      1.5.3      2020-12-10 [1] CRAN (R 4.0.2)                    
##  httpuv           1.5.5      2021-01-13 [1] CRAN (R 4.0.2)                    
##  httr             1.4.2      2020-07-20 [1] CRAN (R 4.0.2)                    
##  igraph           1.2.6      2020-10-06 [1] CRAN (R 4.0.2)                    
##  inline           0.3.17     2020-12-01 [1] CRAN (R 4.0.2)                    
##  jquerylib        0.1.3      2020-12-17 [1] CRAN (R 4.0.2)                    
##  jsonlite         1.7.2      2020-12-09 [1] CRAN (R 4.0.2)                    
##  knitr          * 1.31       2021-01-27 [1] CRAN (R 4.0.2)                    
##  later            1.1.0.1    2020-06-05 [1] CRAN (R 4.0.0)                    
##  lattice          0.20-41    2020-04-02 [1] CRAN (R 4.0.3)                    
##  lifecycle        1.0.0      2021-02-15 [1] CRAN (R 4.0.2)                    
##  listenv          0.8.0      2019-12-05 [1] CRAN (R 4.0.0)                    
##  lme4             1.1-26     2020-12-01 [1] CRAN (R 4.0.2)                    
##  loo              2.4.1      2020-12-09 [1] CRAN (R 4.0.2)                    
##  lubridate        1.7.9.2    2020-11-13 [1] CRAN (R 4.0.2)                    
##  magrittr         2.0.1      2020-11-17 [1] CRAN (R 4.0.2)                    
##  markdown         1.1        2019-08-07 [1] CRAN (R 4.0.0)                    
##  MASS             7.3-53.1   2021-02-12 [1] CRAN (R 4.0.2)                    
##  Matrix           1.3-2      2021-01-06 [1] CRAN (R 4.0.2)                    
##  matrixStats      0.58.0     2021-01-29 [1] CRAN (R 4.0.2)                    
##  mgcv             1.8-34     2021-02-16 [1] CRAN (R 4.0.2)                    
##  mime             0.10       2021-02-13 [1] CRAN (R 4.0.2)                    
##  miniUI           0.1.1.1    2018-05-18 [1] CRAN (R 4.0.0)                    
##  minqa            1.2.4      2014-10-09 [1] CRAN (R 4.0.0)                    
##  modelr           0.1.8      2020-05-19 [1] CRAN (R 4.0.0)                    
##  multcomp         1.4-16     2021-02-08 [1] CRAN (R 4.0.2)                    
##  munsell          0.5.0      2018-06-12 [1] CRAN (R 4.0.0)                    
##  mvtnorm          1.1-1      2020-06-09 [1] CRAN (R 4.0.0)                    
##  nlme             3.1-152    2021-02-04 [1] CRAN (R 4.0.2)                    
##  nloptr           1.2.2.2    2020-07-02 [1] CRAN (R 4.0.2)                    
##  pacman           0.5.1      2019-03-11 [1] CRAN (R 4.0.0)                    
##  parallelly       1.23.0     2021-01-04 [1] CRAN (R 4.0.2)                    
##  patchwork      * 1.1.1      2020-12-17 [1] CRAN (R 4.0.2)                    
##  pillar           1.5.0      2021-02-22 [1] CRAN (R 4.0.3)                    
##  pkgbuild         1.2.0      2020-12-15 [1] CRAN (R 4.0.2)                    
##  pkgconfig        2.0.3      2019-09-22 [1] CRAN (R 4.0.0)                    
##  plyr             1.8.6      2020-03-03 [1] CRAN (R 4.0.0)                    
##  prettyunits      1.1.1      2020-01-24 [1] CRAN (R 4.0.0)                    
##  processx         3.4.5      2020-11-30 [1] CRAN (R 4.0.2)                    
##  projpred         2.0.2      2020-10-28 [1] CRAN (R 4.0.2)                    
##  promises         1.2.0.1    2021-02-11 [1] CRAN (R 4.0.2)                    
##  ps               1.5.0      2020-12-05 [1] CRAN (R 4.0.2)                    
##  purrr          * 0.3.4      2020-04-17 [1] CRAN (R 4.0.0)                    
##  R6               2.5.0      2020-10-28 [1] CRAN (R 4.0.2)                    
##  Rcpp           * 1.0.6      2021-01-15 [1] CRAN (R 4.0.2)                    
##  RcppParallel     5.0.3      2021-02-24 [1] CRAN (R 4.0.3)                    
##  readr          * 1.4.0      2020-10-05 [1] CRAN (R 4.0.2)                    
##  readxl           1.3.1      2019-03-13 [1] CRAN (R 4.0.0)                    
##  reprex           1.0.0      2021-01-27 [1] CRAN (R 4.0.2)                    
##  reshape2         1.4.4      2020-04-09 [1] CRAN (R 4.0.0)                    
##  rlang            0.4.10     2020-12-30 [1] CRAN (R 4.0.2)                    
##  rmarkdown        2.7.2      2021-03-01 [1] Github (rstudio/rmarkdown@9bfaf4a)
##  rsconnect        0.8.16     2019-12-13 [1] CRAN (R 4.0.0)                    
##  rstan            2.21.3     2020-09-29 [1] Github (stan-dev/rstan@ae36e6f)   
##  rstantools       2.1.1      2020-07-06 [1] CRAN (R 4.0.2)                    
##  rstudioapi       0.13       2020-11-12 [1] CRAN (R 4.0.2)                    
##  rvest            0.3.6      2020-07-25 [1] CRAN (R 4.0.2)                    
##  sandwich         3.0-0      2020-10-02 [1] CRAN (R 4.0.2)                    
##  sass             0.3.1      2021-01-24 [1] CRAN (R 4.0.2)                    
##  scales           1.1.1      2020-05-11 [1] CRAN (R 4.0.0)                    
##  sessioninfo    * 1.1.1      2018-11-05 [1] CRAN (R 4.0.0)                    
##  shiny            1.6.0      2021-01-25 [1] CRAN (R 4.0.3)                    
##  shinyjs          2.0.0      2020-09-09 [1] CRAN (R 4.0.2)                    
##  shinystan        2.5.0      2018-05-01 [1] CRAN (R 4.0.0)                    
##  shinythemes      1.2.0      2021-01-25 [1] CRAN (R 4.0.3)                    
##  StanHeaders      2.21.0-7   2020-12-17 [1] CRAN (R 4.0.2)                    
##  statmod          1.4.35     2020-10-19 [1] CRAN (R 4.0.2)                    
##  stringi          1.5.3      2020-09-09 [1] CRAN (R 4.0.2)                    
##  stringr        * 1.4.0      2019-02-10 [1] CRAN (R 4.0.0)                    
##  survival         3.2-7      2020-09-28 [1] CRAN (R 4.0.3)                    
##  svUnit           1.0.3      2020-04-20 [1] CRAN (R 4.0.0)                    
##  TH.data          1.0-10     2019-01-21 [1] CRAN (R 4.0.0)                    
##  threejs          0.3.3      2020-01-21 [1] CRAN (R 4.0.0)                    
##  tibble         * 3.1.0      2021-02-25 [1] CRAN (R 4.0.2)                    
##  tidybayes        2.3.1      2020-11-02 [1] CRAN (R 4.0.2)                    
##  tidyr          * 1.1.2      2020-08-27 [1] CRAN (R 4.0.2)                    
##  tidyselect       1.1.0      2020-05-11 [1] CRAN (R 4.0.0)                    
##  tidyverse      * 1.3.0      2019-11-21 [1] CRAN (R 4.0.0)                    
##  utf8             1.1.4      2018-05-24 [1] CRAN (R 4.0.0)                    
##  V8               3.4.0      2020-11-04 [1] CRAN (R 4.0.2)                    
##  vctrs            0.3.6      2020-12-17 [1] CRAN (R 4.0.2)                    
##  withr            2.4.1      2021-01-26 [1] CRAN (R 4.0.2)                    
##  xfun             0.21       2021-02-10 [1] CRAN (R 4.0.2)                    
##  xml2             1.3.2      2020-04-23 [1] CRAN (R 4.0.0)                    
##  xtable           1.8-4      2019-04-21 [1] CRAN (R 4.0.0)                    
##  xts              0.12.1     2020-09-09 [1] CRAN (R 4.0.2)                    
##  yaml             2.2.1      2020-02-01 [1] CRAN (R 4.0.0)                    
##  zoo              1.8-8      2020-05-02 [1] CRAN (R 4.0.0)                    
## 
## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```
