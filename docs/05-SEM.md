# Latent Variable Models




```r
# Parallel processing settings
plan(multisession(workers = parallel::detectCores(logical = FALSE)))
```

## Data


```r
mtf <- read_rds("data/mtf.rds")
us <- read_rds("data/us.rds")
yrbs <- read_rds("data/yrbs.rds")
```

## Models


```r
fit <- function(data, items, x, y, name, missing = "ml") {
  # Center year
  data <- mutate(data, Year = Year - 2017)
  
  # Contrast code sex
  data <- mutate(data, Sex = ifelse(Sex=="Male", -0.5, 0.5))
  
  # Drop rows with missing predictor
  data <- drop_na(data, all_of(x))
  
  # Drop rows where all outcome items are missing
  data <- drop_na(data, all_of(y))
  
  # Mean-center predictors
  data <- data %>% 
    mutate(
      across(
        all_of(x), 
        ~as.numeric(scale(., center = TRUE, scale = FALSE))
      )
    )
  
  # Ordered?
  if (name=="YRBS") {
    data <- mutate(data, across(sad_lonely:suicide_3, ordered))
    missing = "listwise"
  }
  
  # Create interaction terms because lavaan doesn't know how to
  newdata <- model.matrix(
    as.formula(str_glue("{y} ~ Sex * Year * {x}")), 
    data = data
  )[,-1] %>%  # Take out the intercept column because it causes lavaan to break
    as.data.frame()
  # Interaction term breaks lavaan so change to dot
  names(newdata) <- str_replace_all(names(newdata), ":", ".")
  # return(newdata)
  newdata <- cbind(data[,items], newdata)
  # Combine names of items to a string for lavaan model
  items_all <- paste0(items, collapse = " + ")
  
  # Model strings
  sem0 <- str_glue("{y} =~ {items_all}\n{y} ~ Sex + Year + Sex.Year")
  sem1 <- str_glue("{y} =~ {items_all}\n{y} ~ Sex + Year + {x} + Sex.Year + Sex.{x} + Year.{x} + Sex.Year.{x}")
  
  ml0 <- sem(sem0, data = newdata, missing = missing)
  ml1 <- sem(sem1, data = newdata, missing = missing)
  
  tibble(
    data = name,
    Technology = x,
    Outcome = y,
    ml0 = list(ml0),
    ml1 = list(ml1)
  )
  
}
```


```r
x1 %<-% fit(yrbs, c("sad_lonely", paste0("suicide_", 1:3)), "TV", "Suicide", "YRBS")
x2 %<-% fit(yrbs, c("sad_lonely", paste0("suicide_", 1:3)), "DV", "Suicide", "YRBS")
x3 %<-% fit(mtf, paste0("D_B_", 1:6), "TV", "Depression", "MTF")
x4 %<-% fit(mtf, paste0("D_B_", 1:6), "SM", "Depression", "MTF")

sdq_con <- c("sdqe", "sdqg", "sdql", "sdqr", "sdqv")
sdq_emo <- c("sdqc", "sdqh", "sdqm", "sdqp", "sdqx")

x5 %<-% fit(us, sdq_con, "TV", "Conduct", "US")
x6 %<-% fit(us, sdq_con, "SM", "Conduct", "US")
x7 %<-% fit(us, sdq_emo, "TV", "Emotion", "US")
x8 %<-% fit(us, sdq_emo, "SM", "Emotion", "US")

# Rename variables
fits <- bind_rows(x1,x2,x3,x4,x5,x6,x7,x8)
fits <- fits %>% 
  mutate(
    Technology = ifelse(
      Technology %in% c("SM", "DV"), 
      "Social media / device", 
      "Television"
    )
  ) %>% 
  arrange(Outcome, Technology)
```

## Results


```r
coefs <- fits %>% 
  mutate(p = map(ml1, ~tidy(., conf.int = TRUE))) %>% 
  unnest(p) %>% 
  filter(op == "~") %>%
  separate(term, c("lhs", "rhs"), sep = " ~ ") %>% 
  mutate(N = map_dbl(ml1, nobs)) %>% 
  mutate(N = scales::comma(N, accuracy = 1))
coefs %>% 
  mutate(Parameter = str_replace(rhs, "SM|DV|TV", "Technology")) %>%
  mutate(Parameter = str_replace_all(Parameter, "\\.", ":")) %>% 
  mutate(Parameter = fct_inorder(Parameter)) %>% 
  mutate(Outcome = fct_rev(Outcome)) %>% 
  ggplot(aes(estimate, Outcome, shape = Technology, fill = p.value < 0.05)) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("white", "black"), guide = FALSE) +
  scale_x_continuous(
    "Parameter estimate",
    breaks = scales::pretty_breaks(), 
    expand = expansion(.25)
  ) +
  geom_vline(xintercept = 0, lty = 2, size = .25) +
  geom_linerangeh(
    aes(xmin = conf.low, xmax = conf.high), size = .25,
    position = position_dodge2v(.4), show.legend = FALSE
  ) +
  geom_point(
    size = 2, position = position_dodge2v(.4),
  ) +
  # geom_text(
  #   aes(label = N), size = 2, vjust = 2,
  #   position = position_dodge2v(.4)
  # ) +
  facet_wrap("Parameter", scales = "free_x", nrow = 2) +
  theme(
    legend.position = c(.875, .25),
    axis.title.y = element_blank(), 
    panel.spacing.x = unit(12, "pt")
  )
```

<img src="05-SEM_files/figure-html/unnamed-chunk-2-1.png" width="768" style="display: block; margin: auto;" />


```r
# Numbers
coefs %>% 
  mutate(coef = str_glue("{lhs} {op} {rhs}")) %>% 
  select(data:Outcome, coef, estimate:conf.high)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["data"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Technology"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Outcome"],"name":[3],"type":["chr"],"align":["left"]},{"label":["coef"],"name":[4],"type":["glue"],"align":["right"]},{"label":["estimate"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[10],"type":["dbl"],"align":["right"]}],"data":[{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Sex","5":"-0.1044875218","6":"0.0152499944","7":"-6.85164331","8":"7.300605e-12","9":"-0.1343769615","10":"-0.0745980821"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Year","5":"-0.0165857555","6":"0.0014496387","7":"-11.44130315","8":"0.000000e+00","9":"-0.0194269951","10":"-0.0137445159"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ SM","5":"0.0681493312","6":"0.0070047147","7":"9.72906600","8":"0.000000e+00","9":"0.0544203428","10":"0.0818783197"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Sex.Year","5":"0.0058201807","6":"0.0028774491","7":"2.02268764","8":"4.310536e-02","9":"0.0001804841","10":"0.0114598773"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Sex.SM","5":"-0.0132362839","6":"0.0139030315","7":"-0.95204300","8":"3.410752e-01","9":"-0.0404857250","10":"0.0140131572"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Year.SM","5":"-0.0025342544","6":"0.0013730313","7":"-1.84573687","8":"6.493042e-02","9":"-0.0052253462","10":"0.0001568374"},{"1":"US","2":"Social media / device","3":"Conduct","4":"Conduct ~ Sex.Year.SM","5":"-0.0037911137","6":"0.0027465922","7":"-1.38029726","8":"1.674951e-01","9":"-0.0091743355","10":"0.0015921081"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Sex","5":"-0.0868559015","6":"0.0151334651","7":"-5.73932678","8":"9.505365e-09","9":"-0.1165169481","10":"-0.0571948550"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Year","5":"-0.0127239577","6":"0.0014288231","7":"-8.90520141","8":"0.000000e+00","9":"-0.0155243995","10":"-0.0099235158"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ TV","5":"0.0715038489","6":"0.0098986061","7":"7.22362804","8":"5.062617e-13","9":"0.0521029374","10":"0.0909047605"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Sex.Year","5":"0.0073172266","6":"0.0028463254","7":"2.57076244","8":"1.014749e-02","9":"0.0017385312","10":"0.0128959219"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Sex.TV","5":"0.0182757349","6":"0.0197054795","7":"0.92744431","8":"3.536959e-01","9":"-0.0203462953","10":"0.0568977650"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Year.TV","5":"-0.0020822647","6":"0.0018770772","7":"-1.10931221","8":"2.672955e-01","9":"-0.0057612685","10":"0.0015967391"},{"1":"US","2":"Television","3":"Conduct","4":"Conduct ~ Sex.Year.TV","5":"0.0020016167","6":"0.0037535629","7":"0.53325782","8":"5.938551e-01","9":"-0.0053552313","10":"0.0093584648"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Sex","5":"0.3612804062","6":"0.0116136192","7":"31.10833924","8":"0.000000e+00","9":"0.3385181308","10":"0.3840426815"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Year","5":"0.0296670480","6":"0.0011960843","7":"24.80347560","8":"0.000000e+00","9":"0.0273227658","10":"0.0320113302"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ SM","5":"0.0084092619","6":"0.0052155651","7":"1.61233955","8":"1.068881e-01","9":"-0.0018130579","10":"0.0186315817"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Sex.Year","5":"0.0394581792","6":"0.0023889491","7":"16.51696092","8":"0.000000e+00","9":"0.0347759250","10":"0.0441404335"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Sex.SM","5":"0.0311267402","6":"0.0104311433","7":"2.98402000","8":"2.844882e-03","9":"0.0106820750","10":"0.0515714054"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Year.SM","5":"-0.0031849067","6":"0.0009795906","7":"-3.25126285","8":"1.148936e-03","9":"-0.0051048691","10":"-0.0012649443"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"Depression ~ Sex.Year.SM","5":"0.0009510722","6":"0.0019590971","7":"0.48546455","8":"6.273469e-01","9":"-0.0028886876","10":"0.0047908320"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Sex","5":"0.2015673206","6":"0.0064485441","7":"31.25780282","8":"0.000000e+00","9":"0.1889284064","10":"0.2142062348"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Year","5":"0.0019530145","6":"0.0002118183","7":"9.22023647","8":"0.000000e+00","9":"0.0015378583","10":"0.0023681707"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ TV","5":"0.0020299871","6":"0.0018523461","7":"1.09590053","8":"2.731223e-01","9":"-0.0016005446","10":"0.0056605189"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Sex.Year","5":"0.0077976811","6":"0.0004237438","7":"18.40187807","8":"0.000000e+00","9":"0.0069671586","10":"0.0086282036"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Sex.TV","5":"-0.0015940881","6":"0.0037045975","7":"-0.43029994","8":"6.669775e-01","9":"-0.0088549658","10":"0.0056667896"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Year.TV","5":"-0.0019162366","6":"0.0001245524","7":"-15.38498416","8":"0.000000e+00","9":"-0.0021603548","10":"-0.0016721184"},{"1":"MTF","2":"Television","3":"Depression","4":"Depression ~ Sex.Year.TV","5":"-0.0001578254","6":"0.0002490008","7":"-0.63383461","8":"5.261888e-01","9":"-0.0006458580","10":"0.0003302073"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Sex","5":"0.1236368483","6":"0.0084837995","7":"14.57328736","8":"0.000000e+00","9":"0.1070089068","10":"0.1402647899"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Year","5":"0.0035351396","6":"0.0007712600","7":"4.58358968","8":"4.570607e-06","9":"0.0020234977","10":"0.0050467815"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ SM","5":"0.0331096281","6":"0.0037847108","7":"8.74825834","8":"0.000000e+00","9":"0.0256917312","10":"0.0405275250"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Sex.Year","5":"0.0011645130","6":"0.0015382662","7":"0.75702953","8":"4.490322e-01","9":"-0.0018504334","10":"0.0041794593"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Sex.SM","5":"0.0379622648","6":"0.0074730603","7":"5.07988206","8":"3.776693e-07","9":"0.0233153358","10":"0.0526091938"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Year.SM","5":"0.0027153340","6":"0.0007358235","7":"3.69019718","8":"2.240803e-04","9":"0.0012731463","10":"0.0041575216"},{"1":"US","2":"Social media / device","3":"Emotion","4":"Emotion ~ Sex.Year.SM","5":"-0.0002185718","6":"0.0014675345","7":"-0.14893808","8":"8.816025e-01","9":"-0.0030948865","10":"0.0026577429"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Sex","5":"0.1400478334","6":"0.0084496374","7":"16.57441937","8":"0.000000e+00","9":"0.1234868484","10":"0.1566088183"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Year","5":"0.0049444816","6":"0.0007565688","7":"6.53540247","8":"6.343881e-11","9":"0.0034616340","10":"0.0064273292"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ TV","5":"0.0335843594","6":"0.0052517268","7":"6.39491747","8":"1.606342e-10","9":"0.0232911640","10":"0.0438775548"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Sex.Year","5":"0.0041009867","6":"0.0015047939","7":"2.72528125","8":"6.424673e-03","9":"0.0011516448","10":"0.0070503286"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Sex.TV","5":"0.0152080406","6":"0.0104102378","7":"1.46087350","8":"1.440502e-01","9":"-0.0051956506","10":"0.0356117319"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Year.TV","5":"0.0016197997","6":"0.0009912972","7":"1.63402036","8":"1.022546e-01","9":"-0.0003231070","10":"0.0035627065"},{"1":"US","2":"Television","3":"Emotion","4":"Emotion ~ Sex.Year.TV","5":"0.0009949967","6":"0.0019817424","7":"0.50208175","8":"6.156100e-01","9":"-0.0028891470","10":"0.0048791403"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Sex","5":"0.4805733697","6":"0.0250408680","7":"19.19156193","8":"0.000000e+00","9":"0.4314941702","10":"0.5296525692"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Year","5":"0.0012169757","6":"0.0019855373","7":"0.61292009","8":"5.399292e-01","9":"-0.0026746059","10":"0.0051085572"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ DV","5":"0.0599033296","6":"0.0057202540","7":"10.47214505","8":"0.000000e+00","9":"0.0486918378","10":"0.0711148215"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Sex.Year","5":"0.0156597613","6":"0.0039724940","7":"3.94204783","8":"8.078886e-05","9":"0.0078738162","10":"0.0234457065"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Sex.DV","5":"-0.0004530478","6":"0.0114060389","7":"-0.03971999","8":"9.683164e-01","9":"-0.0228084732","10":"0.0219023777"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Year.DV","5":"0.0007656597","6":"0.0009504588","7":"0.80556855","8":"4.204917e-01","9":"-0.0010972053","10":"0.0026285248"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"Suicide ~ Sex.Year.DV","5":"0.0010018156","6":"0.0019009326","7":"0.52701268","8":"5.981848e-01","9":"-0.0027239439","10":"0.0047275750"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Sex","5":"0.4926345332","6":"0.0249402162","7":"19.75261678","8":"0.000000e+00","9":"0.4437526077","10":"0.5415164588"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Year","5":"0.0080699414","6":"0.0019904083","7":"4.05441505","8":"5.025991e-05","9":"0.0041688128","10":"0.0119710700"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ TV","5":"0.0134627015","6":"0.0063583950","7":"2.11731127","8":"3.423344e-02","9":"0.0010004762","10":"0.0259249268"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Sex.Year","5":"0.0193887317","6":"0.0039826039","7":"4.86835558","8":"1.125307e-06","9":"0.0115829716","10":"0.0271944919"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Sex.TV","5":"0.0158027606","6":"0.0127140840","7":"1.24293347","8":"2.138924e-01","9":"-0.0091163862","10":"0.0407219073"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Year.TV","5":"-0.0003443434","6":"0.0010349348","7":"-0.33271991","8":"7.393457e-01","9":"-0.0023727782","10":"0.0016840914"},{"1":"YRBS","2":"Television","3":"Suicide","4":"Suicide ~ Sex.Year.TV","5":"0.0017047885","6":"0.0020698925","7":"0.82361211","8":"4.101600e-01","9":"-0.0023521262","10":"0.0057617032"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>



```r
coefs %>% 
  distinct(data, Technology, Outcome, N)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["data"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Technology"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Outcome"],"name":[3],"type":["chr"],"align":["left"]},{"label":["N"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"US","2":"Social media / device","3":"Conduct","4":"18,815"},{"1":"US","2":"Television","3":"Conduct","4":"19,079"},{"1":"MTF","2":"Social media / device","3":"Depression","4":"120,265"},{"1":"MTF","2":"Television","3":"Depression","4":"367,444"},{"1":"US","2":"Social media / device","3":"Emotion","4":"18,811"},{"1":"US","2":"Television","3":"Emotion","4":"19,074"},{"1":"YRBS","2":"Social media / device","3":"Suicide","4":"24,584"},{"1":"YRBS","2":"Television","3":"Suicide","4":"24,593"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


Just time


```r
fits %>% 
  mutate(p = map(ml0, ~tidy(., conf.int = TRUE))) %>% 
  unnest(p) %>% 
  filter(op == "~") %>%
  separate(term, c("lhs", "rhs"), sep = " ~ ") %>% 
  # mutate(
  #   Parameter = factor(rhs, levels = c("Year", "Technology", "Year x Technology"))
  # ) %>% 
  mutate(Outcome = fct_rev(Outcome)) %>% 
  ggplot(aes(estimate, Outcome, shape = Technology, fill = p.value < 0.05)) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("white", "black"), guide = FALSE) +
  scale_x_continuous(
    "Parameter estimate",
    breaks = scales::pretty_breaks(), 
    expand = expansion(.25)
  ) +
  geom_vline(xintercept = 0, lty = 2, size = .25) +
  geom_linerangeh(
    aes(xmin = conf.low, xmax = conf.high), size = .25,
    position = position_dodge2v(.4), show.legend = FALSE
  ) +
  geom_point(
    size = 2, position = position_dodge2v(.4),
  ) +
  facet_wrap("rhs", scales = "free_x", nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(), 
    panel.spacing.x = unit(12, "pt")
  )
```

<img src="05-SEM_files/figure-html/unnamed-chunk-5-1.png" width="768" style="display: block; margin: auto;" />


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
##  package     * version    date       lib source                            
##  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                    
##  backports     1.2.1      2020-12-09 [1] CRAN (R 4.0.2)                    
##  bookdown      0.21.6     2021-03-01 [1] Github (rstudio/bookdown@ca0145f) 
##  broom       * 0.7.5.9000 2021-03-01 [1] Github (tidymodels/broom@0b3528b) 
##  bslib         0.2.4      2021-01-25 [1] CRAN (R 4.0.3)                    
##  cellranger    1.1.0      2016-07-27 [1] CRAN (R 4.0.0)                    
##  cli           2.3.1      2021-02-23 [1] CRAN (R 4.0.3)                    
##  codetools     0.2-18     2020-11-04 [1] CRAN (R 4.0.2)                    
##  colorspace    2.0-0      2020-11-11 [1] CRAN (R 4.0.2)                    
##  crayon        1.4.1      2021-02-08 [1] CRAN (R 4.0.3)                    
##  DBI           1.1.1      2021-01-15 [1] CRAN (R 4.0.2)                    
##  dbplyr        2.1.0      2021-02-03 [1] CRAN (R 4.0.2)                    
##  digest        0.6.27     2020-10-24 [1] CRAN (R 4.0.2)                    
##  dplyr       * 1.0.4      2021-02-02 [1] CRAN (R 4.0.2)                    
##  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)                    
##  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)                    
##  fansi         0.4.2      2021-01-15 [1] CRAN (R 4.0.2)                    
##  farver        2.0.3      2020-01-16 [1] CRAN (R 4.0.0)                    
##  forcats     * 0.5.1      2021-01-27 [1] CRAN (R 4.0.2)                    
##  fs            1.5.0      2020-07-31 [1] CRAN (R 4.0.2)                    
##  future      * 1.21.0     2020-12-10 [1] CRAN (R 4.0.2)                    
##  generics      0.1.0      2020-10-31 [1] CRAN (R 4.0.2)                    
##  ggplot2     * 3.3.3      2020-12-30 [1] CRAN (R 4.0.2)                    
##  ggstance    * 0.3.5      2020-12-17 [1] CRAN (R 4.0.2)                    
##  globals       0.14.0     2020-11-22 [1] CRAN (R 4.0.2)                    
##  glue          1.4.2      2020-08-27 [1] CRAN (R 4.0.2)                    
##  gtable        0.3.0      2019-03-25 [1] CRAN (R 4.0.0)                    
##  haven         2.3.1      2020-06-01 [1] CRAN (R 4.0.0)                    
##  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)                    
##  hms           1.0.0      2021-01-13 [1] CRAN (R 4.0.2)                    
##  htmltools     0.5.1.1    2021-01-22 [1] CRAN (R 4.0.2)                    
##  httr          1.4.2      2020-07-20 [1] CRAN (R 4.0.2)                    
##  jquerylib     0.1.3      2020-12-17 [1] CRAN (R 4.0.2)                    
##  jsonlite      1.7.2      2020-12-09 [1] CRAN (R 4.0.2)                    
##  knitr       * 1.31       2021-01-27 [1] CRAN (R 4.0.2)                    
##  lavaan      * 0.6-7      2020-07-31 [1] CRAN (R 4.0.2)                    
##  lifecycle     1.0.0      2021-02-15 [1] CRAN (R 4.0.2)                    
##  listenv       0.8.0      2019-12-05 [1] CRAN (R 4.0.0)                    
##  lubridate     1.7.9.2    2020-11-13 [1] CRAN (R 4.0.2)                    
##  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.2)                    
##  mnormt        2.0.2      2020-09-01 [1] CRAN (R 4.0.2)                    
##  modelr        0.1.8      2020-05-19 [1] CRAN (R 4.0.0)                    
##  munsell       0.5.0      2018-06-12 [1] CRAN (R 4.0.0)                    
##  pacman        0.5.1      2019-03-11 [1] CRAN (R 4.0.0)                    
##  parallelly    1.23.0     2021-01-04 [1] CRAN (R 4.0.2)                    
##  pbivnorm      0.6.0      2015-01-23 [1] CRAN (R 4.0.0)                    
##  pillar        1.5.0      2021-02-22 [1] CRAN (R 4.0.3)                    
##  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.0)                    
##  purrr       * 0.3.4      2020-04-17 [1] CRAN (R 4.0.0)                    
##  R6            2.5.0      2020-10-28 [1] CRAN (R 4.0.2)                    
##  Rcpp          1.0.6      2021-01-15 [1] CRAN (R 4.0.2)                    
##  readr       * 1.4.0      2020-10-05 [1] CRAN (R 4.0.2)                    
##  readxl        1.3.1      2019-03-13 [1] CRAN (R 4.0.0)                    
##  reprex        1.0.0      2021-01-27 [1] CRAN (R 4.0.2)                    
##  rlang         0.4.10     2020-12-30 [1] CRAN (R 4.0.2)                    
##  rmarkdown     2.7.2      2021-03-01 [1] Github (rstudio/rmarkdown@9bfaf4a)
##  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.0.2)                    
##  rvest         0.3.6      2020-07-25 [1] CRAN (R 4.0.2)                    
##  sass          0.3.1      2021-01-24 [1] CRAN (R 4.0.2)                    
##  scales        1.1.1      2020-05-11 [1] CRAN (R 4.0.0)                    
##  sessioninfo * 1.1.1      2018-11-05 [1] CRAN (R 4.0.0)                    
##  stringi       1.5.3      2020-09-09 [1] CRAN (R 4.0.2)                    
##  stringr     * 1.4.0      2019-02-10 [1] CRAN (R 4.0.0)                    
##  tibble      * 3.1.0      2021-02-25 [1] CRAN (R 4.0.2)                    
##  tidyr       * 1.1.2      2020-08-27 [1] CRAN (R 4.0.2)                    
##  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.0)                    
##  tidyverse   * 1.3.0      2019-11-21 [1] CRAN (R 4.0.0)                    
##  tmvnsim       1.0-2      2016-12-15 [1] CRAN (R 4.0.0)                    
##  utf8          1.1.4      2018-05-24 [1] CRAN (R 4.0.0)                    
##  vctrs         0.3.6      2020-12-17 [1] CRAN (R 4.0.2)                    
##  withr         2.4.1      2021-01-26 [1] CRAN (R 4.0.2)                    
##  xfun          0.21       2021-02-10 [1] CRAN (R 4.0.2)                    
##  xml2          1.3.2      2020-04-23 [1] CRAN (R 4.0.0)                    
##  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)                    
## 
## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```
