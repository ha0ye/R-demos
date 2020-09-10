rodent\_hfl
================
Hao Ye
9/10/2020

``` r
library(portalr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
download_observations()
```

    ## ℹ Running gh query

    ## ℹ Running gh query, got 100 records of about 200                                                Downloading version 2.43.0 of the data...

    ## [1] TRUE

``` r
dat <- summarise_individual_rodents()
```

    ## Loading in data version 2.43.0

``` r
dat %>%
  group_by(species) %>%
  summarize(mean_hfl = mean(hfl, na.rm = TRUE)) %>%
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| species | mean\_hfl |
| :------ | --------: |
| BA      |  13.24802 |
| DM      |  35.89954 |
| DO      |  35.54172 |
| DS      |  49.93542 |
| NA      |  32.06667 |
| OL      |  20.31890 |
| OT      |  20.01614 |
| PB      |  26.02784 |
| PE      |  19.92540 |
| PF      |  15.43328 |
| PH      |  25.00000 |
| PI      |  21.96226 |
| PL      |  20.47917 |
| PM      |  20.30491 |
| PP      |  21.46363 |
| RF      |  17.51899 |
| RM      |  16.36591 |
| RO      |  15.63529 |
| SF      |  25.92416 |
| SH      |  28.67391 |
| SO      |  25.65854 |
| NA      |       NaN |
