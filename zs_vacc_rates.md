Vaccination Rate by Zodiac Sign
================
Hao Ye
10/20/2021

``` r
library(tidyverse)
library(lubridate)
```

# SaltLakeHealth tweet

<https://twitter.com/SaltLakeHealth/status/1450570095550517249>

``` r
SLC_vacc_prop <- data.frame(zs = c("Leo", "Aquarius", 
                                 "Aries","Sagittarius", 
                                 "Cancer","Taurus", 
                                 "Gemini","Libra", 
                                 "Pisces","Capricorn", 
                                 "Virgo","Scorpio"), 
                          vacc_rate = c(0.70, 0.67, 
                                   0.59, 0.59, 
                                   0.58, 0.56, 
                                   0.55, 0.54, 
                                   0.51, 0.51, 
                                   0.50, 0.46))

# check that we got the input correct
knitr::kable(SLC_vacc_prop)
```

| zs          | vacc\_rate |
|:------------|-----------:|
| Leo         |       0.70 |
| Aquarius    |       0.67 |
| Aries       |       0.59 |
| Sagittarius |       0.59 |
| Cancer      |       0.58 |
| Taurus      |       0.56 |
| Gemini      |       0.55 |
| Libra       |       0.54 |
| Pisces      |       0.51 |
| Capricorn   |       0.51 |
| Virgo       |       0.50 |
| Scorpio     |       0.46 |

# Back-calculate proportion vaccinated

We assume the calculations are as follows:

``` r
# SLC_vacc_prop$vacc_rate = num_vac / (zs_prop * n)
#   - num_vac is from vaccination data
#   - zs_prop is zodiac sign proportion (from some source)
#   - n is SLC population estimate
```

## zodiac sign proportion

The tweet suggests the zodiac sign proportion is from University of
Texas-Austin. The reddit thread at
<https://www.reddit.com/r/SaltLakeCity/comments/qbzwra/helpful_data_from_salt_lake_county_health/>
points to this data from Texas A&M:
<http://faculty.tamucc.edu/sfriday/wordpress/?p=1317#>:\~:text=Most%20Common%20to%20Least%20Common%20Zodiac%20Signs%20,%200.090%20%25%20%208%20more%20rows%20

``` r
tamu_zs_prop <- data.frame(zs = c("Scorpio", "Virgo", 
                                  "Gemini","Pisces", 
                                  "Libra","Cancer", 
                                  "Taurus","Capricorn", 
                                  "Aries","Sagittarius", 
                                  "Leo","Aquarius"), 
                           zs_prop = c(0.094, 0.093, 
                                    0.092, 0.090, 
                                    0.087, 0.084, 
                                    0.083, 0.082, 
                                    0.081, 0.073, 
                                    0.071, 0.063))

# check that we got the input correct
knitr::kable(tamu_zs_prop)
```

| zs          | zs\_prop |
|:------------|---------:|
| Scorpio     |    0.094 |
| Virgo       |    0.093 |
| Gemini      |    0.092 |
| Pisces      |    0.090 |
| Libra       |    0.087 |
| Cancer      |    0.084 |
| Taurus      |    0.083 |
| Capricorn   |    0.082 |
| Aries       |    0.081 |
| Sagittarius |    0.073 |
| Leo         |    0.071 |
| Aquarius    |    0.063 |

## Back-calculation

``` r
SLC_vacc_df <- SLC_vacc_prop %>%
    left_join(tamu_zs_prop, by = "zs") %>%
    mutate(num_vac_over_n = vacc_rate * zs_prop)

knitr::kable(SLC_vacc_df)
```

| zs          | vacc\_rate | zs\_prop | num\_vac\_over\_n |
|:------------|-----------:|---------:|------------------:|
| Leo         |       0.70 |    0.071 |           0.04970 |
| Aquarius    |       0.67 |    0.063 |           0.04221 |
| Aries       |       0.59 |    0.081 |           0.04779 |
| Sagittarius |       0.59 |    0.073 |           0.04307 |
| Cancer      |       0.58 |    0.084 |           0.04872 |
| Taurus      |       0.56 |    0.083 |           0.04648 |
| Gemini      |       0.55 |    0.092 |           0.05060 |
| Libra       |       0.54 |    0.087 |           0.04698 |
| Pisces      |       0.51 |    0.090 |           0.04590 |
| Capricorn   |       0.51 |    0.082 |           0.04182 |
| Virgo       |       0.50 |    0.093 |           0.04650 |
| Scorpio     |       0.46 |    0.094 |           0.04324 |

# Better zodiac sign proportion data

FiveThirtyEight has US birth data at
<https://github.com/fivethirtyeight/data/tree/master/births>

Let’s check the distribution of zodiac signs:

## Function to return zodiac sign from date input

Originally from
<https://stackoverflow.com/questions/48026462/writing-r-function-using-lubridate-package-that-takes-dateex-august-14-and>,
with some modifications to vectorize properly

``` r
zodiac_sign <- function(input)
{
    zodiac_sign_df <- data.frame(Month = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"),
                                 Zodiac_Separation_Dt = c(21, 20, 21, 21, 23, 23, 23, 23, 22, 22, 20, 19),
                                 LowerSideZS = c("Pisces", "Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn","Aquarius"),
                                 UpperSideZS = c("Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn","Aquarius", "Pisces"),
                                 stringsAsFactors = FALSE )
    val_df <- zodiac_sign_df[match(months(input), zodiac_sign_df$Month), ]
    ifelse( day(input) >= val_df$Zodiac_Separation_Dt, 
            val_df$UpperSideZS, val_df$LowerSideZS)
}
```

## FiveThirtyEight US birth data

There are two datasets, let’s check both:

``` r
SSA_data <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv"
CDC_NCHS_data <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_1994-2003_CDC_NCHS.csv"

SSA_dat <- read.csv(SSA_data) %>%
    mutate(date = ISOdate(year, month, date_of_month), 
           zs = zodiac_sign(date)) %>%
    count(zs, wt = births) %>%
    mutate(zs_prop = n / sum(n)) %>%
    arrange(desc(zs_prop))

CDC_NCHS_dat <- read.csv(CDC_NCHS_data) %>%
    mutate(date = ISOdate(year, month, date_of_month), 
           zs = zodiac_sign(date)) %>%
    count(zs, wt = births) %>%
    mutate(zs_prop = n / sum(n)) %>%
    arrange(desc(zs_prop))

knitr::kable(SSA_dat)
knitr::kable(CDC_NCHS_dat)
```

| zs          |       n |  zs\_prop |
|:------------|--------:|----------:|
| Cancer      | 5595033 | 0.0899711 |
| Virgo       | 5574916 | 0.0896476 |
| Leo         | 5521329 | 0.0887859 |
| Gemini      | 5267980 | 0.0847119 |
| Libra       | 5228627 | 0.0840791 |
| Taurus      | 5161756 | 0.0830037 |
| Scorpio     | 5114444 | 0.0822429 |
| Pisces      | 5051808 | 0.0812357 |
| Sagittarius | 5023366 | 0.0807784 |
| Aquarius    | 5001409 | 0.0804253 |
| Aries       | 4983157 | 0.0801318 |
| Capricorn   | 4663199 | 0.0749867 |

| zs          |       n |  zs\_prop |
|:------------|--------:|----------:|
| Cancer      | 3579599 | 0.0901160 |
| Virgo       | 3537962 | 0.0890678 |
| Leo         | 3530547 | 0.0888811 |
| Gemini      | 3377507 | 0.0850283 |
| Libra       | 3338585 | 0.0840485 |
| Taurus      | 3322713 | 0.0836489 |
| Pisces      | 3243089 | 0.0816444 |
| Scorpio     | 3237561 | 0.0815052 |
| Aries       | 3198747 | 0.0805281 |
| Aquarius    | 3192257 | 0.0803647 |
| Sagittarius | 3186852 | 0.0802286 |
| Capricorn   | 2976718 | 0.0749385 |

These both have very similar zs\_prop values.

# Re-calculate proportion vaccinated

Using `SSA_dat$zs_prop`:

``` r
SLC_vacc_df %>%
    select(zs, num_vac_over_n) %>%
    left_join(SSA_dat, by = "zs") %>%
    mutate(vacc_rate = num_vac_over_n / zs_prop) %>%
    select(zs, vacc_rate) %>%
    arrange(desc(vacc_rate)) %>%
    knitr::kable()
```

| zs          | vacc\_rate |
|:------------|-----------:|
| Gemini      |  0.5973188 |
| Aries       |  0.5963926 |
| Pisces      |  0.5650223 |
| Taurus      |  0.5599747 |
| Leo         |  0.5597738 |
| Libra       |  0.5587598 |
| Capricorn   |  0.5576990 |
| Cancer      |  0.5415074 |
| Sagittarius |  0.5331873 |
| Scorpio     |  0.5257594 |
| Aquarius    |  0.5248350 |
| Virgo       |  0.5186978 |

Using `CDC_NCHS_dat$zs_prop`:

``` r
SLC_vacc_df %>%
    select(zs, num_vac_over_n) %>%
    left_join(CDC_NCHS_dat, by = "zs") %>%
    mutate(vacc_rate = num_vac_over_n / zs_prop) %>%
    select(zs, vacc_rate) %>%
    arrange(desc(vacc_rate)) %>%
    knitr::kable()
```

| zs          | vacc\_rate |
|:------------|-----------:|
| Gemini      |  0.5950958 |
| Aries       |  0.5934577 |
| Pisces      |  0.5621943 |
| Leo         |  0.5591740 |
| Libra       |  0.5589632 |
| Capricorn   |  0.5580575 |
| Taurus      |  0.5556559 |
| Cancer      |  0.5406367 |
| Sagittarius |  0.5368409 |
| Scorpio     |  0.5305183 |
| Aquarius    |  0.5252307 |
| Virgo       |  0.5220744 |

Ok, well, both of those actually look much more reasonable in terms of
disparities in vaccination rate with zodiac sign.
