Anonymous functions in dplyr and purrr
================
Hao Ye
2017 January 06

-   [Preamble](#preamble)
-   [Simple Workflow](#simple-workflow)
-   [Fancier Workflow](#fancier-workflow)
-   [Workflow with dplyr and purrr](#workflow-with-dplyr-and-purrr)
-   [Using a dplyr workflow to process model output](#using-a-dplyr-workflow-to-process-model-output)
    -   [single dataset example](#single-dataset-example)
    -   [multiple dataset example](#multiple-dataset-example)

Preamble
========

``` r
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
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     contains, order_by

Simple Workflow
===============

This is a silly example, but demonstrates what we want to do:

``` r
sample_data <- data.frame(x = runif(10), y = runif(10))
print(sample_data)
```

    ##            x           y
    ## 1  0.2304907 0.004440553
    ## 2  0.6050268 0.548838630
    ## 3  0.8476621 0.518779969
    ## 4  0.4425188 0.579565065
    ## 5  0.5288064 0.466710529
    ## 6  0.9775090 0.955430104
    ## 7  0.3394113 0.780254330
    ## 8  0.6359704 0.780465576
    ## 9  0.9064275 0.990811630
    ## 10 0.7698236 0.871429706

``` r
# imagine F is some model that we're fitting that returns some complex object, 
# in this case, computing a prediction for y using a linear model
F <- function(df) {
    data.frame(y_obs = df$y, 
               y_hat = lm(y ~ x, data = df)$fitted.values)
}

sample_output <- F(sample_data)
print(sample_output)
```

    ##          y_obs     y_hat
    ## 1  0.004440553 0.3209490
    ## 2  0.548838630 0.6303909
    ## 3  0.518779969 0.8308562
    ## 4  0.579565065 0.4961267
    ## 5  0.466710529 0.5674175
    ## 6  0.955430104 0.9381358
    ## 7  0.780254330 0.4109393
    ## 8  0.780465576 0.6559565
    ## 9  0.990811630 0.8794082
    ## 10 0.871429706 0.7665461

``` r
# imagine we want to process the output by computing the residual
G <- function(output) {
    data.frame(output, 
               resid = output$y_obs - output$y_hat)
}

sample_summary <- G(sample_output)
print(sample_summary)
```

    ##          y_obs     y_hat       resid
    ## 1  0.004440553 0.3209490 -0.31650849
    ## 2  0.548838630 0.6303909 -0.08155224
    ## 3  0.518779969 0.8308562 -0.31207626
    ## 4  0.579565065 0.4961267  0.08343838
    ## 5  0.466710529 0.5674175 -0.10070698
    ## 6  0.955430104 0.9381358  0.01729432
    ## 7  0.780254330 0.4109393  0.36931507
    ## 8  0.780465576 0.6559565  0.12450911
    ## 9  0.990811630 0.8794082  0.11140344
    ## 10 0.871429706 0.7665461  0.10488365

Fancier Workflow
================

Suppose that we have multiple datasets that we plan to do the same procedure to:

``` r
data_list <- list(data.frame(x = runif(10), y = runif(10)), 
                     data.frame(x = runif(100), y = runif(100)), 
                     data.frame(x = runif(1000), y = runif(1000)))

output_list <- lapply(data_list, F)
summary_list <- lapply(output_list, G)
```

Workflow with dplyr and purrr
=============================

We can replace the `lapply` calls using the `map` function from the `purrr` package:

``` r
# note that we use the data_frame instead of data.frame:
# the former makes it simpler to have a list column and makes a tibble object
my_data <- data_frame(species = c("A", "B", "C"), 
                      data = data_list)
print(my_data)
```

    ## # A tibble: 3 × 2
    ##   species                     data
    ##     <chr>                   <list>
    ## 1       A    <data.frame [10 × 2]>
    ## 2       B   <data.frame [100 × 2]>
    ## 3       C <data.frame [1,000 × 2]>

``` r
results <- my_data %>% 
    mutate(output = map(data, F)) %>% 
    mutate(summary = map(output, G))

print(results)
```

    ## # A tibble: 3 × 4
    ##   species                     data                   output
    ##     <chr>                   <list>                   <list>
    ## 1       A    <data.frame [10 × 2]>    <data.frame [10 × 2]>
    ## 2       B   <data.frame [100 × 2]>   <data.frame [100 × 2]>
    ## 3       C <data.frame [1,000 × 2]> <data.frame [1,000 × 2]>
    ## # ... with 1 more variables: summary <list>

Using a dplyr workflow to process model output
==============================================

single dataset example
----------------------

Note that our summary function `G` can be rewritten simply using `mutate`.

``` r
sample_summary_2 <- sample_output %>% 
    mutate(resid = y_obs - y_hat)

print(sample_summary_2)
```

    ##          y_obs     y_hat       resid
    ## 1  0.004440553 0.3209490 -0.31650849
    ## 2  0.548838630 0.6303909 -0.08155224
    ## 3  0.518779969 0.8308562 -0.31207626
    ## 4  0.579565065 0.4961267  0.08343838
    ## 5  0.466710529 0.5674175 -0.10070698
    ## 6  0.955430104 0.9381358  0.01729432
    ## 7  0.780254330 0.4109393  0.36931507
    ## 8  0.780465576 0.6559565  0.12450911
    ## 9  0.990811630 0.8794082  0.11140344
    ## 10 0.871429706 0.7665461  0.10488365

multiple dataset example
------------------------

But if we want to use it with map, we need to make an anonymous function. The key here is to wrap the `dplyr` workflow in parantheses:

``` r
G_dplyr <- function(output) {
    (output %>% mutate(resid = y_obs - y_hat))
}

results_dplyr <- my_data %>% 
    mutate(output = map(data, F)) %>%
    mutate(summary = map(output, G_dplyr))

print(results_dplyr)
```

    ## # A tibble: 3 × 4
    ##   species                     data                   output
    ##     <chr>                   <list>                   <list>
    ## 1       A    <data.frame [10 × 2]>    <data.frame [10 × 2]>
    ## 2       B   <data.frame [100 × 2]>   <data.frame [100 × 2]>
    ## 3       C <data.frame [1,000 × 2]> <data.frame [1,000 × 2]>
    ## # ... with 1 more variables: summary <list>

Yes, it's silly here, becasue `G_dplyr` is no more complex than `G`, but if we have some complex workflow in `dplyr` that we need to map, this allows us to do that.
