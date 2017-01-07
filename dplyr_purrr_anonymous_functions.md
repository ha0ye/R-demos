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
    -   [multiple dataset example (conditional mutate)](#multiple-dataset-example-conditional-mutate)

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

``` r
set.seed(42)
```

Simple Workflow
===============

This is a silly example, but demonstrates what we want to do:

``` r
sample_data <- data.frame(x = runif(10), y = runif(10))
print(sample_data)
```

    ##            x         y
    ## 1  0.9148060 0.4577418
    ## 2  0.9370754 0.7191123
    ## 3  0.2861395 0.9346722
    ## 4  0.8304476 0.2554288
    ## 5  0.6417455 0.4622928
    ## 6  0.5190959 0.9400145
    ## 7  0.7365883 0.9782264
    ## 8  0.1346666 0.1174874
    ## 9  0.6569923 0.4749971
    ## 10 0.7050648 0.5603327

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

    ##        y_obs     y_hat
    ## 1  0.4577418 0.6111525
    ## 2  0.7191123 0.6128411
    ## 3  0.9346722 0.5634810
    ## 4  0.2554288 0.6047556
    ## 5  0.4622928 0.5904464
    ## 6  0.9400145 0.5811459
    ## 7  0.9782264 0.5976383
    ## 8  0.1174874 0.5519948
    ## 9  0.4749971 0.5916026
    ## 10 0.5603327 0.5952479

``` r
# imagine we want to process the output by computing the residual
G <- function(output) {
    data.frame(output, 
               resid = output$y_obs - output$y_hat)
}

sample_summary <- G(sample_output)
print(sample_summary)
```

    ##        y_obs     y_hat       resid
    ## 1  0.4577418 0.6111525 -0.15341068
    ## 2  0.7191123 0.6128411  0.10627112
    ## 3  0.9346722 0.5634810  0.37119128
    ## 4  0.2554288 0.6047556 -0.34932677
    ## 5  0.4622928 0.5904464 -0.12815358
    ## 6  0.9400145 0.5811459  0.35886858
    ## 7  0.9782264 0.5976383  0.38058814
    ## 8  0.1174874 0.5519948 -0.43450749
    ## 9  0.4749971 0.5916026 -0.11660548
    ## 10 0.5603327 0.5952479 -0.03491513

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

    ##        y_obs     y_hat       resid
    ## 1  0.4577418 0.6111525 -0.15341068
    ## 2  0.7191123 0.6128411  0.10627112
    ## 3  0.9346722 0.5634810  0.37119128
    ## 4  0.2554288 0.6047556 -0.34932677
    ## 5  0.4622928 0.5904464 -0.12815358
    ## 6  0.9400145 0.5811459  0.35886858
    ## 7  0.9782264 0.5976383  0.38058814
    ## 8  0.1174874 0.5519948 -0.43450749
    ## 9  0.4749971 0.5916026 -0.11660548
    ## 10 0.5603327 0.5952479 -0.03491513

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

multiple dataset example (conditional mutate)
---------------------------------------------

``` r
library(dplyrExtras)
```

    ## Loading required package: data.table

    ## Warning: package 'data.table' was built under R version 3.3.2

    ## -------------------------------------------------------------------------

    ## data.table + dplyr code now lives in dtplyr.
    ## Please library(dtplyr)!

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'data.table'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## Loading required package: dtplyr

    ## 
    ## Attaching package: 'dplyrExtras'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     mutate_if

``` r
results_dplyr_cond <- my_data %>% 
    mutate(output = map(data, F)) %>%
    mutate_rows(species > "A", output = map(output, G_dplyr))

print(results_dplyr_cond)
```

    ## # A tibble: 3 × 3
    ##   species                     data                   output
    ##     <chr>                   <list>                   <list>
    ## 1       A    <data.frame [10 × 2]>    <data.frame [10 × 2]>
    ## 2       B   <data.frame [100 × 2]>   <data.frame [100 × 3]>
    ## 3       C <data.frame [1,000 × 2]> <data.frame [1,000 × 3]>
