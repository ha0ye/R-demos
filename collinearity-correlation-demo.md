collinearity-correlation demo
================
Hao Ye
2017 September 17

``` r
set.seed(42)
p <- rnorm(100)
x <- matrix(rnorm(500), ncol = 5)
for(i in 1:5)
{
    idx <- 1:20 + (i*20 - 20)
    x[idx, i] <- p[idx]
    x[idx, (i %% 5) + 1] <- p[idx] - rowSums(x[idx,-((i %% 5) + 1)])
}
```

``` r
all.equal(p, rowSums(x)) # test that p = x_1+x_2+x_3+x_4+x_5
```

    ## [1] TRUE

``` r
cor(x) # correlation matrix
```

    ##            [,1]       [,2]       [,3]        [,4]        [,5]
    ## [1,]  1.0000000 -0.2011700 -0.1817371 -0.19609874 -0.14528317
    ## [2,] -0.2011700  1.0000000 -0.2380931 -0.30502091 -0.27840190
    ## [3,] -0.1817371 -0.2380931  1.0000000 -0.15391307 -0.32570544
    ## [4,] -0.1960987 -0.3050209 -0.1539131  1.00000000 -0.08461244
    ## [5,] -0.1452832 -0.2784019 -0.3257054 -0.08461244  1.00000000
