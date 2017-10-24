Google Maps Lat/Long Lookup
================
Hao Ye
2017 October 24

Look up Latitude and Longitude from place name:

``` r
library(ggmap)
```

    ## Loading required package: ggplot2

``` r
geocode("Lake Quesnel")
```

    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Lake%20Quesnel&sensor=false

    ##         lon      lat
    ## 1 -121.1164 52.53349
