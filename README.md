
# gtfsio <img align="right" src="man/figures/logo.png" width="180">

[![CRAN
status](https://www.r-pkg.org/badges/version/gtfsio)](https://CRAN.R-project.org/package=gtfsio)
[![gtfsio status
badge](https://dhersz.r-universe.dev/badges/gtfsio)](https://dhersz.r-universe.dev)
[![B
status](https://github.com/r-transit/gtfsio/workflows/R-CMD-check/badge.svg)](https://github.com/r-transit/gtfsio/actions?query=workflow%3AR-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/r-transit/gtfsio/branch/master/graph/badge.svg)](https://app.codecov.io/gh/r-transit/gtfsio?branch=master)
[![Lifecycle:
maturing](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/gtfsio?color=yellow)](https://CRAN.R-project.org/package=gtfsio)

**gtfsio** offers tools for the development of GTFS-related packages. It
establishes a standard for representing GTFS feeds using R data types
based on [Google’s Static GTFS
Reference](https://developers.google.com/transit/gtfs/reference). It
provides fast and flexible functions to read and write GTFS feeds while
sticking to this standard. It defines a basic `gtfs` class which is
meant to be extended by packages that depend on it. And it also offers
utility functions that support checking the structure of GTFS objects.

## Installation

Stable version:

``` r
install.packages("gtfsio")
```

Development version:

``` r
install.packages("gtfsio", repos = "https://dhersz.r-universe.dev")

# or
# install.packages("remotes")
remotes::install_github("r-transit/gtfsio")
```

## Usage

GTFS feeds are read using the `import_gtfs()` function:

``` r
library(gtfsio)

path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

gtfs <- import_gtfs(path)

names(gtfs)
#>  [1] "agency"          "attributions"    "calendar"       
#>  [4] "calendar_dates"  "fare_attributes" "fare_rules"     
#>  [7] "feed_info"       "frequencies"     "levels"         
#> [10] "pathways"        "routes"          "shapes"         
#> [13] "stop_times"      "stops"           "transfers"      
#> [16] "translations"    "trips"
```

`import_gtfs()` returns a `gtfs` object. The `gtfs` class might be
extended by other packages using the constructor, validator and methods
provided by **gtfsio**:

``` r
class(gtfs)
#> [1] "gtfs" "list"
```

Use the `export_gtfs()` function to write GTFS objects to disk:

``` r
tmpf <- tempfile(fileext = ".zip")

export_gtfs(gtfs, tmpf)

zip::zip_list(tmpf)$filename
#>  [1] "agency.txt"          "attributions.txt"   
#>  [3] "calendar.txt"        "calendar_dates.txt" 
#>  [5] "fare_attributes.txt" "fare_rules.txt"     
#>  [7] "feed_info.txt"       "frequencies.txt"    
#>  [9] "levels.txt"          "pathways.txt"       
#> [11] "routes.txt"          "shapes.txt"         
#> [13] "stop_times.txt"      "stops.txt"          
#> [15] "transfers.txt"       "translations.txt"   
#> [17] "trips.txt"
```

For a more complete demonstration please read the [introductory
vignette](https://r-transit.github.io/gtfsio/articles/gtfsio.html).

## GTFS-related packages

  - [`{tidytransit}`](https://github.com/r-transit/tidytransit)
  - [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps)
  - [`{gtfsrouter}`](https://github.com/ATFutures/gtfs-router)
  - [`{gtfstools}`](https://github.com/ipeaGIT/gtfstools)
