
# gtfsio

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
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5650829.svg)](https://zenodo.org/record/5650829)

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
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#>  [5] "frequencies"     "levels"          "pathways"        "routes"         
#>  [9] "shapes"          "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"          "attributions"   
#> [17] "calendar"
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
#>  [1] "calendar_dates.txt"  "fare_attributes.txt" "fare_rules.txt"     
#>  [4] "feed_info.txt"       "frequencies.txt"     "levels.txt"         
#>  [7] "pathways.txt"        "routes.txt"          "shapes.txt"         
#> [10] "stop_times.txt"      "stops.txt"           "transfers.txt"      
#> [13] "translations.txt"    "trips.txt"           "agency.txt"         
#> [16] "attributions.txt"    "calendar.txt"
```

For a more complete demonstration please read the [introductory
vignette](https://r-transit.github.io/gtfsio/articles/gtfsio.html).

## GTFS-related packages

-   [`{tidytransit}`](https://github.com/r-transit/tidytransit)
-   [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps)
-   [`{gtfsrouter}`](https://github.com/ATFutures/gtfs-router)
-   [`{gtfstools}`](https://github.com/ipeaGIT/gtfstools)

## Citation

``` r
citation("gtfsio")
#> 
#> To cite gtfsio in publications use:
#> 
#>   Daniel Herszenhut, Flavio Poletti & Mark Padgham. (2021, November).
#>   gtfsio: Read and Write General Transit Feed Specification (GTFS)
#>   Files (Version v1.0.0). Zenodo. http://doi.org/10.5281/zenodo.5650829
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {gtfsio: Read and Write General Transit Feed Specification (GTFS) Files},
#>     author = {Daniel Herszenhut and Flavio Poletti and Mark Padgham},
#>     month = {nov},
#>     year = {2021},
#>     publisher = {Zenodo},
#>     version = {v1.0.0},
#>     doi = {10.5281/zenodo.5650829},
#>     url = {https://doi.org/10.5281/zenodo.5650829},
#>   }
```
