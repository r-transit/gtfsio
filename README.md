
# gtfsio

[![CRAN
status](https://www.r-pkg.org/badges/version/gtfsio)](https://CRAN.R-project.org/package=gtfsio)
[![B
status](https://github.com/r-transit/gtfsio/workflows/R-CMD-check/badge.svg)](https://github.com/r-transit/gtfsio/actions?query=workflow%3AR-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/r-transit/gtfsio/branch/master/graph/badge.svg)](https://codecov.io/gh/r-transit/gtfsio?branch=master)
[![Lifecycle:
maturing](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/gtfsio?color=yellow)](https://CRAN.R-project.org/package=gtfsio)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5337518.svg)](https://zenodo.org/record/5337518)

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
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"     
#>  [4] "feed_info"       "frequencies"     "levels"         
#>  [7] "pathways"        "routes"          "shapes"         
#> [10] "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"         
#> [16] "attributions"    "calendar"
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
#>  [1] "calendar_dates.txt"  "fare_attributes.txt"
#>  [3] "fare_rules.txt"      "feed_info.txt"      
#>  [5] "frequencies.txt"     "levels.txt"         
#>  [7] "pathways.txt"        "routes.txt"         
#>  [9] "shapes.txt"          "stop_times.txt"     
#> [11] "stops.txt"           "transfers.txt"      
#> [13] "translations.txt"    "trips.txt"          
#> [15] "agency.txt"          "attributions.txt"   
#> [17] "calendar.txt"
```

For a more complete demonstration please read the [introductory
vignette](https://r-transit.github.io/gtfsio/articles/gtfsio.html).

## GTFS-related packages

  - [`{tidytransit}`](https://github.com/r-transit/tidytransit)
  - [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps)
  - [`{gtfsrouter}`](https://github.com/ATFutures/gtfs-router)
  - [`{gtfstools}`](https://github.com/ipeaGIT/gtfstools)

## Citation

``` r
citation("gtfsio")
#> 
#> To cite gtfsio in publications use:
#> 
#>   Daniel Herszenhut, Flavio Poletti & Mark Padgham.
#>   (2021, August). gtfsio: Read and Write General
#>   Transit Feed Specification (GTFS) Files (Version
#>   v0.2.0). Zenodo.
#>   http://doi.org/10.5281/zenodo.5337518
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {gtfsio: Read and Write General Transit Feed Specification (GTFS) Files},
#>     author = {Daniel Herszenhut and Flavio Poletti and Mark Padgham},
#>     month = {aug},
#>     year = {2021},
#>     publisher = {Zenodo},
#>     version = {v0.2.0},
#>     doi = {10.5281/zenodo.5337518},
#>     url = {https://doi.org/10.5281/zenodo.5337518},
#>   }
```
