
# gtfsio

[![CRAN
status](https://www.r-pkg.org/badges/version/gtfsio)](https://CRAN.R-project.org/package=gtfsio)
[![B
status](https://github.com/r-transit/gtfsio/workflows/R-CMD-check/badge.svg)](https://github.com/r-transit/gtfsio/actions?query=workflow%3AR-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/r-transit/gtfsio/branch/master/graph/badge.svg)](https://codecov.io/gh/r-transit/gtfsio?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

**gtfsio** is a package focused towards package development. It offers
tools to read and write transit feeds in the General Transit Feed
Specification (GTFS) format according to [Google’s Static GTFS
Reference](https://developers.google.com/transit/gtfs/reference). It
also defines a `gtfs` class, providing relevant methods, which is meant
to be extended by packages which depend on it.

## Installation

Development version:

``` r
# install.packages("remotes")
remotes::install_github("r-transit/gtfsio")
```

## Related packages

-   [`{tidytransit}`](https://github.com/r-transit/tidytransit)
-   [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps)
-   [`{gtfsrouter}`](https://github.com/ATFutures/gtfs-router)
-   [`{gtfstools}`](https://github.com/ipeaGIT/gtfstools)
