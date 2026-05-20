# Export GTFS objects

Writes GTFS objects to disk as GTFS transit feeds. The object must be
formatted according to the standards for reading and writing GTFS
transit feeds, as specified in
[`gtfs_reference`](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md)
(i.e. data types are not checked). If present, does not write auxiliary
tables held in a sub-list named `"."`.

## Usage

``` r
export_gtfs(
  gtfs,
  path,
  files = NULL,
  standard_only = FALSE,
  compression_level = 9,
  as_dir = FALSE,
  overwrite = TRUE,
  quiet = TRUE
)
```

## Arguments

- gtfs:

  A GTFS object.

- path:

  A string. Where the resulting `.zip` file must be written to.

- files:

  A character vector. The name of the elements to be written to the
  feed.

- standard_only:

  A logical. Whether only standard files and fields should be written
  (defaults to `TRUE`, which drops extra files and fields).

- compression_level:

  A numeric, between 1 and 9. The higher the value, the best the
  compression, which demands more processing time. Defaults to 9 (best
  compression).

- as_dir:

  A logical. Whether the feed should be exported as a directory, instead
  of a `.zip` file. Defaults to `FALSE`.

- overwrite:

  A logical. Whether to overwrite an existing `.zip` file (defaults to
  `TRUE`).

- quiet:

  A logical. Whether to hide log messages and progress bars (defaults to
  `TRUE`).

## Value

Invisibly returns the same GTFS object passed to `gtfs`.

## See also

[`gtfs_reference`](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md)

Other io functions:
[`import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

gtfs <- import_gtfs(gtfs_path)

tmpf <- tempfile(pattern = "gtfs", fileext = ".zip")

export_gtfs(gtfs, tmpf)
zip::zip_list(tmpf)$filename
#>  [1] "calendar_dates.txt"  "fare_attributes.txt" "fare_rules.txt"     
#>  [4] "feed_info.txt"       "frequencies.txt"     "levels.txt"         
#>  [7] "pathways.txt"        "routes.txt"          "shapes.txt"         
#> [10] "stop_times.txt"      "stops.txt"           "transfers.txt"      
#> [13] "translations.txt"    "trips.txt"           "agency.txt"         
#> [16] "attributions.txt"    "calendar.txt"       

export_gtfs(gtfs, tmpf, files = c("shapes", "trips"))
zip::zip_list(tmpf)$filename
#> [1] "shapes.txt" "trips.txt" 
```
