# Import GTFS transit feeds

Imports GTFS transit feeds from either a local `.zip` file or an URL.
Columns are parsed according to the standards for reading and writing
GTFS feeds specified in
[`gtfs_reference`](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md).

## Usage

``` r
import_gtfs(
  path,
  files = NULL,
  fields = NULL,
  extra_spec = NULL,
  skip = NULL,
  quiet = TRUE,
  encoding = "unknown"
)
```

## Arguments

- path:

  A string. The path to a GTFS `.zip` file.

- files:

  A character vector. The text files to be read from the GTFS, without
  the `.txt` extension. If `NULL` (the default), all existing text files
  are read.

- fields:

  A named list. The fields to be read from each text file, in the format
  `list(file1 = c("field1", "field2"))`. If `NULL` (the default), all
  fields from the files specified in `files` are read. If a file is
  specified in `files` but not in `fields`, all fields from that file
  will be read (i.e. you may specify in `fields` only files whose fields
  you want to subset).

- extra_spec:

  A named list. Custom specification used when reading undocumented
  fields, in the format
  `list(file1 = c(field1 = "type1", field2 = "type2"))`. If `NULL` (the
  default), all undocumented fields are read as character. Similarly, if
  an undocumented field is not specified in `extra_spec`, it is read as
  character (i.e. you may specify in `extra_spec` only the fields that
  you want to read as a different type). Only supports the `character`,
  `integer` and `numeric` types.

- skip:

  A character vector. Text files that should not be read from the GTFS,
  without the `.txt` extension. If `NULL` (the default), no files are
  skipped. Cannot be used if `files` is set. Files in a subfolder called
  `__MACOSX` are always silently skipped.

- quiet:

  A logical. Whether to hide log messages and progress bars (defaults to
  `TRUE`).

- encoding:

  A string. Passed to
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html), defaults to
  `"unknown"`. Other possible options are `"UTF-8"` and `"Latin-1"`.
  Please note that this is not used to re-encode the input, but to
  enable handling encoded strings in their native encoding.

## Value

A GTFS object: a named list of data frames, each one corresponding to a
distinct text file from the given GTFS feed.

## See also

[`gtfs_reference`](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md)

Other io functions:
[`export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

# read all files and columns
gtfs <- import_gtfs(gtfs_path)
names(gtfs)
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#>  [5] "frequencies"     "levels"          "pathways"        "routes"         
#>  [9] "shapes"          "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"          "attributions"   
#> [17] "calendar"       
names(gtfs$trips)
#> [1] "route_id"      "service_id"    "trip_id"       "trip_headsign"
#> [5] "block_id"     

# read all columns from selected files
gtfs <- import_gtfs(gtfs_path, files = c("trips", "stops"))
names(gtfs)
#> [1] "trips" "stops"
names(gtfs$trips)
#> [1] "route_id"      "service_id"    "trip_id"       "trip_headsign"
#> [5] "block_id"     

# read specific columns from selected files
gtfs <- import_gtfs(
  gtfs_path,
  files = c("trips", "stops"),
  fields = list(
    trips = c("route_id", "trip_id"),
    stops = c("stop_id", "stop_lat", "stop_lon")
  )
)
```
