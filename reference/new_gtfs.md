# GTFS object constructor

Creates a GTFS object. Mostly useful for package authors who may want to
either create `gtfs` objects in their packages or create subclasses of
the main `gtfs` class. The usage of this function assumes some knowledge
on `gtfs` objects, thus inputs are not extensively checked. See
[`assert_gtfs`](https://r-transit.github.io/gtfsio/reference/assert_gtfs.md)
for more thorough checks.

## Usage

``` r
new_gtfs(x, subclass = character(), ...)
```

## Arguments

- x:

  A GTFS-like object (either a GTFS object or a named list). Each
  element must represent a distinct GTFS text file.

- subclass:

  A character vector. Subclasses to be assigned to the `gtfs` object.

- ...:

  Name-value pairs. Additional attributes.

## Value

A GTFS object: a named list of data frames, each one corresponding to a
distinct GTFS text file, with `gtfs` and `list` classes.

## See also

Other constructors:
[`assert_gtfs()`](https://r-transit.github.io/gtfsio/reference/assert_gtfs.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

tmpdir <- tempfile(pattern = "new_gtfs_example")
zip::unzip(gtfs_path, exdir = tmpdir)

agency <- data.table::fread(file.path(tmpdir, "agency.txt"))
stops <- data.table::fread(file.path(tmpdir, "stops.txt"))
routes <- data.table::fread(file.path(tmpdir, "routes.txt"))
trips <- data.table::fread(file.path(tmpdir, "trips.txt"))
stop_times <- data.table::fread(file.path(tmpdir, "stop_times.txt"))
calendar <- data.table::fread(file.path(tmpdir, "calendar.txt"))

txt_files <- list(
  agency = agency,
  stops = stops,
  routes = routes,
  trips = trips,
  stop_times = stop_times,
  calendar = calendar
)

gtfs <- new_gtfs(txt_files)

class(gtfs)
#> [1] "gtfs" "list"
names(gtfs)
#> [1] "agency"     "stops"      "routes"     "trips"      "stop_times"
#> [6] "calendar"  
```
