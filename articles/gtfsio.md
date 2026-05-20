# Introduction to gtfsio

The General Transit Feed Specification (GTFS) data format defines a
common scheme for describing transit systems, and is widely used by
transit agencies around the world and consumed by many software
applications. Thus, many **R** packages have been developed to read,
write, manipulate and analyse such feeds, such as
[gtfs2gps](https://github.com/ipeaGIT/gtfs2gps),
[gtfsrouter](https://github.com/UrbanAnalyst/gtfsrouter),
[gtfstools](https://ipeagit.github.io/gtfstools/) and
[tidytransit](https://github.com/r-transit/tidytransit).

Each one of these, however, represent GTFS feeds in a slightly different
way, making the interoperability between packages harder to accomplish.
At the end of the day, this lack of integration results in a more
painful experience to the final user who may want to enjoy functions
from different packages.

**gtfsio** offers tools for the development of GTFS-related packages
that aim to increase such interoperability. It establishes a standard
for representing GTFS feeds using R data types based on [Google’s Static
GTFS Reference](https://developers.google.com/transit/gtfs/reference).
It provides fast and flexible functions to read and write GTFS feeds
while sticking to this standard. It defines a basic `gtfs` class which
is meant to be extended by packages that depend on it. And it also
offers utility functions that support checking the structure of GTFS
objects.

This vignette describes the basic usage of **gtfsio**. Please read the
[gtfs_reference dataset
documentation](https://r-transit.github.io/gtfsio/reference/gtfs_reference.html)
for more detail on the standards for reading and writing GTFS feeds
using R data types.

## Installation

Before using **gtfsio** please make sure that you have it installed in
your computer. You can download either the most stable version from CRAN
or the development version from GitHub:

``` r

# stable version
install.packages("gtfsio")

# development version
remotes::install_github("r-transit/gtfsio")
```

Then attach it to the current R session:

``` r

library(gtfsio)
```

Throughout this demonstration we will be using a few sample files
included in the package:

``` r

data_dir <- system.file("extdata", package = "gtfsio")
list.files(data_dir)
#> [1] "bad_gtfs.zip"       "blank_lines.zip"    "ggl_gtfs.zip"      
#> [4] "locations_feed.zip" "nested_gtfs.zip"    "subdirectories.zip"
```

- `ggl_gtfs.zip` has been manually built from the [example GTFS
  feed](https://developers.google.com/transit/gtfs/examples/gtfs-feed)
  provided by Google. The files samples are licensed under [Creative
  Commons Attribution 4.0
  License](https://creativecommons.org/licenses/by/4.0/).
- `bad_gtfs.zip` is a modified version of `ggl_gtfs.zip` that includes
  some issues frequently found in GTFS data.

## Basic usage

### Reading feeds

To read a feed use the
[`import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.md)
function. It takes either a local path or an URL to a GTFS `.zip` file
and returns a GTFS object (which is, basically, a `list` of data
frames):

``` r

gtfs_path <- file.path(data_dir, "ggl_gtfs.zip")
gtfs_url  <- "https://github.com/r-transit/gtfsio/raw/master/inst/extdata/ggl_gtfs.zip"

gtfs_from_path <- import_gtfs(gtfs_path)
names(gtfs_from_path)
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#>  [5] "frequencies"     "levels"          "pathways"        "routes"         
#>  [9] "shapes"          "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"          "attributions"   
#> [17] "calendar"

gtfs_from_url <- import_gtfs(gtfs_url)
names(gtfs_from_url)
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#>  [5] "frequencies"     "levels"          "pathways"        "routes"         
#>  [9] "shapes"          "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"          "attributions"   
#> [17] "calendar"
```

The function reads, by default, all `.txt` files contained in the `.zip`
file. Alternatively, you can specify which files should be read with the
`files` argument (note: without the `.txt` extension):

``` r

gtfs <- import_gtfs(gtfs_path, files = c("shapes", "trips"))

gtfs
#> $shapes
#>    shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
#>      <char>        <num>        <num>             <int>               <num>
#> 1:    A_shp     37.61956    -122.4816                 1              0.0000
#> 2:    A_shp     37.64430    -122.4107                 2              6.8310
#> 3:    A_shp     37.65863    -122.3084                 3             15.8765
#> 
#> $trips
#>    route_id service_id trip_id trip_headsign block_id
#>      <char>     <char>  <char>        <char>   <char>
#> 1:        A         WE    AWE1      Downtown        1
#> 2:        A         WE    AWE2      Downtown        2
```

Similarly, you can use the `fields` argument to read only a few
selective fields of a file. These arguments can be combined, offering a
great deal of flexibility that can translate into very fast reading
times.

``` r

gtfs <- import_gtfs(
  gtfs_path,
  files = c("shapes", "trips"),
  fields = list(trips = c("trip_id", "route_id"))
)

gtfs
#> $shapes
#>    shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
#>      <char>        <num>        <num>             <int>               <num>
#> 1:    A_shp     37.61956    -122.4816                 1              0.0000
#> 2:    A_shp     37.64430    -122.4107                 2              6.8310
#> 3:    A_shp     37.65863    -122.3084                 3             15.8765
#> 
#> $trips
#>    trip_id route_id
#>     <char>   <char>
#> 1:    AWE1        A
#> 2:    AWE2        A
```

These fields are parsed according to the standards for reading and
writing GTFS feeds in R. Undocumented files and fields (i.e. not
specified in the [GTFS
reference](https://developers.google.com/transit/gtfs/reference)) are
read as `character`, by default. You can overrule this default with
`extra_spec` (note that only undocumented fields should be specified in
this argument). `ggl_gtfs.zip` contains an undocumented field in the
`levels.txt` file, named `elevation`. Let’s check the effect of
`extra_spec`:

``` r

gtfs <- import_gtfs(gtfs_path, files = "levels")

gtfs$levels
#>    level_id level_index level_name elevation
#>      <char>       <num>     <char>    <char>
#> 1:       L0           0     Street         0
#> 2:       L1          -1  Mezzanine        -6
#> 3:       L2          -2 Southbound       -18
#> 4:       L3          -3 Northbound       -24

class(gtfs$levels$elevation)
#> [1] "character"

gtfs <- import_gtfs(
  gtfs_path, 
  files = "levels", 
  extra_spec = list(levels = c(elevation = "integer"))
)

gtfs$levels
#>    level_id level_index level_name elevation
#>      <char>       <num>     <char>     <int>
#> 1:       L0           0     Street         0
#> 2:       L1          -1  Mezzanine        -6
#> 3:       L2          -2 Southbound       -18
#> 4:       L3          -3 Northbound       -24

class(gtfs$levels$elevation)
#> [1] "integer"
```

### Writing feeds

Use
[`export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.md)
to write a GTFS object to disk. Please note that the function assumes
that the object is formatted according to the standards for reading and
writing GTFS feeds in **R** - i.e. if it’s not, any conversions should
be done before using
[`export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.md).

Objects are written as `.zip` feeds by default, but you can also write
them as directories using the `as_dir` argument:

``` r

gtfs <- import_gtfs(gtfs_path)
tmpf <- tempfile(fileext = ".zip")
tmpd <- tempfile()

export_gtfs(gtfs, tmpf)
zip::zip_list(tmpf)$filename
#>  [1] "calendar_dates.txt"  "fare_attributes.txt" "fare_rules.txt"     
#>  [4] "feed_info.txt"       "frequencies.txt"     "levels.txt"         
#>  [7] "pathways.txt"        "routes.txt"          "shapes.txt"         
#> [10] "stop_times.txt"      "stops.txt"           "transfers.txt"      
#> [13] "translations.txt"    "trips.txt"           "agency.txt"         
#> [16] "attributions.txt"    "calendar.txt"

export_gtfs(gtfs, tmpd, as_dir = TRUE)
list.files(tmpd)
#>  [1] "agency.txt"          "attributions.txt"    "calendar_dates.txt" 
#>  [4] "calendar.txt"        "fare_attributes.txt" "fare_rules.txt"     
#>  [7] "feed_info.txt"       "frequencies.txt"     "levels.txt"         
#> [10] "pathways.txt"        "routes.txt"          "shapes.txt"         
#> [13] "stop_times.txt"      "stops.txt"           "transfers.txt"      
#> [16] "translations.txt"    "trips.txt"
```

The function defaults to writing every element inside a GTFS object as a
`.txt` file. As with
[`import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.md),
use the `files` argument to overrule this behaviour:

``` r

export_gtfs(gtfs, tmpf, files = c("shapes", "trips"))
zip::zip_list(tmpf)$filename
#> [1] "shapes.txt" "trips.txt"
```

You can also use the `standard_only` argument to export only files and
fields specified in the [GTFS
reference](https://developers.google.com/transit/gtfs/reference)
(i.e. to leave out undocumented files/fields). In the example below,
`extra_gtfs` contains both an undocumented file (`extra_file`) and an
undocumented field in a regular file (`levels$elevation`) that are not
written to disk when using
[`export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.md):

``` r

extra_gtfs <- gtfs
extra_gtfs$extra_file <- data.frame(column = "value")

export_gtfs(extra_gtfs, tmpd, as_dir = TRUE, standard_only = TRUE)

"extra_file" %in% sub(".txt", "", list.files(tmpd))
#> [1] FALSE

levels_fields <- readLines(file.path(tmpd, "levels.txt"), n = 1L)
grepl("elevation", levels_fields)
#> [1] FALSE
```

### Checking GTFS objects

**gtfsio** also includes functions to check the structure of GTFS
objects.
[`check_file_exists()`](https://r-transit.github.io/gtfsio/reference/check_file_exists.md)
checks the existence of elements representing specific text files inside
an object. It returns `TRUE` if the check is successful, and `FALSE`
otherwise.
[`assert_file_exists()`](https://r-transit.github.io/gtfsio/reference/check_file_exists.md)
invisibly returns the object if successful, and throws an error
otherwise:

``` r

gtfs <- import_gtfs(gtfs_path, files = c("shapes", "trips"))

check_file_exists(gtfs, "shapes")
#> [1] TRUE
check_file_exists(gtfs, "stop_times")
#> [1] FALSE

assert_file_exists(gtfs, "shapes")
assert_file_exists(gtfs, "stop_times")
#> Error:
#> ! The GTFS object is missing the following required element(s): 'stop_times'
```

[`check_field_exists()`](https://r-transit.github.io/gtfsio/reference/check_field_exists.md)
checks the existence of fields, represented by columns, inside GTFS
objects. It returns `TRUE` if the check is successful, and `FALSE`
otherwise.
[`assert_field_exists()`](https://r-transit.github.io/gtfsio/reference/check_field_exists.md)
invisibly returns the object if successful, and throws an error
otherwise:

``` r

gtfs <- import_gtfs(
  gtfs_path,
  files = "trips",
  fields = list(trips = "trip_id")
)

check_field_exists(gtfs, "trips", fields = "trip_id")
#> [1] TRUE
check_field_exists(gtfs, "trips", fields = "shape_id")
#> [1] FALSE

assert_field_exists(gtfs, "trips", fields = "trip_id")
assert_field_exists(gtfs, "trips", fields = "shape_id")
#> Error:
#> ! The GTFS object 'trips' element is missing the following required column(s): 'shape_id'
```

[`check_field_class()`](https://r-transit.github.io/gtfsio/reference/check_field_class.md)
checks the classes of fields inside GTFS objects. It returns `TRUE` if
the check is successful, and `FALSE` otherwise.
[`assert_field_class()`](https://r-transit.github.io/gtfsio/reference/check_field_class.md)
invisibly returns the object if successful, and throws an error
otherwise:

``` r

gtfs <- import_gtfs(gtfs_path, files = "levels")

check_field_class(gtfs, "levels", fields = "elevation", classes = "character")
#> [1] TRUE
check_field_class(gtfs, "levels", fields = "elevation", classes = "integer")
#> [1] FALSE

assert_field_class(gtfs, "levels", fields = "elevation", classes = "character")
assert_field_class(gtfs, "levels", fields = "elevation", classes = "integer")
#> Error:
#> ! The following columns in the GTFS object 'levels' element do not inherit from the required classes:
#>   - 'elevation': requires integer, but inherits from character
```

Please notes that “lower-level” checks are conducted inside each
function - e.g. before checking the type of a field, first the existence
of such field is checked:

``` r

gtfs <- import_gtfs(gtfs_path, files = "shapes")

check_field_class(gtfs, "stop_times", fields = "stop_id", classes = "character")
#> [1] FALSE

assert_field_class(gtfs, "stop_times", fields = "stop_id", classes = "character")
#> Error:
#> ! The GTFS object is missing the following required element(s): 'stop_times'
```

These functions are great for package interoperability. If two distinct
packages represent GTFS text files using the same data structure (both
[gtfstools](https://ipeagit.github.io/gtfstools/) and
[gtfsrouter](https://github.com/UrbanAnalyst/gtfsrouter) use
`data.table`s, for example), they just need to add some basic checks
before proceeding with operations on objects created by the other
package.

So, if [gtfsrouter](https://github.com/UrbanAnalyst/gtfsrouter) requires
the `transfers` element to perform some operations, it might as well
perform them on an object created by
[gtfstools](https://ipeagit.github.io/gtfstools/), as long as it
contains a `transfers` element. Thus, it could greatly benefit of some
`assert_*`/`check_*` calls before proceeding with such operations.
