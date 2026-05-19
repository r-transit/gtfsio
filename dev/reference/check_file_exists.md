# Check the existence of text files in a GTFS object

Checks the existence of elements inside a GTFS object that represent
specific GTFS text files.

## Usage

``` r
check_file_exists(x, files)

assert_file_exists(x, files)
```

## Arguments

- x:

  A GTFS object.

- files:

  A character vector. The files to check the existence of.

## Value

`check_file_exists` returns `TRUE` if the check is successful, and
`FALSE` otherwise.  
`assert_file_exists` returns `x` invisibly if the check is successful,
and throws an error otherwise.

## See also

Other checking functions:
[`check_field_class()`](https://r-transit.github.io/gtfsio/dev/reference/check_field_class.md),
[`check_field_exists()`](https://r-transit.github.io/gtfsio/dev/reference/check_field_exists.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

check_file_exists(gtfs, c("calendar", "agency"))
#> [1] TRUE

check_file_exists(gtfs, c("calendar", "oi"))
#> [1] FALSE
```
