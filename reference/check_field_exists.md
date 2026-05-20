# Check the existence of fields in a GTFS object element

Checks the existence of fields, represented by columns, inside a GTFS
object element.

## Usage

``` r
check_field_exists(x, file, fields)

assert_field_exists(x, file, fields)
```

## Arguments

- x:

  A GTFS object.

- file:

  A string. The element, that represents a GTFS text file, where fields
  should be searched.

- fields:

  A character vector. The fields to check the existence of.

## Value

`check_field_exists` returns `TRUE` if the check is successful, and
`FALSE` otherwise.  
`assert_field_exists` returns `x` invisibly if the check is successful,
and throws an error otherwise.

## See also

Other checking functions:
[`check_field_class()`](https://r-transit.github.io/gtfsio/reference/check_field_class.md),
[`check_file_exists()`](https://r-transit.github.io/gtfsio/reference/check_file_exists.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

check_field_exists(gtfs, "calendar", c("monday", "tuesday"))
#> [1] TRUE

check_field_exists(gtfs, "calendar", c("monday", "oi"))
#> [1] FALSE
```
