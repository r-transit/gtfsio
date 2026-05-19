# Check the classes of fields in a GTFS object element

Checks the classes of fields, represented by columns, inside a GTFS
object element.

## Usage

``` r
check_field_class(x, file, fields, classes)

assert_field_class(x, file, fields, classes)
```

## Arguments

- x:

  A GTFS object.

- file:

  A string. The element, that represents a GTFS text file, whose fields'
  classes should be checked.

- fields:

  A character vector. The fields to have their classes checked.

- classes:

  A character vector, with the same length of `fields`. The classes that
  each field must inherit from.

## Value

`check_field_class` returns `TRUE` if the check is successful, and
`FALSE` otherwise.  
`assert_field_class` returns `x` invisibly if the check is successful,
and throws an error otherwise.

## See also

Other checking functions:
[`check_field_exists()`](https://r-transit.github.io/gtfsio/dev/reference/check_field_exists.md),
[`check_file_exists()`](https://r-transit.github.io/gtfsio/dev/reference/check_file_exists.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

check_field_class(
  gtfs,
  "calendar",
  fields = c("monday", "tuesday"),
  classes = rep("integer", 2)
)
#> [1] TRUE

check_field_class(
  gtfs,
  "calendar",
  fields = c("monday", "tuesday"),
  classes = c("integer", "character")
)
#> [1] FALSE
```
