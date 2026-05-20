# GTFS object validator

Asserts that a GTFS object is valid. Valid objects are those in which:

- Every element is named.

- Every element inherits from `data.frame`s.

The exception to the second rule are objects that contain an element
named `"."`. In such case, this element is actually composed by a named
list of elements who inherit from `data.frame`s.

## Usage

``` r
assert_gtfs(x)
```

## Arguments

- x:

  A GTFS object.

## Value

The same GTFS object passed to `x`.

## See also

Other constructors:
[`new_gtfs()`](https://r-transit.github.io/gtfsio/reference/new_gtfs.md)

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

gtfs <- import_gtfs(gtfs_path)
gtfs <- assert_gtfs(gtfs)
```
