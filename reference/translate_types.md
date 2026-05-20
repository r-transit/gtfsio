# Translate GTFS specification types to R equivalent types

Translate GTFS specification types to R equivalent types

## Usage

``` r
translate_types(text_file, r_equivalents)
```

## Arguments

- text_file:

  A named `list` containing a GTFS text file specification, as described
  in the body of
  [get_gtfs_standards](https://r-transit.github.io/gtfsio/reference/get_gtfs_standards.md).

- r_equivalents:

  A named `character vector`, in which each name is the GTFS
  specification type and the content its R equivalent.

## Value

A named `list` holding a GTFS text file specification, but with R data
types instead of GTFS spec data types.
