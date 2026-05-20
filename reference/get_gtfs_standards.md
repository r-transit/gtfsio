# Generate GTFS standards (deprecated)

This function is **deprecated** and no longer used in
[`import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.md)
or
[`export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.md).
The dataset
[gtfs_reference](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md)
now contains the standard specifications.

## Usage

``` r
get_gtfs_standards()
```

## Value

A named list, in which each element represents the R equivalent of each
GTFS table standard (based on the specifications of 2022-05-09).

## See also

[gtfs_reference](https://r-transit.github.io/gtfsio/reference/gtfs_reference.md)
