# Print summary of a GTFS object

Print summary of a GTFS object

## Usage

``` r
# S3 method for class 'gtfs'
summary(object, ...)
```

## Arguments

- object:

  A GTFS object.

- ...:

  Ignored.

## Value

Vector with the number of entries for each gtfs table.

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

summary(gtfs)
#> A gtfs object with the following tables and respective numbers of entries in each:
#>  calendar_dates fare_attributes      fare_rules       feed_info     frequencies 
#>               4               5              10               1               3 
#>          levels        pathways          routes          shapes      stop_times 
#>               4              19               1               3              11 
#>           stops       transfers    translations           trips          agency 
#>              16               3               3               2               1 
#>    attributions        calendar 
#>               2               2 
```
