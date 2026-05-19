# Subset a GTFS object

Subsetting a GTFS object using `[` preserves the object class.

## Usage

``` r
# S3 method for class 'gtfs'
x[value]
```

## Arguments

- x:

  A GTFS object.

- value:

  Either a numeric or a character vector. Designates the elements to be
  returned.

## Value

A GTFS object.

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

gtfs <- import_gtfs(gtfs_path)
names(gtfs)
#>  [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#>  [5] "frequencies"     "levels"          "pathways"        "routes"         
#>  [9] "shapes"          "stop_times"      "stops"           "transfers"      
#> [13] "translations"    "trips"           "agency"          "attributions"   
#> [17] "calendar"       
class(gtfs)
#> [1] "gtfs" "list"

small_gtfs <- gtfs[1:5]
names(small_gtfs)
#> [1] "calendar_dates"  "fare_attributes" "fare_rules"      "feed_info"      
#> [5] "frequencies"    
class(small_gtfs)
#> [1] "gtfs" "list"

small_gtfs <- gtfs[c("shapes", "trips")]
names(small_gtfs)
#> [1] "shapes" "trips" 
class(small_gtfs)
#> [1] "gtfs" "list"
```
