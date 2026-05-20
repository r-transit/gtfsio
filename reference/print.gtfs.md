# Print a GTFS object

Prints a GTFS object suppressing the `class` attribute.

## Usage

``` r
# S3 method for class 'gtfs'
print(x, ...)
```

## Arguments

- x:

  A GTFS object.

- ...:

  Optional arguments ultimately passed to `format`.

## Value

The GTFS object that was printed, invisibly.

## Examples

``` r
gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

# subset 'gtfs' for a smaller output
gtfs <- gtfs[c("routes", "trips")]

print(gtfs)
#> $routes
#>    route_id route_short_name route_long_name
#>      <char>           <char>          <char>
#> 1:        A               17         Mission
#>                                                 route_desc route_type
#>                                                     <char>      <int>
#> 1: The ""A"" route travels from lower Mission to Downtown.          3
#> 
#> $trips
#>    route_id service_id trip_id trip_headsign block_id
#>      <char>     <char>  <char>        <char>   <char>
#> 1:        A         WE    AWE1      Downtown        1
#> 2:        A         WE    AWE2      Downtown        2
#> 
```
