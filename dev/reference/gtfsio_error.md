# gtfsio's custom error condition constructor

gtfsio's custom error condition constructor

## Usage

``` r
gtfsio_error(message, subclass = character(0), call = sys.call(-1))
```

## Arguments

- message:

  The message to inform about the error.

- subclass:

  The subclass of the error.

- call:

  A call to associate the error with.

## Value

errorCondition

## See also

Other error constructors:
[`parent_function_error()`](https://r-transit.github.io/gtfsio/dev/reference/parent_function_error.md)
