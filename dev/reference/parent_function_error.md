# Parent error function constructor

Creates a function that raises an error that is assigned to the function
in which the error was originally seen. Useful to prevent big repetitive
[`gtfsio_error()`](https://r-transit.github.io/gtfsio/dev/reference/gtfsio_error.md)
calls in the "main" functions.

## Usage

``` r
parent_function_error(message, subclass = character(0))
```

## Arguments

- message:

  The message to inform about the error.

- subclass:

  The subclass of the error.

## Value

errorCondition

## See also

Other error constructors:
[`gtfsio_error()`](https://r-transit.github.io/gtfsio/dev/reference/gtfsio_error.md)
