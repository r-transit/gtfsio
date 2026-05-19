# Read a GTFS text file

Reads a GTFS text file from the main `.zip` file.

## Usage

``` r
read_files(file, fields, extra_spec, tmpdir, quiet, encoding)
```

## Arguments

- file:

  A string. The name of the file (with `.txt` or `.geojson` extension)
  to be read.

- fields:

  A named list. Passed by the user to
  [`import_gtfs`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md).

- extra_spec:

  A named list. Passed by the user to
  [`import_gtfs`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md).

- tmpdir:

  A string. The path to the temporary folder where GTFS text files were
  unzipped to.

- quiet:

  Whether to hide log messages and progress bars (defaults to TRUE).

- encoding:

  A string. Passed to
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html), defaults to
  `"unknown"`. Other possible options are `"UTF-8"` and `"Latin-1"`.
  Please note that this is not used to re-encode the input, but to
  enable handling encoded strings in their native encoding.

## Value

A `data.table` representing the desired text file according to the
standards for reading and writing GTFS feeds with R.

## See also

[`gtfs_reference`](https://r-transit.github.io/gtfsio/dev/reference/gtfs_reference.md)
