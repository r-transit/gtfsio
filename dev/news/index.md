# Changelog

## gtfsio (development version)

### Bug fixes

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  always silently skips metadata files in the `__MACOSX` subdirectory
  ([\#59](https://github.com/r-transit/gtfsio/issues/59))

## gtfsio 1.2.1

CRAN release: 2026-05-20

### Bug fixes

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  can now download URLs that do not end in .zip on Windows
  ([\#51](https://github.com/r-transit/gtfsio/pull/51))
- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  issues a warning if a feed contains a subdirectory
  ([\#53](https://github.com/r-transit/gtfsio/pull/53))
- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  skips blank lines in csv files
  ([\#57](https://github.com/r-transit/gtfsio/pull/57))

### Notes

- `gtfs_reference` has been updated to version 2026-04-27

## gtfsio 1.2.0

CRAN release: 2024-10-11

### New features

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  now reads geojson files as json
  ([\#37](https://github.com/r-transit/gtfsio/issues/37))
- Updated gtfs reference implementation with new `gtfs_reference`
  dataset. `get_gtfs_standard()` has been deprecated.

### Notes

- Internally, `fs` is now used for file path manipulations
  ([\#39](https://github.com/r-transit/gtfsio/issues/39))

## gtfsio 1.1.1

CRAN release: 2023-10-20

### Bug fixes

- Now prevents
  [`export_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/export_gtfs.md)
  to save large round numbers in scientific notation. This was not
  exactly a bug, as the specification does not forbid it, but the
  behavior could interfere with the workflow of people using other
  applications (as shown in
  [{gtfstools}](https://github.com/ipea/gtfstools/issues/73)
  [\#73](https://github.com/r-transit/gtfsio/issues/73)). Also, improves
  the readability of the tables.

## gtfsio 1.1.0

CRAN release: 2023-03-08

### Bug fixes

- Fixed a bug in which
  [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  would append a `.txt` extension to non text files found inside the
  GTFS feed, which would lead to an error when attempting to unzip the
  feed. The function now ignores non text files when reading feeds, and
  raises an informative warning message if any of such files is found
  ([\#23](https://github.com/r-transit/gtfsio/issues/23)).

### New features

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  now accepts paths and URLs without `.zip` extension, as long as they
  still point to zip files (in other words, it accepts zip files that
  for some reason or another do not have `.zip` extension).

### Notes

- Started converting more warnings to messages when `quiet = FALSE` in
  [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  and
  [`export_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/export_gtfs.md).
- Updated standards to read and write GTFS tables and fields in R due to
  changes in the specification.

## gtfsio 1.0.0

CRAN release: 2021-11-06

### Bug fixes

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  would raise a [bit64](https://github.com/r-lib/bit64)-related warning
  when the first row of any table included a 64-bit integer. This is now
  fixed - when reading the first row to figure out which fields are
  present, all columns are read as character vectors.

### New features

- Added (internal) input assertion functions, which declutter the input
  checking sections quite a bit.
- Added new (internal)
  [`gtfsio_error()`](https://r-transit.github.io/gtfsio/dev/reference/gtfsio_error.md)
  function, which raises a custom-classed error condition. All errors
  raised in gtfsio’s exported functions inherit from `gtfsio_error` and
  `<function_name>_error`, which allows for easier and more clear error
  catching.

### Potentially breaking changes

- Files and fields checking functions had their names changed. They now
  use the singular form, instead of the plural
  (i.e. `check_fields_exist()` was substituted by
  [`check_field_exists()`](https://r-transit.github.io/gtfsio/dev/reference/check_field_exists.md)).
  This shouldn’t raise reverse dependencies concern, because as of now
  only [gtfstools](https://ipeagit.github.io/gtfstools/) dev version
  uses such functions, which is an easy fix that doesn’t concern CRAN
  release.

## gtfsio 0.2.0

CRAN release: 2021-09-06

### Bug fixes

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  would occasionally include the full path to a table to their name in a
  GTFS object ([\#17](https://github.com/r-transit/gtfsio/issues/17)).
  Fixed by Mark Padgham ([@mpadge](https://github.com/mpadge)) in
  [\#18](https://github.com/r-transit/gtfsio/issues/18).

### New features

- Added new `summary.gtfs` method. Thanks Mark Padgham
  ([@mapdge](https://github.com/mapdge)).
- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  has a new `encoding` parameter, used to handle encoded strings in
  their native encoding.

## gtfsio 0.1.2

CRAN release: 2021-06-07

### Bug fixes

- Fixed a bug (or perhaps, changed the behaviour) of
  `check_fields_exist()` and `check_fields_types()` when a lower-level
  test (i.e. if the file that holds such field exists, for example)
  failed. Now it returns `FALSE` instead of raising an error.

## gtfsio 0.1.1

CRAN release: 2021-05-26

### Bug fixes

- Fixed a bug in
  [`export_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/export_gtfs.md)
  when `as_dir` was set to `TRUE` and `path` was set to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) in
  [\#15](https://github.com/r-transit/gtfsio/issues/15). The function
  now returns an (intentional) error if
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is passed to
  `path`. Thanks Flavio Poletti
  ([@polettif](https://github.com/polettif)).

### New features

- [`import_gtfs()`](https://r-transit.github.io/gtfsio/dev/reference/import_gtfs.md)
  has a new `skip` parameter. It may be used similarly to `files`, but
  you specify the files you *don’t want* to read, instead of the ones
  you do. Thanks Flavio Poletti
  ([@polettif](https://github.com/polettif)).

## gtfsio 0.1.0

CRAN release: 2021-04-06

- First CRAN release!
