# gtfsio 1.1.1

## Bug fixes

- Now prevents `export_gtfs()` to save large round numbers in scientific notation. This was not exactly a bug, as the specification does not forbid it, but the behavior could interfere with the workflow of people using other applications (as shown in [{gtfstools} #73](https://github.com/ipeaGIT/gtfstools/issues/73)). Also, improves the readability of the tables.

# gtfsio 1.1.0

## Bug fixes

- Fixed a bug in which `import_gtfs()` would append a `.txt` extension to non text files found inside the GTFS feed, which would lead to an error when attempting to unzip the feed. The function now ignores non text files when reading feeds, and raises an informative warning message if any of such files is found ([#23](https://github.com/r-transit/gtfsio/issues/23)).

## New features

- `import_gtfs()` now accepts paths and URLs without `.zip` extension, as long as they still point to zip files (in other words, it accepts zip files that for some reason or another do not have `.zip` extension).

## Notes

- Started converting more warnings to messages when `quiet = FALSE` in `import_gtfs()` and `export_gtfs()`.
- Updated standards to read and write GTFS tables and fields in R due to changes in the specification.

# gtfsio 1.0.0

## Bug fixes

- `import_gtfs()` would raise a `{bit64}`-related warning when the first row of any table included a 64-bit integer. This is now fixed - when reading the first row to figure out which fields are present, all columns are read as character vectors.

## New features

- Added (internal) input assertion functions, which declutter the input checking sections quite a bit.
- Added new (internal) `gtfsio_error()` function, which raises a custom-classed error condition. All errors raised in gtfsio's exported functions inherit from `gtfsio_error` and `<function_name>_error`, which allows for easier and more clear error catching.

## Potentially breaking changes

- Files and fields checking functions had their names changed. They now use the singular form, instead of the plural (i.e. `check_fields_exist()` was substituted by `check_field_exists()`). This shouldn't raise reverse dependencies concern, because as of now only `{gtfstools}` dev version uses such functions, which is an easy fix that doesn't concern CRAN release.

# gtfsio 0.2.0

## Bug fixes

- `import_gtfs()` would ocasionally include the full path to a table to their name in a GTFS object (#17). Fixed by Mark Padgham (@mapdge) in #18.

## New features

- Added new `summary.gtfs` method. Thanks Mark Padgham (@mapdge).
- `import_gtfs()` has a new `encoding` parameter, used to handle encoded strings in their native encoding.

# gtfsio 0.1.2

## Bug fixes

- Fixed a bug (or perhaps, changed the behaviour) of `check_fields_exist()` and `check_fields_types()` when a lower-level test (i.e. if the file that holds such field exists, for example) failed. Now it returns `FALSE` instead of raising an error.

# gtfsio 0.1.1

## Bug fixes

- Fixed a bug in `export_gtfs()` when `as_dir` was set to `TRUE` and `path` was set to `tempdir()` in #15. The function now returns an (intentional) error if `tempdir()` is passed to `path`. Thanks Flavio Poletti (@polettif).

## New features

- `import_gtfs()` has a new `skip` parameter. It may be used similarly to `files`, but you specify the files you *don't want* to read, instead of the ones you do. Thanks Flavio Poletti (@polettif).

# gtfsio 0.1.0

- First CRAN release!
