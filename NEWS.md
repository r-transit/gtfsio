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
