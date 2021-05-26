# gtfsio 0.1.1 

## Bug fixes

- Fixed a bug in `export_gtfs()` when `as_dir` was set to `TRUE` and `path` was set to `tempdir()` in #15. The function now returns an (intentional) error if `tempdir()` is passed to `path`. Thanks Flavio Poletti (@polettif).

## New features

- `import_gtfs()` has a new `skip` parameter. It may be used similarly to `files`, but you specify the files you *don't want* to read, instead of the ones you do. Thanks Flavio Poletti (@polettif).

# gtfsio 0.1.0

- First CRAN release!
