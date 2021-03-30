path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")


# basic input checking ----------------------------------------------------

path_type_pat <- "'path' must be a character vector of length 1\\."
expect_error(import_gtfs(factor(path)), pattern = path_type_pat)
expect_error(import_gtfs(c(path, path)), pattern = path_type_pat)

expect_error(
  import_gtfs(sub(".zip", "", path)),
  pattern = "'path' must have '\\.zip' extension\\."
)

missing_path <- sub("ggl", "", path)
expect_error(
  import_gtfs(missing_path),
  pattern = paste0("'path' points to non-existent file: '", missing_path, "'")
)

quiet_pat <- "'quiet' must be a logical vector of length 1\\."
expect_error(import_gtfs(path, quiet = "TRUE"), pattern = quiet_pat)
expect_error(import_gtfs(path, quiet = c(TRUE, TRUE)), pattern = quiet_pat)

expect_error(
  import_gtfs(path, extra_spec = NA),
  pattern = "'extra_spec' must be either a list or NULL\\."
)

expect_error(
  import_gtfs(path, extra_spec = list(levels = (elevation = "factor"))),
  pattern = paste0(
    "Only character, integer and numeric ",
    "are supported in 'extra_spec'\\."
  )
)

expect_error(
  import_gtfs(path, files = NA),
  pattern = "'files' must be either a character vector or NULL\\."
)

expect_error(
  import_gtfs(path, fields = NA),
  pattern = "'fields' must be either a list or NULL\\."
)
