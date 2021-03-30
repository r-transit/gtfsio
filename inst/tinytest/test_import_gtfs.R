path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
url<-"https://github.com/r-transit/gtfsio/raw/master/inst/extdata/ggl_gtfs.zip"


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


# 'files' behaviour -------------------------------------------------------

# raise error if file specified in 'files' doesn't exist

expect_error(
  import_gtfs(path, files = "ola"),
  pattern = paste0(
    "The provided GTFS feed doesn't contain the following ",
    "text file\\(s\\): 'ola'"
  )
)

# if 'files' is NULL (the default), all existing files are read

existing_files <- zip::zip_list(path)$filename
existing_files <- sub(".txt", "", existing_files)

gtfs <- import_gtfs(path)

expect_identical(names(gtfs), existing_files)

# if it's not, read only the specified files

gtfs <- import_gtfs(path, files = c("shapes", "trips"))
expect_identical(names(gtfs), c("shapes", "trips"))


# 'fields' behaviour ------------------------------------------------------

# raise error if file specified in 'fields' either does not exist or wasn't
# specified in 'files'

expect_error(
  import_gtfs(path, fields = list(oi = "ola")),
  pattern = paste0(
    "The following files were specified in 'fields' but either were not ",
    "specified in 'files' or do not exist: 'oi'"
  )
)

expect_error(
  import_gtfs(path, files = "shapes", fields = list(trips = "trip_id")),
  pattern = paste0(
    "The following files were specified in 'fields' but either were not ",
    "specified in 'files' or do not exist: 'trips'"
  )
)

# if 'fields' is NULL (the default), all fields from all files are read

tmpd <- tempfile("gtfsio_test")
zip::unzip(path, exdir = tmpd)

existing_fields <- lapply(
  list.files(tmpd),
  function(file) {
    header <- readLines(con = file.path(tmpd, file), n = 1L)
    fields <- unlist(strsplit(header, ","))
    fields <- sub("^ ", "", fields)
  }
)
names(existing_fields) <- sub(".txt", "", list.files(tmpd))
existing_fields <- existing_fields[order(names(existing_fields))]

gtfs        <- import_gtfs(path)
gtfs_fields <- lapply(gtfs, names)
gtfs_fields <- gtfs_fields[order(names(gtfs_fields))]

expect_identical(existing_fields, gtfs_fields)

# if 'fields' is not NULL, read only the specified fields

gtfs <- import_gtfs(
  path,
  files = c("shapes", "trips"),
  fields = list(shapes = "shape_id", trips = "trip_id")
)
gtfs_fields <- lapply(gtfs, names)

expect_identical(gtfs_fields, list(shapes = "shape_id", trips = "trip_id"))


# fields formatted according to the standards -----------------------------

# get the standard type in R used to read each field

gtfs_standards <- get_gtfs_standards()

standard_types <- lapply(
  gtfs_standards,
  function(file) {
    fields <- setdiff(names(file), "file_spec")
    types  <- vapply(fields, function(f) file[[f]][[1]], character(1))
    types  <- types[order(names(types))]
  }
)
standard_types <- standard_types[order(names(standard_types))]

# get the type actually used to read each field

gtfs <- import_gtfs(path)

actual_types <- lapply(
  gtfs,
  function(file) {
    types <- vapply(file, class, character(1))
    types <- types[order(names(types))]
  }
)
actual_types <- actual_types[order(names(actual_types))]

# remove 'elevation' from 'levels' in 'actual_types', because this is not a
# standard field

actual_types$levels <- actual_types$levels[2:4]

# remove fields not present in 'actual_types' from 'standard_types'

prev_names <- names(standard_types)
standard_types <- lapply(
  names(standard_types),
  function(file) standard_types[[file]][names(actual_types[[file]])]
)
names(standard_types) <- prev_names

expect_identical(standard_types, actual_types)


# 'extra_spec' behaviour --------------------------------------------------



# 'quiet' behaviour -------------------------------------------------------

expect_silent(import_gtfs(path))
expect_silent(import_gtfs(url))
expect_message(import_gtfs(path, quiet = FALSE))
expect_message(import_gtfs(url, quiet = FALSE))
