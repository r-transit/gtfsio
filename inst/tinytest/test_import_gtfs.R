path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
url<-"https://github.com/r-transit/gtfsio/raw/master/inst/extdata/ggl_gtfs.zip"

tester <- function(data_path = path,
                   files = NULL,
                   fields = NULL,
                   extra_spec = NULL,
                   skip = NULL,
                   quiet = TRUE,
                   encoding = "unknown") {

  import_gtfs(data_path, files, fields, extra_spec, skip, quiet, encoding)

}

# basic input checking ----------------------------------------------------

a_list <- list(1)

expect_error(tester(factor(path)), class = "bad_path_argument")
expect_error(tester(c(path, path)), class = "bad_path_argument")
expect_error(tester(c(path, path)), class = "import_gtfs_error")
expect_error(
  tester(sub(".zip", "", path)),
  pattern = "'path' must have '\\.zip' extension\\."
)
expect_error(tester(quiet = "TRUE"), class = "bad_quiet_argument")
expect_error(tester(quiet = rep(TRUE, 2)), class = "bad_quiet_argument")
expect_error(tester(extra_spec = NA), class = "bad_extra_spec_argument")
expect_error(tester(extra_spec = NA), class = "import_gtfs_error")
expect_error(tester(extra_spec = a_list), class = "bad_extra_spec_argument")
expect_error(
  tester(extra_spec = list(levels = c(elevation = "factor"))),
  pattern = paste0(
    "Only character, integer and numeric ",
    "are supported in 'extra_spec'\\."
  )
)
expect_error(tester(files = NA), class = "bad_files_argument")
expect_error(tester(fields = NA), class = "bad_fields_argument")
expect_error(tester(skip = NA), class = "bad_skip_argument")
expect_error(tester(encoding = ""), class = "bad_encoding_argument")
expect_error(tester(encoding = TRUE), class = "bad_encoding_argument")
expect_error(
  tester(encoding = c("unknown", "UTF-8")),
  class = "bad_encoding_argument"
)

missing_path <- sub("ggl", "", path)
expect_error(
  tester(missing_path),
  pattern = paste0("'path' points to non-existent file: '", missing_path, "'")
)


# 'files' behaviour -------------------------------------------------------

# raise error if file specified in 'files' doesn't exist

expect_error(
  tester(files = "ola"),
  pattern = paste0(
    "The provided GTFS feed doesn't contain the following ",
    "text file\\(s\\): 'ola'"
  ),
  class = "gtfs_missing_files"
)

# if 'files' is NULL (the default), all existing files are read

existing_files <- zip::zip_list(path)$filename
existing_files <- sub(".txt", "", existing_files)

gtfs <- import_gtfs(path)

expect_identical(names(gtfs), existing_files)

# if it's not, read only the specified files

gtfs <- tester(files = c("shapes", "trips"))
expect_identical(names(gtfs), c("shapes", "trips"))


# 'fields' behaviour ------------------------------------------------------

# raise error if file specified in 'fields' either does not exist or wasn't
# specified in 'files'

expect_error(
  tester(fields = list(oi = "ola")),
  pattern = paste0(
    "The following files were specified in 'fields' but either were not ",
    "specified in 'files' or do not exist: 'oi'"
  ),
  class = "files_misspecified"
)

expect_error(
  tester(files = "shapes", fields = list(trips = "trip_id")),
  pattern = paste0(
    "The following files were specified in 'fields' but either were not ",
    "specified in 'files' or do not exist: 'trips'"
  ),
  class = "files_misspecified"
)

# raise error if field is specified in 'fields' but doesn't exist

expect_error(
  tester(fields = list(shapes = "ola")),
  pattern = "'shapes' doesn't contain the following field\\(s\\): 'ola'",
  class = "gtfs_missing_fields"
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

# raise an error if a field was specified in 'extra_spec' but its format is
# already specified in the standards

expect_error(
  tester(extra_spec = list(shapes = c(shape_id = "integer"))),
  pattern = paste0(
    "The following field\\(s\\) from the 'shapes' file were specified in ",
    "'extra_spec' but are already documented in the official GTFS reference: ",
    "'shape_id'"
  ),
  class = "field_is_documented"
)

# raise an error if a field was specified in 'extra_spec' but either does not
# exist or was not specified in 'fields'

expect_error(
  tester(extra_spec = list(shapes = c(ola = "character"))),
  pattern = paste0(
    "The following fields were specified in 'extra_spec' but either were not ",
    "specified in 'fields' or do not exist: 'ola'"
  ),
  class = "gtfs_fields_misspec"
)

expect_error(
  import_gtfs(
    path,
    fields = list(levels = "level_id"),
    extra_spec = list(levels = c(elevation = "character"))
  ),
  pattern = paste0(
    "The following fields were specified in 'extra_spec' but either were not ",
    "specified in 'fields' or do not exist: 'elevation'"
  ),
  class = "gtfs_fields_misspec"
)

# if 'extra_spec' id NULL (default), extra fields should be read as character

gtfs <- import_gtfs(path)

expect_true(class(gtfs$levels$elevation) == "character")

# else, fields should be read as specified

gtfs <- tester(extra_spec = list(levels = c(elevation = "integer")))

expect_true(class(gtfs$levels$elevation) == "integer")


# output should be a 'gtfs' object composed by 'data.table's --------------

gtfs <- import_gtfs(path)

expect_inherits(gtfs, "gtfs")

expect_true(
  all(vapply(gtfs, function(i) inherits(i, "data.table"), logical(1)))
)


# empty file should be read as a NULL data.table --------------------------

bad_path <- system.file("extdata/bad_gtfs.zip", package = "gtfsio")

expect_warning(bad_gtfs <- import_gtfs(bad_path, files = "agency"))

expect_inherits(bad_gtfs$agency, "data.table")
expect_true(ncol(bad_gtfs$agency) == 0)


# 'quiet' behaviour -------------------------------------------------------

# silent both when reading from local path and url

expect_silent(import_gtfs(path))
expect_silent(import_gtfs(url))

# loud when quiet = FALSE

expect_message(tester(quiet = FALSE))
out <- capture.output(g <- tester(quiet = FALSE), type = "message")
expect_true(any(grepl("^Unzipped the following files to ", out)))
expect_true(any(grepl("^  \\*", out)))
expect_true(any(grepl("^Reading ", out)))

# message when reading from url

out <- capture.output(g <- import_gtfs(url, quiet = FALSE), type = "message")
expect_true(any(grepl("^File downloaded to ", out)))

# message when reading undocumented file

out <- capture.output(
  g <- import_gtfs(bad_path, files = "trips_bad_name", quiet = FALSE),
  type = "message"
)
expect_true(any(grepl("^  - File undocumented\\.", out)))

# message when reading empty file

suppressWarnings(
  out <- capture.output(
    g <- import_gtfs(bad_path, files = "agency", quiet = FALSE),
    type = "message"
  )
)
expect_true(any(grepl("^  - File is empty\\.", out)))

# warnings converted to messages upon parsing failures

suppressWarnings(
  out <- capture.output(
    g <- import_gtfs(bad_path, files = "fare_rules", quiet = FALSE),
    type = "message"
  )
)
expect_true(any(grepl("^  - Stopped early on line.", out)))

suppressWarnings(
  out <- capture.output(
    g <- import_gtfs(bad_path, files = "stop_times", quiet = FALSE),
    type = "message"
  )
)
expect_true(any(grepl("^  - Discarded single-line footer", out)))


# 'skip' behaviour -------------------------------------------------------

# skip the specified text files

g_all <- import_gtfs(path)
g_skipped <- tester(skip = c("transfers", "translations"))

expect_equal(
  setdiff(names(g_all), names(g_skipped)),
  c("transfers", "translations")
)

# raise error if both 'files' and 'skip' are not NULL

expect_error(
  tester(files = "dummy1", skip = "dummy2"),
  pattern = paste0(
    "Both 'files' and 'skip' were provided\\. ",
    "Please use only one of these parameters at a time\\."
  ),
  class = "files_and_skip_provided"
)


# 'encoding' behaviour ----------------------------------------------------

gtfs <- import_gtfs(path)

gtfs$agency <- rbind(
  gtfs$agency,
  data.table::data.table(
    agency_id = "agência",
    agency_name = "Agência do busão",
    agency_url = "https://www.agencia.com/",
    agency_timezone = "UTC",
    agency_lang = "pt"
  )
)

tmpf <- tempfile("import_gtfs_test", fileext = ".zip")
export_gtfs(gtfs, tmpf)

gtfs_utf8 <- import_gtfs(tmpf, files = "agency", encoding = "UTF-8")
gtfs_latin1 <- import_gtfs(tmpf, files = "agency", encoding = "Latin-1")

expect_false(identical(gtfs_utf8$agency, gtfs_latin1$agency))
