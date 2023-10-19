path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(path)
tmpf <- tempfile(fileext = ".zip")
tmpd <- tempfile()
gtfs_standards <- get_gtfs_standards()

tester <- function(gtfs_obj = gtfs,
                   path = tmpf,
                   files = NULL,
                   standard_only = FALSE,
                   compression_level = 9,
                   as_dir = FALSE,
                   overwrite = TRUE,
                   quiet = TRUE) {
  export_gtfs(
    gtfs_obj,
    path,
    files,
    standard_only,
    compression_level,
    as_dir,
    overwrite,
    quiet
  )
}


# basic input checks ------------------------------------------------------

expect_error(tester(unclass(gtfs)), class = "bad_gtfs_argument")
expect_error(tester(unclass(gtfs)), class = "export_gtfs_error")
expect_error(tester(path = factor(tmpf)), class = "bad_path_argument")
expect_error(tester(path = c(tmpf, tmpf)), class = "bad_path_argument")
expect_error(tester(files = NA), class = "bad_files_argument")
expect_error(tester(standard_only = NA), class = "bad_standard_only_argument")
expect_error(
  tester(standard_only = c(TRUE, TRUE)),
  class = "bad_standard_only_argument"
)
expect_error(
  tester(compression_level = c(1, 9)),
  class = "bad_compression_level_argument"
)
expect_error(
  tester(compression_level = "1"),
  class = "bad_compression_level_argument"
)
expect_error(tester(as_dir = NA), class = "bad_as_dir_argument")
expect_error(tester(as_dir = c(TRUE, TRUE)), class = "bad_as_dir_argument")
expect_error(tester(overwrite = NA), class = "bad_overwrite_argument")
expect_error(
  tester(overwrite = c(TRUE, TRUE)),
  class = "bad_overwrite_argument"
)
expect_error(tester(quiet = NA), class = "bad_quiet_argument")
expect_error(tester(quiet = c(TRUE, TRUE)), class = "bad_quiet_argument")


# elements inside '.' sub-list are not exported ---------------------------

dot_gtfs <- gtfs
dot_gtfs$. <- list(oi = data.frame(a = 1, b = 2))

tester()
expect_false("oi" %in% sub(".txt", "", zip::zip_list(tmpf)$filename))


# export_gtfs() returns 'gtfs' invisibly ----------------------------------

out <- capture.output(tester())
expect_identical(out, character(0))
expect_identical(gtfs, tester())
expect_identical(dot_gtfs, tester(dot_gtfs))


# 'as_dir' behaviour ------------------------------------------------------

# raise an error if path doesn't have '.zip' extension but 'as_dir' is FALSE

expect_error(
  tester(path = tmpd),
  pattern = paste0(
    "'path' must have '\\.zip' extension\\. ",
    "If you meant to create a directory please set 'as_dir' to TRUE\\."
  ),
  class = "ext_must_be_zip"
)

# raise an error if path has a '.zip' extension but 'as_dir' is TRUE

expect_error(
  tester(as_dir = TRUE),
  pattern = "'path' cannot have '\\.zip' extension\\ when 'as_dir' is TRUE\\.",
  class = "path_must_be_dir"
)

# raise an error if 'as_dir' is TRUE and 'path' is tempdir()

expect_error(
  tester(path = tempdir(), as_dir = TRUE),
  pattern = paste0(
    "Please use 'tempfile\\(\\)' instead of 'tempdir\\(\\)' to designate ",
    "temporary directories."
  ),
  class = "tempfile_misused"
)

# object should be exported as a zip file by default

tester()
expect_true(file.exists(tmpf))
expect_false(dir.exists(tmpf))

# and as a directory if 'as_dir' is TRUE

expect_false(dir.exists(tmpd))
tester(path = tmpd, as_dir = TRUE)
expect_true(dir.exists(tmpd))

# 'files' behaviour -------------------------------------------------------

# raise an error if a specified file is not an element of 'gtfs'

expect_error(
  tester(files = "oi"),
  pattern = paste0(
    "The provided GTFS object does not contain the following elements ",
    "specified in 'files': 'oi'"
  ),
  class = "missing_specified_file"
)

# if 'files' is NULL (the default), all elements should exported - both when
# final output is a zip file and when it's a directory

existing_files <- names(gtfs)

tester()
expect_identical(existing_files, sub(".txt", "", zip::zip_list(tmpf)$filename))

tester(path = tmpd, as_dir = TRUE)
expect_true(all(existing_files %in% sub(".txt", "", list.files(tmpd))))

# else, only specified files are exported

tester(files = "shapes")
expect_identical("shapes", sub(".txt", "", zip::zip_list(tmpf)$filename))

tester(path = tmpd, files = "shapes", as_dir = TRUE)
expect_identical("shapes", sub(".txt", "", list.files(tmpd)))


# 'standard_only' behaviour -----------------------------------------------

# raise an error if non-standard file was specified in 'files' when
# 'standard_only' is TRUE

expect_error(
  tester(files = "oi", standard_only = TRUE),
  pattern = paste0(
    "Non-standard file specified in 'files', even though 'standard_only' is ",
    "set to TRUE: 'oi'"
  ),
  class = "non_standard_files"
)

# if 'standard_only' is FALSE (the default), extra files and fields are exported

non_std_gtfs <- gtfs
non_std_gtfs$ola <- data.frame(oi = 2)

export_gtfs(non_std_gtfs, tmpd, as_dir = TRUE)
expect_true("ola" %in% sub(".txt", "", list.files(tmpd)))

levels_fields <- readLines(file.path(tmpd, "levels.txt"), n = 1L)
levels_fields <- strsplit(levels_fields, ",")[[1]]
expect_true("elevation" %in% levels_fields)

# else, only standard files and fields are written

export_gtfs(non_std_gtfs, tmpd, standard_only = TRUE, as_dir = TRUE)
expect_false("ola" %in% sub(".txt", "", list.files(tmpd)))
expect_true(
  all(
    setdiff(names(non_std_gtfs), "ola") %in% sub(".txt", "", list.files(tmpd))
  )
)

for (file in list.files(tmpd)) {

  # all existing fields should be standard

  no_txt_file     <- sub(".txt", "", file)
  std_fields      <- setdiff(names(gtfs_standards[[no_txt_file]]), "file_spec")
  existing_fields <- readLines(file.path(tmpd, file), n = 1L)
  existing_fields <- strsplit(existing_fields, ",")[[1]]

  expect_true(all(existing_fields %in% std_fields), info = no_txt_file)

  # all standard fields in the object should be written

  std_fields_in_obj <- names(gtfs[[no_txt_file]])
  std_fields_in_obj <- std_fields_in_obj[std_fields_in_obj %in% std_fields]

  expect_true(
    all(std_fields_in_obj %in% existing_fields),
    info = no_txt_file
  )

}


# 'compression_level' behaviour -------------------------------------------

tester(compression_level = 1)
big_file <- file.size(tmpf)
tester(compression_level = 9)
small_file <- file.size(tmpf)

expect_true(big_file > small_file)


# 'overwrite' behaviour ---------------------------------------------------

exist_pat <- paste0(
  "'path' points to an existing file/directory, ",
  "but 'overwrite' is set to FALSE\\."
)

expect_true(file.exists(tmpf))
expect_true(dir.exists(tmpd))
expect_error(
  tester(overwrite = FALSE),
  pattern = exist_pat,
  class = "cannot_overwrite_file"
)
expect_error(
  tester(path = tmpd, overwrite = FALSE),
  pattern = exist_pat,
  class = "cannot_overwrite_file"
)

# 'quiet' behaviour -------------------------------------------------------

expect_silent(tester())
expect_silent(tester(path = tmpd, as_dir = TRUE))

# as_dir = FALSE

expect_message(tester(quiet = FALSE))
out <- capture.output(tester(quiet = FALSE), type = "message")
expect_true(any(grepl("^Writing text files to ", out)))
expect_true(any(grepl("^  - Writing ", out)))
expect_true(any(grepl("^GTFS object successfully zipped to ", out)))

# as_dir = TRUE

expect_message(tester(path = tmpd, as_dir = TRUE, quiet = FALSE))
out <- capture.output(
  tester(path = tmpd, as_dir = TRUE, quiet = FALSE),
  type = "message"
)
expect_true(any(grepl("^Writing text files to ", out)))
expect_true(any(grepl("^  - Writing ", out)))

# making sure fwrite warnings are converted to messages

bad_path <- system.file("extdata/bad_gtfs.zip", package = "gtfsio")
expect_warning(bad_gtfs <- import_gtfs(bad_path, quiet = FALSE))
suppressWarnings(
  out <- capture.output(tester(bad_gtfs, quiet = FALSE), type = "message")
)
expect_true(any(grepl("^    - Input has no columns", out)))

# issue #34 ---------------------------------------------------------------

# export_gtfs() should not save large round numbers in scientific notation

mock_shapes <- data.frame(
  shape_id = c("a", "b", "c"),
  shape_pt_sequence = 1:3,
  shape_pt_lat = 40:42,
  shape_pt_lon = 40:42,
  shape_dist_traveled = c(1, 10000000, 10000001)
)

mock_gtfs <- list(shapes = mock_shapes)
mock_gtfs <- new_gtfs(mock_gtfs)

target_dir <- tempfile()
export_gtfs(mock_gtfs, target_dir, as_dir = TRUE)

resulting_shapes_content <- readLines(file.path(target_dir, "shapes.txt"))

expect_false(identical(resulting_shapes_content[3], "b,2,41,41,1e+07"))
expect_identical(resulting_shapes_content[3], "b,2,41,41,10000000")
