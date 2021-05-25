path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(path)
tmpf <- tempfile(fileext = ".zip")
tmpd <- tempfile()
gtfs_standards <- get_gtfs_standards()


# basic input checks ------------------------------------------------------

expect_error(
  export_gtfs(unclass(gtfs), tmpf),
  pattern = "'gtfs' must inherit from the 'gtfs' class\\."
)

path_pat <- "'path' must be a string \\(a character vector of length 1\\)\\."
expect_error(export_gtfs(gtfs, factor(tmpf)), pattern = path_pat)
expect_error(export_gtfs(gtfs, c(tmpf, tmpf)), pattern = path_pat)

expect_error(
  export_gtfs(gtfs, tmpf, files = NA),
  pattern = "'files' must either be a character vector or NULL\\."
)

std_pat <- "'standard_only' must be a logical vector of length 1\\."
expect_error(
  export_gtfs(gtfs, tmpf, standard_only = "TRUE"),
  pattern = std_pat
)
expect_error(
  export_gtfs(gtfs, tmpf, standard_only = c(TRUE, TRUE)),
  pattern = std_pat
)

comp_pat <- "'compression_level' must be a numeric vector of length 1\\."
expect_error(
  export_gtfs(gtfs, tmpf, compression_level = "1"),
  pattern = comp_pat
)
expect_error(
  export_gtfs(gtfs, tmpf, compression_level = c(1, 9)),
  pattern = comp_pat
)

dir_pat <- "'as_dir' must be a logical vector of length 1\\."
expect_error(
  export_gtfs(gtfs, tmpf, as_dir = "TRUE"),
  pattern = dir_pat
)
expect_error(
  export_gtfs(gtfs, tmpf, as_dir = c(TRUE, TRUE)),
  pattern = dir_pat
)

ovwt_pat <- "'overwrite' must be a logical vector of length 1\\."
expect_error(
  export_gtfs(gtfs, tmpf, overwrite = "TRUE"),
  pattern = ovwt_pat
)
expect_error(
  export_gtfs(gtfs, tmpf, overwrite = c(TRUE, TRUE)),
  pattern = ovwt_pat
)

quiet_pat <- "'quiet' must be a logical vector of length 1\\."
expect_error(
  export_gtfs(gtfs, tmpf, quiet = "TRUE"),
  pattern = quiet_pat
)
expect_error(
  export_gtfs(gtfs, tmpf, quiet = c(TRUE, TRUE)),
  pattern = quiet_pat
)


# elements inside '.' sub-list are not exported ---------------------------

dot_gtfs <- gtfs
dot_gtfs$. <- list(oi = data.frame(a = 1, b = 2))

export_gtfs(gtfs, tmpf)
expect_false("oi" %in% sub(".txt", "", zip::zip_list(tmpf)$filename))


# export_gtfs() returns 'gtfs' invisibly ----------------------------------

out <- capture.output(export_gtfs(gtfs, tmpf))
expect_identical(out, character(0))
expect_identical(gtfs, export_gtfs(gtfs, tmpf))
expect_identical(dot_gtfs, export_gtfs(dot_gtfs, tmpf))


# 'as_dir' behaviour ------------------------------------------------------

# raise an error if path doesn't have '.zip' extension but 'as_dir' is FALSE

expect_error(
  export_gtfs(gtfs, tmpd),
  pattern = paste0(
    "'path' must have '\\.zip' extension\\. ",
    "If you meant to create a directory please set 'as_dir' to TRUE\\."
  )
)

# raise an error if path has a '.zip' extension but 'as_dir' is TRUE

expect_error(
  export_gtfs(gtfs, tmpf, as_dir = TRUE),
  pattern = "'path' cannot have '\\.zip' extension\\ when 'as_dir' is TRUE\\."
)

# raise an error if 'as_dir' is TRUE and 'path' is tempdir()

expect_error(
  export_gtfs(gtfs, tempdir(), as_dir = TRUE),
  pattern = paste0(
    "Please use 'tempfile\\(\\)' instead of 'tempdir\\(\\)' to designate ",
    "temporary directories."
  )
)

# object should be exported as a zip file by default

export_gtfs(gtfs, tmpf)
expect_true(file.exists(tmpf))
expect_false(dir.exists(tmpf))

# and as a directory if 'as_dir' is TRUE

expect_false(dir.exists(tmpd))
export_gtfs(gtfs, tmpd, as_dir = TRUE)
expect_true(dir.exists(tmpd))

# 'files' behaviour -------------------------------------------------------

# raise an error if a specified file is not an element of 'gtfs'

expect_error(
  export_gtfs(gtfs, tmpf, files = "oi"),
  pattern = paste0(
    "The provided GTFS object does not contain the following elements ",
    "specified in 'files': 'oi'"
  )
)

# if 'files' is NULL (the default), all elements should exported - both when
# final output is a zip file and when it's a directory

existing_files <- names(gtfs)

export_gtfs(gtfs, tmpf)
expect_identical(existing_files, sub(".txt", "", zip::zip_list(tmpf)$filename))

export_gtfs(gtfs, tmpd, as_dir = TRUE)
expect_true(all(existing_files %in% sub(".txt", "", list.files(tmpd))))

# else, only specified files are exported

export_gtfs(gtfs, tmpf, files = "shapes")
expect_identical("shapes", sub(".txt", "", zip::zip_list(tmpf)$filename))

export_gtfs(gtfs, tmpd, files = "shapes", as_dir = TRUE)
expect_identical("shapes", sub(".txt", "", list.files(tmpd)))


# 'standard_only' behaviour -----------------------------------------------

# raise an error if non-standard file was specified in 'files' when
# 'standard_only' is TRUE

expect_error(
  export_gtfs(gtfs, tmpf, files = "oi", standard_only = TRUE),
  pattern = paste0(
    "Non-standard file specified in 'files', even though 'standard_only' is ",
    "set to TRUE: 'oi'"
  )
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

export_gtfs(gtfs, tmpf, compression_level = 1)
big_file <- file.size(tmpf)
export_gtfs(gtfs, tmpf, compression_level = 9)
small_file <- file.size(tmpf)

expect_true(big_file > small_file)


# 'overwrite' behaviour ---------------------------------------------------

exist_pat <- paste0(
  "'path' points to an existing file/directory, ",
  "but 'overwrite' is set to FALSE\\."
)

expect_true(file.exists(tmpf))
expect_true(dir.exists(tmpd))
expect_error(export_gtfs(gtfs, tmpf, overwrite = FALSE), pattern = exist_pat)
expect_error(export_gtfs(gtfs, tmpd, overwrite = FALSE), pattern = exist_pat)

# 'quiet' behaviour -------------------------------------------------------

expect_silent(export_gtfs(gtfs, tmpf))
expect_silent(export_gtfs(gtfs, tmpd, as_dir = TRUE))

# as_dir = FALSE

expect_message(export_gtfs(gtfs, tmpf, quiet = FALSE))
out <- capture.output(export_gtfs(gtfs, tmpf, quiet = FALSE), type = "message")
expect_true(any(grepl("^Writing text files to ", out)))
expect_true(any(grepl("^  - Writing ", out)))
expect_true(any(grepl("^GTFS object successfully zipped to ", out)))

# as_dir = TRUE

expect_message(export_gtfs(gtfs, tmpd, as_dir = TRUE, quiet = FALSE))
out <- capture.output(
  export_gtfs(gtfs, tmpd, as_dir = TRUE, quiet = FALSE),
  type = "message"
)
expect_true(any(grepl("^Writing text files to ", out)))
expect_true(any(grepl("^  - Writing ", out)))
