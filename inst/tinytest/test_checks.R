# tests setup

gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


##### CHECK_FILES_EXIST

# input checking

expect_error(
  check_files_exist(unclass(gtfs), "shapes"),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  check_files_exist(gtfs, factor("shapes")),
  pattern = "\\'files\\' must be a character vector\\."
)

# expected results

expect_true(check_files_exist(gtfs, "shapes"))
expect_true(check_files_exist(gtfs, c("shapes", "trips")))
expect_false(check_files_exist(gtfs, "ola"))
expect_false(check_files_exist(gtfs, c("shapes", "ola")))


##### ASSERT_FILES_EXIST

# input checking

expect_error(
  assert_files_exist(unclass(gtfs), "shapes"),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  assert_files_exist(gtfs, factor("shapes")),
  pattern = "\\'files\\' must be a character vector\\."
)

# expected results

expect_identical(assert_files_exist(gtfs, "shapes"), gtfs)
expect_identical(assert_files_exist(gtfs, c("shapes", "trips")), gtfs)
expect_silent(assert_files_exist(gtfs, "shapes"))
expect_silent(assert_files_exist(gtfs, c("shapes", "trips")))

expect_error(
  assert_files_exist(gtfs, "ola"),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'ola\\'"
  )
)
expect_error(
  assert_files_exist(gtfs, c("shapes", "ola")),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'ola\\'"
  )
)
expect_error(
  assert_files_exist(gtfs, c("oi", "ola")),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'oi\\', \\'ola\\'"
  )
)


##### CHECK_FIELDS_EXIST

# input checking

expect_error(
  check_fields_exist(unclass(gtfs), "shapes", "shape_id"),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  check_fields_exist(gtfs, factor("shapes"), "shape_id"),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  check_fields_exist(gtfs, c("trips", "shapes"), "shape_id"),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  check_fields_exist(gtfs, "shapes", factor("shape_id")),
  pattern = "\\'fields\\' must be a character vector\\."
)

# 'file' must exist

expect_error(
  check_fields_exist(gtfs, "oi", "shape_id"),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'oi\\'"
  )
)

# expected results

expect_true(check_fields_exist(gtfs, "shapes", "shape_id"))
expect_true(check_fields_exist(gtfs, "shapes", c("shape_id", "shape_pt_lat")))
expect_false(check_fields_exist(gtfs, "shapes", "oi"))
expect_false(check_fields_exist(gtfs, "shapes", c("shape_id", "oi")))
