gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


# check_file_exists() -----------------------------------------------------

# input checking

expect_error(
  check_file_exists(unclass(gtfs), "shapes"),
  class = "bad_x_argument"
)
expect_error(
  check_file_exists(unclass(gtfs), "shapes"),
  class = "check_file_exists_error"
)
expect_error(
  check_file_exists(gtfs, factor("shapes")),
  class = "bad_files_argument"
)

# expected results

expect_true(check_file_exists(gtfs, "shapes"))
expect_true(check_file_exists(gtfs, c("shapes", "trips")))
expect_false(check_file_exists(gtfs, "ola"))
expect_false(check_file_exists(gtfs, c("shapes", "ola")))


# assert_file_exists() ----------------------------------------------------

# input checking

expect_error(
  assert_file_exists(unclass(gtfs), "shapes"),
  class = "bad_x_argument"
)
expect_error(
  assert_file_exists(gtfs, factor("shapes")),
  class = "bad_files_argument"
)

# expected results

expect_identical(assert_file_exists(gtfs, "shapes"), gtfs)
expect_identical(assert_file_exists(gtfs, c("shapes", "trips")), gtfs)
expect_silent(assert_file_exists(gtfs, "shapes"))
expect_silent(assert_file_exists(gtfs, c("shapes", "trips")))

expect_error(
  assert_file_exists(gtfs, "ola"),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'ola\\'"
  ),
  class = "missing_required_file"
)
expect_error(
  assert_file_exists(gtfs, c("shapes", "ola")),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'ola\\'"
  )
)
expect_error(
  assert_file_exists(gtfs, c("oi", "ola")),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'oi\\', \\'ola\\'"
  )
)


# check_field_exists() ----------------------------------------------------

# input checking

expect_error(
  check_field_exists(unclass(gtfs), "shapes", "shape_id"),
  class = "bad_x_argument"
)
expect_error(
  check_field_exists(gtfs, factor("shapes"), "shape_id"),
  class = "bad_file_argument"
)
expect_error(
  check_field_exists(gtfs, c("trips", "shapes"), "shape_id"),
  class = "bad_file_argument"
)
expect_error(
  check_field_exists(gtfs, "shapes", factor("shape_id")),
  class = "bad_fields_argument"
)

# if 'file' doesn't exist, expect FALSE

expect_false(check_field_exists(gtfs, "oi", "shape_id"))

# expected results

expect_true(check_field_exists(gtfs, "shapes", "shape_id"))
expect_true(check_field_exists(gtfs, "shapes", c("shape_id", "shape_pt_lat")))
expect_false(check_field_exists(gtfs, "shapes", "oi"))
expect_false(check_field_exists(gtfs, "shapes", c("shape_id", "oi")))


# assert_field_exists() ---------------------------------------------------

# input checking

expect_error(
  assert_field_exists(unclass(gtfs), "shapes", "shape_id"),
  class = "bad_x_argument"
)
expect_error(
  assert_field_exists(gtfs, factor("shapes"), "shape_id"),
  class = "bad_file_argument"
)
expect_error(
  assert_field_exists(gtfs, c("trips", "shapes"), "shape_id"),
  class = "bad_file_argument"
)
expect_error(
  assert_field_exists(gtfs, "shapes", factor("shape_id")),
  class = "bad_fields_argument"
)

# 'file' must exist

expect_error(
  assert_field_exists(gtfs, "oi", "shape_id"),
  class = "missing_required_file"
)

# expected results

expect_identical(assert_field_exists(gtfs, "shapes", "shape_id"), gtfs)
expect_identical(
  assert_field_exists(gtfs, "shapes", c("shape_id", "shape_pt_lat")),
  gtfs
)
expect_silent(assert_field_exists(gtfs, "shapes", "shape_id"))
expect_silent(
  assert_field_exists(gtfs, "shapes", c("shape_id", "shape_pt_lat"))
)

expect_error(
  assert_field_exists(gtfs, "shapes", c("oi")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\'"
  ),
  class = "missing_required_field"
)
expect_error(
  assert_field_exists(gtfs, "shapes", c("shape_id", "oi")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\'"
  )
)
expect_error(
  assert_field_exists(gtfs, "shapes", c("oi", "ola")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\', \\'ola\\'"
  )
)


# check_field_class() ----------------------------------------------------

# input checking

expect_error(
  check_field_class(
    unclass(gtfs),
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_x_argument"
)
expect_error(
  check_field_class(
    gtfs,
    c("shapes", "trips"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_file_argument"
)
expect_error(
  check_field_class(
    gtfs,
    factor("shapes"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_file_argument"
)
expect_error(
  check_field_class(
    gtfs,
    "shapes",
    factor("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_fields_argument"
)
expect_error(
  check_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    factor("character", "numeric")
  ),
  class = "bad_classes_argument"
)
expect_error(
  check_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character")
  ),
  class = "bad_classes_argument"
)

# if 'fields' doesn't exist, expect FALSE

expect_false(check_field_class(gtfs, "shapes", "oi", "character"))

# expected results

expect_true(check_field_class(gtfs, "shapes", "shape_id", "character"))
expect_true(
  check_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  )
)
expect_false(check_field_class(gtfs, "shapes", "shape_id", "numeric"))
expect_false(
  check_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    rep("character", 2)
  )
)


# assert_field_class() ---------------------------------------------------

# input checking

expect_error(
  assert_field_class(
    unclass(gtfs),
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_x_argument"
)
expect_error(
  assert_field_class(
    gtfs,
    c("shapes", "trips"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_file_argument"
)
expect_error(
  assert_field_class(
    gtfs,
    factor("shapes"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_file_argument"
)
expect_error(
  assert_field_class(
    gtfs,
    "shapes",
    factor("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  class = "bad_fields_argument"
)
expect_error(
  assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    factor("character", "numeric")
  ),
  class = "bad_classes_argument"
)
expect_error(
  assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character")
  ),
  class = "bad_classes_argument"
)

# 'fields' must exist

expect_error(
  assert_field_class(gtfs, "shapes", "oi", "character"),
  class = "missing_required_field"
)

# expected results

expect_identical(
  assert_field_class(gtfs, "shapes", "shape_id", "character"),
  gtfs
)
expect_identical(
  assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  gtfs
)
expect_silent(
  assert_field_class(gtfs, "shapes", "shape_id", "character")
)
expect_silent(
  assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  )
)

expect_error(
  assert_field_class(gtfs, "shapes", "shape_id", "numeric"),
  pattern = paste0(
    "The following columns in the GTFS object \\'shapes\\' element do not ",
    "inherit from the required classes:\n",
    "  - \\'shape_id\\': requires numeric, but inherits from character"
  )
)
expect_error(
  assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    rep("integer", 2)
  ),
  pattern = paste0(
    "The following columns in the GTFS object \\'shapes\\' element do not ",
    "inherit from the required classes:\n",
    "  - \\'shape_id\\': requires integer, but inherits from character\n",
    "  - \\'shape_pt_lat\\': requires integer, but inherits from numeric"
  )
)
