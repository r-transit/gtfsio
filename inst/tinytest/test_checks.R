gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


# check_files_exist() -----------------------------------------------------

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


# assert_files_exist() ----------------------------------------------------

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


# check_fields_exist() ----------------------------------------------------

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


# assert_fields_exist() ---------------------------------------------------

# input checking

expect_error(
  assert_fields_exist(unclass(gtfs), "shapes", "shape_id"),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  assert_fields_exist(gtfs, factor("shapes"), "shape_id"),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  assert_fields_exist(gtfs, c("trips", "shapes"), "shape_id"),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  assert_fields_exist(gtfs, "shapes", factor("shape_id")),
  pattern = "\\'fields\\' must be a character vector\\."
)

# 'file' must exist

expect_error(
  assert_fields_exist(gtfs, "oi", "shape_id"),
  pattern = paste0(
    "The GTFS object is missing the following ",
    "required element\\(s\\): \\'oi\\'"
  )
)

# expected results

expect_identical(assert_fields_exist(gtfs, "shapes", "shape_id"), gtfs)
expect_identical(
  assert_fields_exist(gtfs, "shapes", c("shape_id", "shape_pt_lat")),
  gtfs
)
expect_silent(assert_fields_exist(gtfs, "shapes", "shape_id"))
expect_silent(
  assert_fields_exist(gtfs, "shapes", c("shape_id", "shape_pt_lat"))
)

expect_error(
  assert_fields_exist(gtfs, "shapes", c("oi")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\'"
  )
)
expect_error(
  assert_fields_exist(gtfs, "shapes", c("shape_id", "oi")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\'"
  )
)
expect_error(
  assert_fields_exist(gtfs, "shapes", c("oi", "ola")),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following required ",
    "column\\(s\\): \\'oi\\', \\'ola\\'"
  )
)


# check_fields_types() ----------------------------------------------------

# input checking

expect_error(
  check_fields_types(
    unclass(gtfs),
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  check_fields_types(
    gtfs,
    c("shapes", "trips"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  check_fields_types(
    gtfs,
    factor("shapes"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  check_fields_types(
    gtfs,
    "shapes",
    factor("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = "\\'fields\\' must be a character vector\\."
)
expect_error(
  check_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    factor("character", "numeric")
  ),
  pattern = paste0(
    "\\'types\\' must be a character vector with ",
    "the same length of \\'fields\\'\\."
  )
)
expect_error(
  check_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character")
  ),
  pattern = paste0(
    "\\'types\\' must be a character vector with ",
    "the same length of \\'fields\\'\\."
  )
)

# 'fields' must exist

expect_error(
  check_fields_types(gtfs, "shapes", "oi", "character"),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following ",
    "required column\\(s\\): \\'oi\\'"
  )
)

# expected results

expect_true(check_fields_types(gtfs, "shapes", "shape_id", "character"))
expect_true(
  check_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  )
)
expect_false(check_fields_types(gtfs, "shapes", "shape_id", "numeric"))
expect_false(
  check_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    rep("character", 2)
  )
)


# assert_fields_types() ---------------------------------------------------

# input checking

expect_error(
  assert_fields_types(
    unclass(gtfs),
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = "\\'x\\' must inherit from the \\'gtfs\\' class\\."
)
expect_error(
  assert_fields_types(
    gtfs,
    c("shapes", "trips"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  assert_fields_types(
    gtfs,
    factor("shapes"),
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = paste0(
    "\\'file\\' must be a string ",
    "\\(a character vector of length 1\\)\\."
  )
)
expect_error(
  assert_fields_types(
    gtfs,
    "shapes",
    factor("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  pattern = "\\'fields\\' must be a character vector\\."
)
expect_error(
  assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    factor("character", "numeric")
  ),
  pattern = paste0(
    "\\'types\\' must be a character vector with ",
    "the same length of \\'fields\\'\\."
  )
)
expect_error(
  assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character")
  ),
  pattern = paste0(
    "\\'types\\' must be a character vector with ",
    "the same length of \\'fields\\'\\."
  )
)

# 'fields' must exist

expect_error(
  assert_fields_types(gtfs, "shapes", "oi", "character"),
  pattern = paste0(
    "The GTFS object \\'shapes\\' element is missing the following ",
    "required column\\(s\\): \\'oi\\'"
  )
)

# expected results

expect_identical(
  assert_fields_types(gtfs, "shapes", "shape_id", "character"),
  gtfs
)
expect_identical(
  assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  ),
  gtfs
)
expect_silent(
  assert_fields_types(gtfs, "shapes", "shape_id", "character")
)
expect_silent(
  assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    c("character", "numeric")
  )
)

expect_error(
  assert_fields_types(gtfs, "shapes", "shape_id", "numeric"),
  pattern = paste0(
    "The following columns in the GTFS object \\'shapes\\' element do not ",
    "inherit from the required types:\n",
    "  - \\'shape_id\\': requires numeric, but inherits from character"
  )
)
expect_error(
  assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat"),
    rep("integer", 2)
  ),
  pattern = paste0(
    "The following columns in the GTFS object \\'shapes\\' element do not ",
    "inherit from the required types:\n",
    "  - \\'shape_id\\': requires integer, but inherits from character\n",
    "  - \\'shape_pt_lat\\': requires integer, but inherits from numeric"
  )
)
