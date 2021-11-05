gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


# checks ------------------------------------------------------------------

# 'x' must be a named list

no_names        <- gtfs
names(no_names) <- NULL

expect_error(
  assert_gtfs(no_names),
  pattern = "Every element in a GTFS object must be named\\.",
  class = "gtfs_not_fully_named"
)
expect_error(assert_gtfs(no_names), class = "assert_gtfs_error")

# every single element of 'x' must be named

missing_one_name <- gtfs
missing_one_name[[18]] <- data.frame(a = 1)
expect_error(assert_gtfs(missing_one_name), class = "gtfs_not_fully_named")

# all elements (other than '.') inherit from 'data.frame'

bad_shapes        <- gtfs
bad_shapes$shapes <- list(oi = 1)

error_pat <- paste0(
  "Every element in a GTFS object must inherit from 'data\\.frame'\\. ",
  "The following elements do not: 'shapes'"
)

expect_error(
  assert_gtfs(bad_shapes),
  pattern = error_pat,
  class = "gtfs_table_not_df"
)

bad_shapes$. <- list(oi = data.frame(oi = 2))
expect_error(assert_gtfs(bad_shapes), class = "gtfs_table_not_df")

bad_shapes$shapes <- gtfs$shapes
expect_silent(assert_gtfs(bad_shapes))

# '.' must be a list

bad_dot   <- gtfs
bad_dot$. <- c("a", "b")

expect_error(
  assert_gtfs(bad_dot),
  pattern = "The '\\.' element of a GTFS object must be a list\\.",
  class = "gtfs_dot_not_list"
)

# all elements inside '.' must be named

no_names_dot   <- bad_dot
no_names_dot$. <- list(data.frame(), data.frame(oi = 3))
expect_error(
  assert_gtfs(no_names_dot),
  pattern = "Every element inside '\\.' must be named\\.",
  class = "gtfs_dot_not_fully_named"
)

no_names_dot$. <- list(a = data.frame(), data.frame())
expect_error(assert_gtfs(no_names_dot), class = "gtfs_dot_not_fully_named")

# all elements inside '.' must inherit from 'data.frame'

no_df_dot   <- bad_dot
no_df_dot$. <- list(a = c("a", "b"), b = data.frame(oi = 4))
expect_error(
  assert_gtfs(no_df_dot),
  pattern = paste0(
    "Every element inside '\\.' must inherit from 'data\\.frame'\\. ",
    "The following elements do not: 'a'"
  ),
  class = "gtfs_dot_table_not_df"
)

# in case of success, 'assert_gtfs' should return the object it was given

good_gtfs   <- gtfs
good_gtfs$. <- list(ola = data.frame(oi = 5))
expect_silent(output <- assert_gtfs(good_gtfs))
expect_identical(output, good_gtfs)
