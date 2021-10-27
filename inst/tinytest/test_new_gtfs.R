gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


# input checks ------------------------------------------------------------

# 'x' must be a list

expect_error(
  new_gtfs(1:5),
  pattern = "'x' must be a list\\.",
  class = "bad_x_argument"
)
expect_error(new_gtfs(1:5), class = "new_gtfs_error")

# 'x' must be a named list

no_names <- gtfs
names(no_names) <- NULL
expect_error(new_gtfs(no_names), pattern = "'x' must be a named list\\.")

# every single element of 'x' must be named

missing_one_name <- gtfs
missing_one_name[[18]] <- data.frame(a = 1)

expect_error(
  new_gtfs(missing_one_name),
  pattern = "Every element in 'x' must be named\\."
)

# 'subclass' must be a character vector

expect_error(
  new_gtfs(gtfs, subclass = factor("subclass")),
  pattern = "'subclass' must be a\\(n\\) character vector\\.",
  class = "bad_subclass_argument"
)


# actual behaviour --------------------------------------------------------

no_class_gtfs <- unclass(gtfs)

# by default an object created with new_gtfs() should inherent from 'gtfs' and
# 'list' only

default <- new_gtfs(no_class_gtfs)

expect_identical(class(default), c("gtfs", "list"))

# if a subclass is specified, the object should inherit from 'gtfs', 'list' and
# the subclass

extra_class <- new_gtfs(no_class_gtfs, subclass = "subclass")

expect_identical(class(extra_class), c("subclass", "gtfs", "list"))

# attributes passed to '...' should be assigned to the object

extra_attr <- new_gtfs(no_class_gtfs, extra_attr = "extra_attr")

expect_equal(attr(extra_attr, "extra_attr"), "extra_attr")
