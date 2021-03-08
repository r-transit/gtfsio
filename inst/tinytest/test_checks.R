# tests setup

gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)


##### CHECK_FILES_EXIST

# input checking

expect_error(check_files_exist(unclass(gtfs), "shapes"))
expect_error(check_files_exist(gtfs, factor("shapes")))

# expected results

expect_true(check_files_exist(gtfs, "shapes"))
expect_true(check_files_exist(gtfs, c("shapes", "trips")))
expect_false(check_files_exist(gtfs, "ola"))
expect_false(check_files_exist(gtfs, c("shapes", "ola")))


##### ASSERT_FILES_EXIST

# input checking

expect_error(assert_files_exist(unclass(gtfs), "shapes"))
expect_error(assert_files_exist(gtfs, factor("shapes")))

# expected results

expect_identical(assert_files_exist(gtfs, "shapes"), gtfs)
expect_identical(assert_files_exist(gtfs, c("shapes", "trips")), gtfs)
expect_silent(assert_files_exist(gtfs, "shapes"))
expect_silent(assert_files_exist(gtfs, c("shapes", "trips")))

expect_error(assert_files_exist(gtfs, "ola"))
expect_error(assert_files_exist(gtfs, c("shapes", "ola")))
expect_error(assert_files_exist(gtfs, c("oi", "ola")))
