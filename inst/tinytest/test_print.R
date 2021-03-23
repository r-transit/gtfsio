gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

# 'class' attribute should not be printed alongside the object itself

out       <- capture.output(print(gtfs))
has_class <- grepl("class", out)

expect_false(any(has_class))

# but other attributes should

attr(gtfs, "tmp") <- "temp_attribute"

out      <- capture.output(print(gtfs))
has_attr <- grepl("attr", out)

expect_true(any(has_attr))

# 'class' aside, print.gtfs() behaves like print.default(), with and without
# additional attributes

out_gtfs    <- capture.output(print(gtfs))
out_default <- capture.output(print(unclass(gtfs)))

expect_identical(out_gtfs, out_default)

attr(gtfs, "tmp") <- NULL

out_gtfs    <- capture.output(print(gtfs))
out_default <- capture.output(print(unclass(gtfs)))

expect_identical(out_gtfs, out_default)
