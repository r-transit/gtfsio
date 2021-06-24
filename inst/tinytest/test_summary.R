gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

# 'class' attribute should not be summarised alongside the object itself

out       <- capture.output(summary(gtfs))
has_class <- grepl("class", out)

expect_false(any(has_class))

# attributes should not print

attr(gtfs, "tmp") <- "temp_attribute"

out      <- capture.output(summary(gtfs))
has_attr <- grepl("temp", out)

expect_false(any(has_attr))

# summary.gtfs() is different to print.default()

out_gtfs    <- capture.output(summary(gtfs))
out_default <- capture.output(summary(unclass(gtfs)))

expect_true(!identical(out_gtfs, out_default))

# default summary is more verbose:

expect_true(length(out_gtfs) < length(out_default))
