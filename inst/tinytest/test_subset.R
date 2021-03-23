gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
gtfs <- import_gtfs(gtfs_path)

# subsetting with '[' preserves the 'gtfs' class and any other subclasses

small_gtfs <- gtfs[1:5]
expect_identical(class(small_gtfs), class(gtfs))

small_gtfs <- gtfs[c("shapes", "trips")]
expect_identical(class(small_gtfs), class(gtfs))

gtfs_sub       <- new_gtfs(gtfs, "sub")
small_gtfs_sub <- gtfs_sub[1:5]
expect_identical(class(small_gtfs_sub), class(gtfs_sub))

# keeping classes aside, the behaviour must be identical to '[.default'

no_class <- unclass(gtfs)

expect_identical(unclass(gtfs[1]), no_class[1])
expect_identical(unclass(gtfs[NULL]), no_class[NULL])
expect_identical(unclass(gtfs[NA]), no_class[NA])
