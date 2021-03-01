
gtfszip <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")

### expected behavior ----------------
test_that("import_gtfs expected behavior", {

  # whole file
  testthat::expect_type(object = import_gtfs(gtfszip), type = 'list')

  # selected files and fields
  testthat::expect_type(object =  import_gtfs(path = gtfszip,
                                     files = 'stops',
                                     fields = list('stops'=c('stop_id')),
                                     quiet = T),
                        type = 'list')

  # quiet argument
  testthat::expect_type(object = import_gtfs(gtfszip, quiet = F),
                        type = 'list')

})

### expected errors and messages ----------------

test_that("import_gtfs errors and messages", {

  # Wrong path
  testthat::expect_error(import_gtfs(path = 'aaa'))
  testthat::expect_error(import_gtfs(path = 1))

  # Wrong file
  testthat::expect_error(import_gtfs(path = gtfszip, files = 'aaa'))

  # Wrong field
  testthat::expect_warning(import_gtfs(path = gtfszip,
                                     files = 'stops',
                                     fields = list('stops'=c('aaa'))))
  # wrong quiet
  testthat::expect_error(import_gtfs(path = gtfszip, quiet = 'aaa'))
  })
