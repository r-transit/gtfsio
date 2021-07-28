#' Print a GTFS object
#'
#' Prints a GTFS object suppressing the \code{class} attribute.
#'
#' @param x A GTFS object.
#' @param ... Optional arguments ultimately passed to \code{format}.
#'
#' @return The GTFS object that was printed, invisibly.
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' # subset 'gtfs' for a smaller output
#' gtfs <- gtfs[c("routes", "trips")]
#'
#' print(gtfs)
#'
#' @export
print.gtfs <- function(x, ...) {

  print(unclass(x), ...)

  return(invisible(x))

}

#' Print summary of a GTFS object
#'
#' @param object A GTFS object.
#' @param ... Ignored.
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' summary(gtfs)
#'
#' @export
summary.gtfs <- function(object, ...) {

    cat(
      "A gtfs object with the following tables",
      "and respective numbers of entries in each:\n"
    )

    print(vapply(object, nrow, numeric(1)))

}
