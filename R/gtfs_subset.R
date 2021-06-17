#' Subset a GTFS object
#'
#' Subsetting a GTFS object using \code{[} preserves the object class.
#'
#' @param x A GTFS object.
#' @param value Either a numeric or a character vector. Designates the elements
#'   to be returned.
#'
#' @return A GTFS object.
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' gtfs <- import_gtfs(gtfs_path)
#' names(gtfs)
#' class(gtfs)
#'
#' small_gtfs <- gtfs[1:5]
#' names(small_gtfs)
#' class(small_gtfs)
#'
#' small_gtfs <- gtfs[c("shapes", "trips")]
#' names(small_gtfs)
#' class(small_gtfs)
#'
#' @export
`[.gtfs` <- function(x, value) {

  # make sure that the resulting object keeps the original object classes

  subclass <- setdiff(class(x), c("gtfs", "list"))

  return(new_gtfs(NextMethod(), subclass = subclass))

}
