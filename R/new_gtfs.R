#' GTFS object constructor
#'
#' Creates a GTFS object. Mostly useful for package authors who may want to
#' either create \code{gtfs} objects in their packages or create subclasses of
#' the main \code{gtfs} class. The usage of this function assumes some knowledge
#' on \code{gtfs} objects, thus inputs are not extensively checked. See
#' \code{\link{assert_gtfs}} for more thorough checks.
#'
#' @param x A GTFS-like object (either a GTFS object or a named list). Each
#'   element must represent a distinct GTFS text file.
#' @param subclass A character vector. Subclasses to be assigned to the
#'   \code{gtfs} object.
#' @param ... Name-value pairs. Additional attributes.
#'
#' @return A GTFS object: a named list of data frames, each one corresponding to
#'   a distinct GTFS text file, with \code{gtfs} and \code{list} classes.
#'
#' @family constructors
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' tmpdir <- tempfile(pattern = "new_gtfs_example")
#' zip::unzip(gtfs_path, exdir = tmpdir)
#'
#' agency <- data.table::fread(file.path(tmpdir, "agency.txt"))
#' stops <- data.table::fread(file.path(tmpdir, "stops.txt"))
#' routes <- data.table::fread(file.path(tmpdir, "routes.txt"))
#' trips <- data.table::fread(file.path(tmpdir, "trips.txt"))
#' stop_times <- data.table::fread(file.path(tmpdir, "stop_times.txt"))
#' calendar <- data.table::fread(file.path(tmpdir, "calendar.txt"))
#'
#' txt_files <- list(
#'   agency = agency,
#'   stops = stops,
#'   routes = routes,
#'   trips = trips,
#'   stop_times = stop_times,
#'   calendar = calendar
#' )
#'
#' gtfs <- new_gtfs(txt_files)
#'
#' class(gtfs)
#' names(gtfs)
#'
#' @export
new_gtfs <- function(x, subclass = character(), ...) {

  # input checking

  if (!is.list(x)) stop("'x' must be a list.")

  if (is.null(names(x))) stop("'x' must be a named list.")

  x_names <- names(x)[! names(x) %chin% ""]
  if (length(x_names) != length(x)) stop("Every element in 'x' must be named.")

  if (!is.character(subclass)) stop("'subclass' must be a character vector.")

  # append "gtfs" and "list" to 'subclass', so any objects created by
  # 'new_gtfs()' always inherit from gtfs and list classes

  class <- c(subclass, "gtfs", "list")

  # create object

  gtfs <- structure(x, class = class, ...)

  return(gtfs)

}
