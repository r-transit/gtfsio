#' GTFS object constructor
#'
#' Creates a GTFS object. Mostly useful for package authors who may want to
#' either create \code{gtfs} objects in their packages or create subclasses of
#' the main \code{gtfs} class. The usage of this function assumes some knowledge
#' on \code{gtfs} objects, thus inputs are not extensively checked. See
#' \code{gtfs} (TODO: remember to add link to gtfs()) for a user-friendlier and more thoroughly checked
#' constructor.
#'
#' @param x A list. Each element must represent a distinct GTFS text file.
#' @param names A character vector. The name of each \code{gtfs} object element
#'   (i.e. the name of the GTFS text files that each dataframe represents).
#' @param subclass A character vector. Subclasses to be assigned to the
#'   \code{gtfs} object.
#' @param ... Name-value pairs. Additional attributes.
#'
#' @return A GTFS object: a named list of dataframes, each one corresponding to
#'   a distinct GTFS text file.
#'
#' @seealso \code{gtfs} (TODO: remember to add link to gtfs())
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' tmpdir <- file.path(tempdir(), "new_gtfs_example")
#' zip::unzip(gtfs_path, exdir = tmpdir)
#'
#' agency <- data.table::fread(file.path(tmpdir, "agency.txt"))
#' stops <- data.table::fread(file.path(tmpdir, "stops.txt"))
#' routes <- data.table::fread(file.path(tmpdir, "routes.txt"))
#' trips <- data.table::fread(file.path(tmpdir, "trips.txt"))
#' stop_times <- data.table::fread(file.path(tmpdir, "stop_times.txt"))
#' calendar <- data.table::fread(file.path(tmpdir, "calendar.txt"))
#'
#' flist <- list(agency, stops, routes, trips, stop_times, calendar)
#' fname <- c("agency", "stops", "routes", "trips", "stop_times", "calendar")
#'
#' gtfs <- new_gtfs(flist, fname)
#'
#' class(gtfs)
#' names(gtfs)
#'
#' @export
new_gtfs <- function(x, names, subclass = character(), ...) {

  # input checking

  if (!is.list(x)) stop("'x' must be a list.")

  if (!is.character(names) & length(names) != length(x))
    stop("'names' must be a character vector with the same length of 'x'.")

  if (!is.character(subclass)) stop("'subclass' must be a character vector.")

  # append "gtfs" to 'subclass', so any objects created by 'new_gtfs()' always
  # inherit from gtfs class

  class <- c(subclass, "gtfs")

  # create object

  gtfs <- structure(x, names = names, class = class)

}
