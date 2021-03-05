#' GTFS object constructor
#'
#' Creates a GTFS object. Mostly useful for package authors who may want to
#' either create \code{gtfs} objects in their packages or create subclasses of
#' the main \code{gtfs} class. The usage of this function assumes some knowledge
#' on \code{gtfs} objects, thus inputs are not extensively checked. See
#' \code{gtfs} (TODO: remember to add link to gtfs()) for a user-friendlier and more thoroughly checked
#' constructor.
#'
#' @param x A GTFS-like object (either a GTFS object or a named list). Each
#'   element must represent a distinct GTFS text file.
#' @param subclass A character vector. Subclasses to be assigned to the
#'   \code{gtfs} object.
#' @param ... Name-value pairs. Additional attributes.
#'
#' @return A GTFS object: a named list of data frames, each one corresponding to
#'   a distinct GTFS text file, with \code{gtfs} class.
#'
#' @seealso \code{\link{assert_gtfs}}, \code{gtfs} (TODO: remember to add link to gtfs())
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

  # append "gtfs" to 'subclass', so any objects created by 'new_gtfs()' always
  # inherit from gtfs class

  class <- c(subclass, "gtfs")

  # create object

  gtfs <- structure(x, class = class)

  return(gtfs)

}



#' GTFS object validator
#'
#' Asserts that a GTFS object is valid. Valid objects are those in which:
#' \itemize{
#'   \item Every element is named.
#'   \item Every element inherits from \code{data.frame}s.
#' }
#' The exception to the second rule are objects that contain an element named
#' \code{"."}. In such case, this element is actually composed by a named list
#' of elements who inherit from \code{data.frame}s.
#'
#' @param x A GTFS object.
#'
#' @return The same GTFS object passed to \code{x}.
#'
#' @seealso \code{\link{new_gtfs}}, \code{gtfs} (TODO: remember to add link to gtfs())
#'
#' @export
assert_gtfs <- function(x) {

  # check if all elements are named

  if (is.null(names(x))) stop("Every element in a GTFS object must be named.")

  x_names <- names(x)[! names(x) %chin% ""]
  if (length(x_names) != length(x))
    stop("Every element in a GTFS object must be named.")

  # check if all elements (other than '.') inherit from 'data.frame'

  no_dot_names <- setdiff(names(x), ".")
  inherit_df <- vapply(x[no_dot_names], inherits, logical(1), "data.frame")

  if (!all(inherit_df))
    stop(
      "Every element in a GTFS object must inherit from 'data.frame'. ",
      "The following elements do not: ",
      paste0("'", no_dot_names[!inherit_df], "'", collapse = ", ")
    )

  # if '.' element exists

  if ("." %chin% names(x)) {

    # check if it is a list

    if (!is.list(x[["."]]))
      stop("The '.' element of a GTFS object must be a list.")

    # check if all elements are named

    if (is.null(names(x[["."]])))
      stop("Every element inside '.' must be named.")

    dot_names <- names(x[["."]])[! names(x[["."]]) %chin% ""]
    if (length(dot_names) != length(x[["."]]))
      stop("Every element inside '.' must be named.")

    # check if all elements inherit from 'data.frame'

    dot_inherit_df <- vapply(x[["."]], inherits, logical(1), "data.frame")
    if (!all(dot_inherit_df))
      stop(
        "Every element inside '.' must inherit from 'data.frame'. ",
        "The following elements do not: ",
        paste0("'", names(x[["."]])[!dot_inherit_df], "'", collapse = ", ")
      )

  }

  return(x)

}
