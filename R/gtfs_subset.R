#' Subset a GTFS object
#'
#' Subsetting a GTFS object using \code{[}, \code{[[} and \code{$} is very
#' similar to subsetting lists, but a bit more verbose, as warnings are thrown
#' when attempting to subset a missing element. When using \code{[} the
#' resulting object inherits classes (and subclasses) from the original object.
#'
#' @param x A GTFS object.
#' @param value Either a numeric or a character vector. Designates the elements
#'   to be returned.
#'
#' @return A GTFS object.
#'
#' @family subset
#'
#' @name gtfs_subset
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' gtfs <- import_gtfs(gtfs_path)
#' names(gtfs)
#' class(gtfs)
#'
#' # '[' behaviour
#' small_gtfs <- gtfs[1:5]
#' names(gtfs)
#' class(small_gtfs)
#'
#' bad_gtfs <- gtfs["ola"] # results in warning
#'
#' # '$' behaviour
#' stop_times <- gtfs$stop_times
#' names(stop_times)
#'
#' bad_element <- gtfs$ola # results in warning
#'
#' # '[[' behaviour
#' shapes <- gtfs$shapes
#' names(shapes)
#'
#' bad_element <- gtfs[["ola"]] # results in warning
NULL

#' @rdname gtfs_subset
#' @param name A string. The name of the element to be returned.
#' @export
`$.gtfs` <- function(x, name) {

  # throw warnings if 'name' is an invalid name

  check_invalid_value(x, name)

  return(NextMethod())

}

#' @rdname gtfs_subset
#' @export
`[.gtfs` <- function(x, value) {

  # throw warnings if 'value' is an invalid name/index

  check_invalid_value(x, value)

  # make sure that the resulting object keeps the original object classes

  subclass <- setdiff(class(x), "gtfs")

  return(new_gtfs(NextMethod(), subclass = subclass))

}

#' @rdname gtfs_subset
#' @export
`[[.gtfs` <- function(x, value) {

  # throw warnings if 'value' is an invalid name/index

  check_invalid_value(x, value)

  return(NextMethod())

}

#' Check for invalid values
#'
#' Checks if all elements from \code{value} correspond to an element from
#' \code{x} and throws a warning if any invalid value is found.
#'
#' @param x A GTFS object.
#' @param value Either a numeric or a character vector.
#'
#' @return \code{NULL}, invisibly. Called for side-effects.
#'
#' @keywords internal
check_invalid_value <- function(x, value) {

  if (is.character(value)) {

    bad_element <- value[! value %chin% names(x)]
    if (!identical(bad_element, character(0)))
      warning(
        "This GTFS object doesn't contain the following element(s): ",
        paste0("'", bad_element, "'", collapse = ", ")
      )

  } else if (is.numeric(value)) {

    bad_index <- value[value > length(x)]

    if (!identical(bad_index, numeric(0)) & !identical(bad_index, integer(0)))
      warning(
        "This GTFS object doesn't contain the following index(es): ",
        paste0(bad_index, collapse = ", ")
      )

  }

  return(invisible(NULL))

}
