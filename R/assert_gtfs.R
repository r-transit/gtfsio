#' GTFS object validator
#'
#' @description
#' Asserts that a GTFS object is valid. Valid objects are those in which:
#'
#' - Every element is named.
#' - Every element inherits from `data.frame`s.
#'
#' The exception to the second rule are objects that contain an element named
#' `"."`. In such case, this element is actually composed by a named list of
#' elements who inherit from `data.frame`s.
#'
#' @param x A GTFS object.
#'
#' @return The same GTFS object passed to \code{x}.
#'
#' @family constructors
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' gtfs <- import_gtfs(gtfs_path)
#' gtfs <- assert_gtfs(gtfs)
#'
#' @export
assert_gtfs <- function(x) {

  # check if all elements are named

  if (is.null(names(x))) error_gtfs_not_fully_named()

  x_names <- names(x)[! names(x) %chin% ""]
  if (length(x_names) != length(x)) error_gtfs_not_fully_named()

  # check if all elements (other than '.') inherit from 'data.frame'

  no_dot_names <- setdiff(names(x), ".")
  inherit_df <- vapply(x[no_dot_names], inherits, logical(1), "data.frame")

  if (!all(inherit_df)) error_table_not_df(no_dot_names, inherit_df)

  # if '.' element exists

  if ("." %chin% names(x)) {

    # check if it is a list

    if (!is.list(x[["."]])) error_dot_not_list()

    # check if all elements are named

    if (is.null(names(x[["."]]))) error_dot_not_fully_named()

    dot_names <- names(x[["."]])[! names(x[["."]]) %chin% ""]
    if (length(dot_names) != length(x[["."]])) error_dot_not_fully_named()

    # check if all elements inherit from 'data.frame'

    dot_inherit_df <- vapply(x[["."]], inherits, logical(1), "data.frame")
    if (!all(dot_inherit_df)) error_dot_table_not_df(x, dot_inherit_df)

  }

  return(x)

}


# errors ------------------------------------------------------------------


#' @include gtfsio_error.R
error_gtfs_not_fully_named <- parent_function_error(
  "Every element in a GTFS object must be named.",
  subclass = "gtfs_not_fully_named"
)

error_table_not_df <- function(no_dot_names, inherit_df) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "Every element in a GTFS object must inherit from 'data.frame'. ",
    "The following elements do not: ",
    paste0("'", no_dot_names[!inherit_df], "'", collapse = ", ")
  )

  gtfsio_error(
    message = message,
    subclass = "gtfs_table_not_df",
    call = parent_call
  )
}

error_dot_not_list <- parent_function_error(
  "The '.' element of a GTFS object must be a list.",
  subclass = "gtfs_dot_not_list"
)

error_dot_not_fully_named <- parent_function_error(
  "Every element inside '.' must be named.",
  subclass = "gtfs_dot_not_fully_named"
)

error_dot_table_not_df <- function(x, dot_inherit_df) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "Every element inside '.' must inherit from 'data.frame'. ",
    "The following elements do not: ",
    paste0("'", names(x[["."]])[!dot_inherit_df], "'", collapse = ", ")
  )

  gtfsio_error(
    message = message,
    subclass = "gtfs_dot_table_not_df",
    call = parent_call
  )
}
