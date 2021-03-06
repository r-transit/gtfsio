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
