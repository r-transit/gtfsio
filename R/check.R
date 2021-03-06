#' Check the existence of text files in a GTFS object
#'
#' Checks the existence of elements inside a GTFS object that represent specific
#' GTFS text files.
#'
#' @param x A GTFS object.
#' @param file A character vector. The files to check the existence of.
#'
#' @return
#' \code{check_files_exist} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_files_exist} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_files_exist(gtfs, c("calendar", "agency"))
#'
#' check_files_exist(gtfs, c("calendar", "oi"))
#'
#' @export
check_files_exist <- function(x, files) {

  # input checking

  if (!inherits(x, "gtfs")) stop("'x' must inherit from the 'gtfs' class.")

  if (!is.character(files)) stop("'files' must be a character vector.")

  # actual checking

  missing_files <- setdiff(files, names(x))

  if (identical(missing_files, character(0))) return(TRUE)

  return(FALSE)

}



#' @rdname check_files_exist
#' @export
assert_files_exist <- function(x, files) {

  # input checking

  if (!inherits(x, "gtfs")) stop("'x' must inherit from the 'gtfs' class.")

  if (!is.character(files)) stop("'files' must be a character vector.")

  # actual checking

  missing_files <- setdiff(files, names(x))

  if (identical(missing_files, character(0))) return(invisible(x))

  stop(
    paste0(
      "The GTFS object is missing the following required element(s): ",
      paste0("'", missing_files, "'", collapse = ", ")
    )
  )

}
