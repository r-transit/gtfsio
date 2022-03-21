#' Check the existence of text files in a GTFS object
#'
#' Checks the existence of elements inside a GTFS object that represent specific
#' GTFS text files.
#'
#' @param x A GTFS object.
#' @param files A character vector. The files to check the existence of.
#'
#' @return
#' \code{check_file_exists} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_file_exists} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_file_exists(gtfs, c("calendar", "agency"))
#'
#' check_file_exists(gtfs, c("calendar", "oi"))
#'
#' @export
check_file_exists <- function(x, files) {

  assert_class(x, "gtfs")
  assert_vector(files, "character")

  # actual checking

  missing_files <- setdiff(files, names(x))

  if (identical(missing_files, character(0))) return(TRUE)

  return(FALSE)

}



#' @rdname check_file_exists
#' @export
assert_file_exists <- function(x, files) {

  assert_class(x, "gtfs")
  assert_vector(files, "character")

  # actual checking

  missing_files <- setdiff(files, names(x))

  if (identical(missing_files, character(0))) return(invisible(x))

  gtfsio_error(
    paste0(
      "The GTFS object is missing the following required element(s): ",
      paste0("'", missing_files, "'", collapse = ", ")
    ),
    subclass = "missing_required_file"
  )

}



#' Check the existence of fields in a GTFS object element
#'
#' Checks the existence of fields, represented by columns, inside a GTFS object
#' element.
#'
#' @param x A GTFS object.
#' @param file A string. The element, that represents a GTFS text file, where
#'   fields should be searched.
#' @param fields A character vector. The fields to check the existence of.
#'
#' @return
#' \code{check_field_exists} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_field_exists} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_field_exists(gtfs, "calendar", c("monday", "tuesday"))
#'
#' check_field_exists(gtfs, "calendar", c("monday", "oi"))
#'
#' @export
check_field_exists <- function(x, file, fields) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")

  # check if 'file' exists, and return FALSE if it doesn't

  if (!check_file_exists(x, file)) return(FALSE)

  # checking for field existence

  missing_fields <- setdiff(fields, names(x[[file]]))

  if (identical(missing_fields, character(0))) return(TRUE)

  return(FALSE)

}



#' @rdname check_field_exists
#' @export
assert_field_exists <- function(x, file, fields) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")
  assert_file_exists(x, file)

  # actual checking

  missing_fields <- setdiff(fields, names(x[[file]]))

  if (identical(missing_fields, character(0))) return(invisible(x))

  gtfsio_error(
    paste0(
      "The GTFS object '",
      file,
      "' element is missing the following required column(s): ",
      paste0("'", missing_fields, "'", collapse = ", ")
    ),
    subclass = "missing_required_field"
  )

}



#' Check the classes of fields in a GTFS object element
#'
#' Checks the classes of fields, represented by columns, inside a GTFS object
#' element.
#'
#' @param x A GTFS object.
#' @param file A string. The element, that represents a GTFS text file, whose
#'   fields' classes should be checked.
#' @param fields A character vector. The fields to have their classes checked.
#' @param classes A character vector, with the same length of \code{fields}. The
#'   classes that each field must inherit from.
#'
#' @return
#' \code{check_field_class} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_field_class} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_field_class(
#'   gtfs,
#'   "calendar",
#'   fields = c("monday", "tuesday"),
#'   classes = rep("integer", 2)
#' )
#'
#' check_field_class(
#'   gtfs,
#'   "calendar",
#'   fields = c("monday", "tuesday"),
#'   classes = c("integer", "character")
#' )
#'
#' @export
check_field_class <- function(x, file, fields, classes) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")

  if (!is.character(classes) | length(classes) != length(fields))
    gtfsio_error(
      "'classes' must be a character vector with the same length of 'fields'.",
      subclass = "bad_classes_argument"
    )

  # check if 'fields' exists, and return FALSE if it doesn't

  if (!check_field_exists(x, file, fields)) return(FALSE)

  # checking - compare the desired classes to the actual classes

  col_classes <- vapply(x[[file]], class, character(1))
  actual_classes <- col_classes[fields]

  if (all(classes == actual_classes)) return(TRUE)

  return(FALSE)

}



#' @rdname check_field_class
#' @export
assert_field_class <- function(x, file, fields, classes) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")
  assert_field_exists(x, file, fields)

  if (!is.character(classes) | length(classes) != length(fields))
    gtfsio_error(
      "'classes' must be a character vector with the same length of 'fields'.",
      subclass = "bad_classes_argument"
    )

  # actual checking - compare the desired classes to the actual classes

  col_classes <- vapply(x[[file]], class, character(1))
  actual_classes <- col_classes[fields]

  if (all(classes == actual_classes)) return(invisible(x))

  bad_fields <- fields[classes != actual_classes]
  req_classes <- classes[classes != actual_classes]
  act_classes <- actual_classes[classes != actual_classes]

  gtfsio_error(
    paste0(
      "The following columns in the GTFS object '",
      file,
      "' element do not inherit from the required classes:\n",
      paste0(
        "  - '",
        bad_fields,
        "': requires ",
        req_classes,
        ", but inherits from ",
        act_classes,
        collapse = "\n"
      )
    ),
    subclass = "wrong_class_field"
  )

}
