#' Check the existence of text files in a GTFS object
#'
#' Checks the existence of elements inside a GTFS object that represent specific
#' GTFS text files.
#'
#' @param x A GTFS object.
#' @param files A character vector. The files to check the existence of.
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

  assert_class(x, "gtfs")
  assert_vector(files, "character")

  # actual checking

  missing_files <- setdiff(files, names(x))

  if (identical(missing_files, character(0))) return(TRUE)

  return(FALSE)

}



#' @rdname check_files_exist
#' @export
assert_files_exist <- function(x, files) {

  assert_class(x, "gtfs")
  assert_vector(files, "character")

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
#' \code{check_fields_exist} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_fields_exist} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_fields_exist(gtfs, "calendar", c("monday", "tuesday"))
#'
#' check_fields_exist(gtfs, "calendar", c("monday", "oi"))
#'
#' @export
check_fields_exist <- function(x, file, fields) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")

  # check if 'file' exists, and return FALSE if it doesn't

  if (!check_files_exist(x, file)) return(FALSE)

  # checking for field existence

  missing_fields <- setdiff(fields, names(x[[file]]))

  if (identical(missing_fields, character(0))) return(TRUE)

  return(FALSE)

}



#' @rdname check_fields_exist
#' @export
assert_fields_exist <- function(x, file, fields) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")
  assert_files_exist(x, file)

  # actual checking

  missing_fields <- setdiff(fields, names(x[[file]]))

  if (identical(missing_fields, character(0))) return(invisible(x))

  stop(
    paste0(
      "The GTFS object '",
      file,
      "' element is missing the following required column(s): ",
      paste0("'", missing_fields, "'", collapse = ", ")
    )
  )

}



#' Check the types of fields in a GTFS object element
#'
#' Checks the types of fields, represented by columns, inside a GTFS object
#' element.
#'
#' @param x A GTFS object.
#' @param file A string. The element, that represents a GTFS text file, whose
#'   fields' types should be checked.
#' @param fields A character vector. The fields to have their types checked.
#' @param types A character vector, with the same length of \code{fields}. The
#'   types that each field must inherit from.
#'
#' @return
#' \code{check_fields_types} returns \code{TRUE} if the check is successful, and
#'   \code{FALSE} otherwise. \cr
#' \code{assert_fields_types} returns \code{x} invisibly if the check is
#'   successful, and throws an error otherwise.
#'
#' @family checking functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#' gtfs <- import_gtfs(gtfs_path)
#'
#' check_fields_types(
#'   gtfs,
#'   "calendar",
#'   fields = c("monday", "tuesday"),
#'   types = rep("integer", 2)
#' )
#'
#' check_fields_types(
#'   gtfs,
#'   "calendar",
#'   fields = c("monday", "tuesday"),
#'   types = c("integer", "character")
#' )
#'
#' @export
check_fields_types <- function(x, file, fields, types) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")

  if (!is.character(types) | length(types) != length(fields))
    gtfsio_error(
      "'types' must be a character vector with the same length of 'fields'.",
      subclass = "bad_types_argument"
    )

  # check if 'fields' exists, and return FALSE if it doesn't

  if (!check_fields_exist(x, file, fields)) return(FALSE)

  # checking - compare the desired types to the actual types

  actual_types <- vapply(x[[file]][, ..fields], class, character(1))

  if (all(types == actual_types)) return(TRUE)

  return(FALSE)

}



#' @rdname check_fields_types
#' @export
assert_fields_types <- function(x, file, fields, types) {

  assert_class(x, "gtfs")
  assert_vector(file, "character", len = 1L)
  assert_vector(fields, "character")
  assert_fields_exist(x, file, fields)

  if (!is.character(types) | length(types) != length(fields))
    gtfsio_error(
      "'types' must be a character vector with the same length of 'fields'.",
      subclass = "bad_types_argument"
    )

  # actual checking - compare the desired types to the actual types

  actual_types <- vapply(x[[file]][, ..fields], class, character(1))

  if (all(types == actual_types)) return(invisible(x))

  bad_fields <- fields[types != actual_types]
  req_types <- types[types != actual_types]
  act_types <- actual_types[types != actual_types]

  stop(
    paste0(
      "The following columns in the GTFS object '",
      file,
      "' element do not inherit from the required types:\n",
      paste0(
        "  - '",
        bad_fields,
        "': requires ",
        req_types,
        ", but inherits from ",
        act_types,
        collapse = "\n"
      )
    )
  )

}
