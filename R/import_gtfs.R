#' Import GTFS transit feeds
#'
#' Imports GTFS transit feeds from either a local \code{.zip} file or an URL.
#' Columns are parsed according to the standards for reading and writing GTFS
#' feeds specified in \code{\link{get_gtfs_standards}}.
#'
#' @param path A string. The path to a GTFS \code{.zip} file.
#' @param files A character vector. The text files to be read from the GTFS,
#'   without the \code{.txt} extension. If \code{NULL} (the default), all
#'   existing text files are read.
#' @param fields A named list. The fields to be read from each text file, in the
#'   format \code{list(file1 = c("field1", "field2"))}. If \code{NULL} (the
#'   default), all fields from the files specified in \code{files} are read. If
#'   a file is specified in \code{files} but not in \code{fields}, all fields
#'   from that file will be read (i.e. you may specify in \code{fields} only
#'   files whose fields you want to subset).
#' @param extra_spec A named list. Custom specification used when reading
#'   undocumented fields, in the format
#'   \code{list(file1 = c(field1 = "type1", field2 = "type2"))}. If \code{NULL}
#'   (the default), all undocumented fields are read as character. Similarly,
#'   if an undocumented field is not specified in \code{extra_spec}, it is read
#'   as character (i.e. you may specify in \code{extra_spec} only the fields
#'   that you want to read as a different type). Only supports the
#'   \code{character}, \code{integer} and \code{numeric} types, also used in
#'   \code{\link{get_gtfs_standards}}.
#' @param skip A character vector. Text files that should not be read from the
#'   GTFS, without the \code{.txt} extension. If \code{NULL} (the default), no
#'   files are skipped. Cannot be used if \code{files} is set.
#' @param quiet A logical. Whether to hide log messages and progress bars
#'   (defaults to \code{TRUE}).
#' @param encoding A string. Passed to \code{\link[data.table]{fread}}, defaults
#'   to \code{"unknown"}. Other possible options are \code{"UTF-8"} and
#'   \code{"Latin-1"}. Please note that this is not used to re-encode the input,
#'   but to enable handling encoded strings in their native encoding.
#'
#' @return A GTFS object: a named list of data frames, each one corresponding to
#'   a distinct text file from the given GTFS feed.
#'
#' @seealso \code{\link{get_gtfs_standards}}
#'
#' @family io functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' # read all files and columns
#' gtfs <- import_gtfs(gtfs_path)
#' names(gtfs)
#' names(gtfs$trips)
#'
#' # read all columns from selected files
#' gtfs <- import_gtfs(gtfs_path, files = c("trips", "stops"))
#' names(gtfs)
#' names(gtfs$trips)
#'
#' # read specific columns from selected files
#' gtfs <- import_gtfs(
#'   gtfs_path,
#'   files = c("trips", "stops"),
#'   fields = list(
#'     trips = c("route_id", "trip_id"),
#'     stops = c("stop_id", "stop_lat", "stop_lon")
#'   )
#' )
#'
#' @export
import_gtfs <- function(path,
                        files = NULL,
                        fields = NULL,
                        extra_spec = NULL,
                        skip = NULL,
                        quiet = TRUE,
                        encoding = "unknown") {

  # input checking ('files', 'fields' and 'extra_spec' are more thoroughly
  # validated further down the code)

  val_enc <- c("unknown", "UTF-8", "Latin-1")

  assert_vector(path, "character", len = 1L)
  assert_vector(quiet, "logical", len = 1L)
  assert_list(extra_spec, null_ok = TRUE, named = TRUE)
  assert_vector(files, "character", null_ok = TRUE)
  assert_list(fields, null_ok = TRUE)
  assert_vector(skip, "character", null_ok = TRUE)
  assert_vector(encoding, "character", len = 1L, subset_of = val_enc)

  path_is_url <- grepl("^http[s]?\\:\\/\\/\\.*", path)

  if (!path_is_url && !file.exists(path)) error_non_existent_file(path)
  if (!is.null(files) && !is.null(skip)) error_files_and_skip_provided()

  for (input_types in extra_spec) {
    if (any(! input_types %chin% c("character", "integer", "numeric"))) {
      error_unsupported_extra_spec()
    }
  }

  # if 'path' is an URL, download it and save path to downloaded file to 'path'

  if (path_is_url) {
    tmp <- tempfile(pattern = "gtfs", fileext = ".zip")
    utils::download.file(path, tmp, method = "auto", quiet = quiet)

    if (!quiet) message("File downloaded to ", tmp, ".")

    path <- tmp
  }

  # check which files are inside the GTFS. if any non text file is found, raise
  # a warning and do not try to read it as a csv. remove the '.txt' extension
  # from the text files to reference them without it in messages and errors

  files_in_gtfs <- tryCatch(
    zip::zip_list(path)$filename,
    error = function(cnd) cnd
  )
  if (inherits(files_in_gtfs, "error")) error_path_must_be_zip()

  non_text_files <- files_in_gtfs[!grepl("\\.txt$", files_in_gtfs)]

  if (!identical(non_text_files, character(0))) {
    warning(
      "Found non .txt files when attempting to read the GTFS feed: ",
      paste(non_text_files, collapse = ", "), "\n",
      "These files have been ignored and were not imported to the GTFS object.",
      call. = FALSE
    )
  }
  files_in_gtfs <- setdiff(files_in_gtfs, non_text_files)
  files_in_gtfs <- gsub("\\.txt", "", files_in_gtfs)

  # read only the text files specified either in 'files' or in 'skip'.
  # if both are NULL, read all text files

  if (!is.null(files)) {
    files_to_read <- files
  } else if (!is.null(skip)) {
    files_to_read <- setdiff(files_in_gtfs, skip)
  } else {
    files_to_read <- files_in_gtfs
  }

  # check if all specified files exist and raise an error if any does not

  missing_files <- files_to_read[! files_to_read %chin% files_in_gtfs]
  if (!identical(missing_files, character(0))) {
    error_gtfs_missing_files(missing_files)
  }

  # raise an error if a file is specified in 'fields' but does not appear in
  # 'files_to_read'

  files_misspec <- names(fields)[! names(fields) %chin% files_to_read]

  if (!is.null(files_misspec) & !identical(files_misspec, character(0))) {
    error_files_misspecified(files_misspec)
  }

  # extract text files to temporary folder to later read them
  # 'unlink()' makes sure that previous imports don't interfere with current one

  tmpdir <- file.path(tempdir(), "gtfsio")
  unlink(tmpdir, recursive = TRUE, force = TRUE)

  zip::unzip(
    path,
    files = paste0(files_to_read, ".txt"),
    exdir = tmpdir,
    overwrite = TRUE
  )

  if (!quiet)
    message(
      "Unzipped the following files to ", tmpdir, ":\n",
      paste0("  * ", files_to_read, ".txt", collapse = "\n")
    )

  # get GTFS standards to assign correct classes to each field

  gtfs_standards <- get_gtfs_standards()

  # read files into list

  gtfs <- lapply(
    X = files_to_read,
    FUN = read_files,
    gtfs_standards,
    fields,
    extra_spec,
    tmpdir,
    quiet,
    encoding
  )

  # assign names to 'gtfs', noting that zip_list may return full paths, which
  # need to be stripped here

  file_names <- vapply(
    files_to_read,
    function(i) utils::tail(strsplit(i, .Platform$file.sep)[[1]], 1),
    character(1),
    USE.NAMES = FALSE
  )

  names(gtfs) <- file_names

  # create gtfs object from 'gtfs'

  gtfs <- new_gtfs(gtfs)

  return(gtfs)

}



#' Read a GTFS text file
#'
#' Reads a GTFS text file from the main \code{.zip} file.
#'
#' @param file A string. The name of the file (without \code{.txt} extension) to
#'   be read.
#' @param gtfs_standards A named list. Created by
#'   \code{\link{get_gtfs_standards}}.
#' @param fields A named list. Passed by the user to \code{\link{import_gtfs}}.
#' @param extra_spec A named list. Passed by the user to
#'   \code{\link{import_gtfs}}.
#' @param tmpdir A string. The path to the temporary folder where GTFS text
#'   files were unzipped to.
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   TRUE).
#' @param encoding A string. Passed to \code{\link[data.table]{fread}}, defaults
#'   to \code{"unknown"}. Other possible options are \code{"UTF-8"} and
#'   \code{"Latin-1"}. Please note that this is not used to re-encode the input,
#'   but to enable handling encoded strings in their native encoding.
#'
#' @return A \code{data.table} representing the desired text file according to
#'   the standards for reading and writing GTFS feeds with R.
#'
#' @seealso \code{\link{get_gtfs_standards}}
#'
#' @keywords internal
read_files <- function(file,
                       gtfs_standards,
                       fields,
                       extra_spec,
                       tmpdir,
                       quiet,
                       encoding) {

  # create object to hold the file with '.txt' extension

  file_txt <- paste0(file, ".txt")

  if (!quiet) message("Reading ", file_txt)

  # get standards for reading and fields to be read from the given 'file'

  file_standards <- gtfs_standards[[file]]
  fields         <- fields[[file]]
  extra_spec     <- extra_spec[[file]]

  # 'extra_spec' should specify how specify how extra fields in either
  # documented or extra files are read. throw an error if it refers to
  # documented fields

  spec_both <- names(extra_spec)[names(extra_spec) %chin% names(file_standards)]

  if (any(names(extra_spec) %chin% names(file_standards))) {
    error_field_is_documented(file, spec_both)
  }

  # read 'file' first row to figure out which fields are present
  # - if 'file_standards' is NULL then file is undocumented
  # - print warning message if warning is raised and 'quiet' is FALSE

  if (is.null(file_standards) & !quiet) {
    message("  - File undocumented. Trying to read it as a csv.")
  }

  withCallingHandlers(
    {
      sample_dt <- data.table::fread(
        file.path(tmpdir, file_txt),
        nrows = 1,
        colClasses = "character"
      )
    },
    warning = function(cnd) if (!quiet) message("  - ", conditionMessage(cnd))
  )

  # if 'file' is completely empty (even without a header), return a NULL
  # 'data.table'

  if (ncol(sample_dt) == 0) return(data.table::data.table(NULL))

  # retrieve which fields are inside the file

  fields_in_file <- names(sample_dt)

  # read all fields if 'fields' is NULL. else, only the ones specified

  if (is.null(fields)) {
    fields_to_read <- fields_in_file
  } else {
    fields_to_read <- fields
  }

  # check if all specified fields exist and raise exception if any does not

  missing_fields <- fields_to_read[! fields_to_read %chin% fields_in_file]

  if (!identical(missing_fields, character(0))) {
    error_gtfs_missing_fields(file, missing_fields)
  }

  # raise an error if a field was specified in 'extra_spec' but does not appear
  # in 'fields_to_read'

  fields_misspec <- setdiff(names(extra_spec), fields_to_read)

  if (!is.null(fields_misspec) & !identical(fields_misspec, character(0))) {
    error_fields_misspec(fields_misspec)
  }

  # get the standard data types of documented fields from 'file_standards'

  doc_fields <- fields_to_read[fields_to_read %chin% names(file_standards)]

  doc_classes <- vapply(
    doc_fields,
    function(field) file_standards[[field]][[1]],
    character(1)
  )

  # set all undocumented files/fields to be read as character by default

  undoc_fields <- setdiff(fields_to_read, doc_fields)
  undoc_classes <- rep("character", length(undoc_fields))
  names(undoc_classes) <- undoc_fields

  # change how fields specified in 'extra_spec' should be read accordingly

  undoc_classes[names(extra_spec)] <- extra_spec

  # join together both documented and undocumented fields classes

  fields_classes <- c(doc_classes, undoc_classes)

  # read the file specifying the column classes
  # if a warning is thrown (e.g. due to a parsing failure) and 'quiet' is FALSE,
  # print warning message to console to help debugging (otherwise all warnings
  # messages are thrown simultaneously at the end, which doesn't help as much)

  withCallingHandlers(
    {
      full_dt <- data.table::fread(
        file.path(tmpdir, file_txt),
        select = fields_classes,
        encoding = encoding
      )
    },
    warning = function(cnd) if (!quiet) message("  - ", conditionMessage(cnd))
  )

  return(full_dt)

}


# errors ------------------------------------------------------------------


error_path_must_be_zip <- parent_function_error(
  "Could not unzip file. Please make sure 'path' points to a zip file/url.",
  subclass = "path_must_be_zip"
)

error_non_existent_file <- function(path) {
  parent_call <- sys.call(-1)
  message <- paste0("'path' points to non-existent file: '", path, "'")

  gtfsio_error(message, subclass = "non_existent_file", call = parent_call)
}

error_files_and_skip_provided <- parent_function_error(
  paste0(
    "Both 'files' and 'skip' were provided. ",
    "Please use only one of these parameters at a time."
  ),
  subclass = "files_and_skip_provided"
)

error_unsupported_extra_spec <- parent_function_error(
  "Only character, integer and numeric are supported in 'extra_spec'.",
  subclass = "unsupported_extra_spec"
)

error_gtfs_missing_files <- function(missing_files) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "The provided GTFS feed doesn't contain the following text file(s): ",
    paste0("'", missing_files, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "gtfs_missing_files", call = parent_call)
}

error_files_misspecified <- function(files_misspec) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "The following files were specified in 'fields' but either were not ",
    "specified in 'files' or do not exist: ",
    paste0("'", files_misspec, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "files_misspecified", call = parent_call)
}

error_field_is_documented <- function(file, spec_both) {
  parent_call <- sys.call(-3)
  message <- paste0(
    "The following field(s) from the '",
    file,
    "' file were specified in 'extra_spec' but are already documented in ",
    "the official GTFS reference: ",
    paste0("'", spec_both, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "field_is_documented", call = parent_call)
}

error_gtfs_missing_fields <- function(file, missing_fields) {
  parent_call <- sys.call(-3)
  message <- paste0(
    "'", file, "' doesn't contain the following field(s): ",
    paste0("'", missing_fields, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "gtfs_missing_fields", call = parent_call)
}

error_fields_misspec <- function(fields_misspec) {
  parent_call <- sys.call(-3)
  message <- paste0(
    "The following fields were specified in 'extra_spec' but either were ",
    "not specified in 'fields' or do not exist: ",
    paste0("'", fields_misspec, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "gtfs_fields_misspec", call = parent_call)
}
