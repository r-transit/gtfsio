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
#' @param quiet A logical. Whether to hide log messages and progress bars
#'   (defaults to \code{TRUE}).
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
                        quiet = TRUE) {

  # input checking ('files', 'fields' and 'extra_spec' are more thoroughly
  # validated further down the code)

  if (!is.character(path) | length(path) > 1)
    stop("'path' must be a character vector of length 1.")

  if (!grepl("\\.zip$", path)) stop("'path' must have '.zip' extension.")

  path_is_url <- grepl("^http[s]?://.*", path)

  if (!path_is_url & !file.exists(path))
    stop("'path' points to non-existent file: '", path, "'")

  if (!is.logical(quiet) | length(quiet) != 1)
    stop("'quiet' must be a logical vector of length 1.")

  if (!is.null(extra_spec) & !is.list(extra_spec))
    stop("'extra_spec' must be either a list or NULL.")

  for (input_types in extra_spec)
    if (any(! input_types %chin% c("character", "integer", "numeric")))
      stop("Only character, integer and numeric are supported in 'extra_spec'.")

  if (!is.null(files) & !is.character(files))
    stop("'files' must be either a character vector or NULL.")

  if (!is.null(fields) & !is.list(fields))
    stop("'fields' must be either a list or NULL.")

  # if 'path' is an URL, download it and save path to downloaded file to 'path'

  if (path_is_url) {

    tmp <- tempfile(pattern = "gtfs", fileext = ".zip")
    utils::download.file(path, tmp, method = "auto", quiet = quiet)

    if (!quiet) message("File downloaded to ", tmp, ".")

    path <- tmp

  }

  # retrieve which files are inside the GTFS and remove '.txt' from their names

  files_in_gtfs <- zip::zip_list(path)$filename
  files_in_gtfs <- gsub(".txt", "", files_in_gtfs)

  # read all text files if 'files' is NULL. else, only the ones specified

  if (is.null(files))
    files_to_read <- files_in_gtfs
  else
    files_to_read <- files

  # check if all specified files exist and raise an error if any does not

  missing_files <- files_to_read[! files_to_read %chin% files_in_gtfs]

  if (!identical(missing_files, character(0)))
    stop(
      "The provided GTFS feed doesn't contain the following text file(s): ",
      paste0("'", missing_files, "'", collapse = ", ")
    )

  # raise an error if a file is specified in 'fields' but does not appear in
  # 'files_to_read'

  files_misspec <- names(fields)[! names(fields) %chin% files_to_read]

  if (!is.null(files_misspec) & !identical(files_misspec, character(0)))
    stop(
      "The following files were specified in 'fields' but either were not ",
      "specified in 'files' or do not exist: ",
      paste0("'", files_misspec, "'", collapse = ", ")
    )

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
    quiet
  )

  # assign names to 'gtfs'

  names(gtfs) <- files_to_read

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
                       quiet) {

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

  if (any(names(extra_spec) %chin% names(file_standards)))
    stop(
      "The following field(s) from the '",
      file,
      "' file were specified in 'extra_spec' but are already documented in ",
      "the official GTFS reference: ",
      paste0("'", spec_both, "'", collapse = ", ")
    )

  # read 'file' first row to figure out which fields are present
  # if 'file_standards' is NULL then file is undocumented

  if (is.null(file_standards) & !quiet)
    message("  - File undocumented. Trying to read it as a csv.")

  sample_dt <- data.table::fread(file.path(tmpdir, file_txt), nrows = 1)

  # if 'file' is completely empty (even without a header), return a NULL
  # 'data.table'

  if (ncol(sample_dt) == 0) {

    if (!quiet) message("  - File is empty. Returning a NULL data.table")
    return(data.table::data.table(NULL))

  }

  # retrieve which fields are inside the file

  fields_in_file <- names(sample_dt)

  # read all fields if 'fields' is NULL. else, only the ones specified

  if (is.null(fields))
    fields_to_read <- fields_in_file
  else
    fields_to_read <- fields

  # check if all specified fields exist and raise exception if any does not

  missing_fields <- fields_to_read[! fields_to_read %chin% fields_in_file]

  if (!identical(missing_fields, character(0)))
    stop(
      "'", file, "' doesn't contain the following field(s): ",
      paste0("'", missing_fields, "'", collapse = ", ")
    )

  # raise an error if a field was specified in 'extra_spec' but does not appear
  # in 'fields_to_read'

  fields_misspec <- setdiff(names(extra_spec), fields_to_read)

  if (!is.null(fields_misspec) & !identical(fields_misspec, character(0)))
    stop(
      "The following fields were specified in 'extra_spec' but either were ",
      "not specified in 'fields' or do not exist: ",
      paste0("'", fields_misspec, "'", collapse = ", ")
    )

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
  # if a warning is thrown (e.g. due to a parsing failure) and 'quiet' != FALSE,
  # print warning message to console to help debugging (otherwise all warnings
  # messages are thrown simultaneously at the end, which doesn't help as much)

  withCallingHandlers(
    {
      full_dt <- data.table::fread(
        file.path(tmpdir, file_txt),
        select = fields_classes
      )
    },
    warning = function(cnd) if(!quiet) message("  - ", conditionMessage(cnd))
  )

  return(full_dt)

}
