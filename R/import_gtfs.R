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
#' @param quiet A logical. Whether to hide log messages and progress bars
#'   (defaults to \code{TRUE}).
#'
#' @return A GTFS object: a named list of dataframes, each one corresponding to
#'   a distinct text file from the given GTFS feed.
#'
#' @seealso \code{\link{get_gtfs_standards}}
#'
#' @family io
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
import_gtfs <- function(path, files = NULL, fields = NULL, quiet = TRUE) {

  # input checking ('files' and 'fields' are validated further down the code)

  if (!is.character(path) | length(path) > 1)
    stop("'path' must be a character vector of length 1.")

  if (!grepl("\\.zip$", path)) stop("'path' must have '.zip' extension.")

  path_is_url <- grepl("^http[s]?://.*", path)

  if (!path_is_url & !file.exists(path))
    stop("'path' points to non-existent file: '", path, "'")

  if (!is.logical(quiet) | length(quiet) > 1)
    stop("'quiet' must be a logical vector of length 1.")

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

  # check if all specified files exist and raise exception if any does not

  missing_files <- files_to_read[! files_to_read %chin% files_in_gtfs]

  if (!identical(missing_files, character(0)))
    warning(
      "The provided GTFS feed doesn't contain the following text file(s): ",
      paste0("'", missing_files, "'", collapse = ", ")
    )

  # remove 'missing_files' from 'files_to_read' and raise error if no valid
  # files were specified (if this errors is not thrown here, zip::unzip() will
  # fail because it will atempt to unzip a file called '.txt')

  files_to_read <- setdiff(files_to_read, missing_files)

  if (identical(files_to_read, character(0)))
    stop("The provided GTFS feed doesn't contain any of the specified files.")

  # raise exception if a file is specified in 'files' but does not appear in
  # 'files_to_read'

  files_misspec <- names(fields)[! names(fields) %chin% files_to_read]

  if (!is.null(files_misspec) & !identical(files_misspec, character(0)))
    warning(
      "The following files were specified in 'fields' but either were not ",
      "specified in 'files' or do no exist: ",
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
    tmpdir,
    quiet
  )

  # assign names to 'gtfs'

  names(gtfs) <- files_to_read

  # create gtfs object from 'gtfs'

  gtfs <- new_gtfs(gtfs, names = files_to_read)

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
read_files <- function(file, gtfs_standards, fields, tmpdir, quiet) {

  # create object to hold the file with '.txt' extension

  file_txt <- paste0(file, ".txt")

  if (!quiet) message("Reading ", file_txt)

  # get standards for reading and fields to be read from the given 'file'

  file_standards <- gtfs_standards[[file]]
  fields <- fields[[file]]

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
    warning(
      "'", file, "' doesn't contain the following field(s): ",
      paste0("'", missing_fields, "'", collapse = ", ")
    )

  # remove 'missing_fields' from 'fields_to_read' and return a NULL 'data.table'
  # if no valid fields were specified

  fields_to_read <- setdiff(fields_to_read, missing_fields)

  if (identical(fields_to_read, character(0))) {

    if (!quiet)
      message("  - No valid fields were provided. Returning a NULL data.table")

    return(data.table::data.table(NULL))

  }

  # get the standard data types of documented fields from 'file_standards'

  doc_fields <- fields_to_read[fields_to_read %chin% names(file_standards)]

  doc_classes <- vapply(
    doc_fields,
    function(field) file_standards[[field]][[1]],
    character(1)
  )

  # set all undocumented files/fields to be read as character

  undoc_fields <- setdiff(fields_to_read, doc_fields)
  undoc_classes <- rep("character", length(undoc_fields))
  names(undoc_classes) <- undoc_fields

  # join together both documented and undocumented fields classes

  fields_classes <- c(doc_classes, undoc_classes)

  # read the file specifying the column classes

  full_dt <- data.table::fread(
    file.path(tmpdir, file_txt),
    select = fields_classes
  )

  return(full_dt)

}
