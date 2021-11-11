#' Export GTFS objects
#'
#' Writes GTFS objects to disk as GTFS transit feeds. The object must be
#' formatted according to the standards for reading and writing GTFS transit
#' feeds, as specified in \code{\link{get_gtfs_standards}} (i.e. data types are
#' not checked). If present, does not write auxiliary tables held in a sub-list
#' named \code{"."}.
#'
#' @param gtfs A GTFS object.
#' @param path A string. Where the resulting \code{.zip} file must be written
#'   to.
#' @param files A character vector. The name of the elements to be written to
#'   the feed.
#' @param standard_only A logical. Whether only standard files and fields should
#'   be written (defaults to \code{TRUE}, which drops extra files and fields).
#' @param compression_level A numeric, between 1 and 9. The higher the value,
#'   the best the compression, which demands more processing time. Defaults to 9
#'   (best compression).
#' @param as_dir A logical. Whether the feed should be exported as a directory,
#'   instead of a \code{.zip} file. Defaults to \code{FALSE}.
#' @param overwrite A logical. Whether to overwrite an existing \code{.zip} file
#'   (defaults to \code{TRUE}).
#' @param quiet A logical. Whether to hide log messages and progress bars
#'   (defaults to \code{TRUE}).
#'
#' @return Invisibly returns the same GTFS object passed to \code{gtfs}.
#'
#' @seealso \code{\link{get_gtfs_standards}}
#'
#' @family io functions
#'
#' @examples
#' gtfs_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfsio")
#'
#' gtfs <- import_gtfs(gtfs_path)
#'
#' tmpf <- tempfile(pattern = "gtfs", fileext = ".zip")
#'
#' export_gtfs(gtfs, tmpf)
#' zip::zip_list(tmpf)$filename
#'
#' export_gtfs(gtfs, tmpf, files = c("shapes", "trips"))
#' zip::zip_list(tmpf)$filename
#'
#' @export
export_gtfs <- function(gtfs,
                        path,
                        files = NULL,
                        standard_only = FALSE,
                        compression_level = 9,
                        as_dir = FALSE,
                        overwrite = TRUE,
                        quiet = TRUE) {

  gtfs_standards <- get_gtfs_standards()

  # basic input checking

  assert_class(gtfs, "gtfs")
  assert_vector(path, "character", len = 1L)
  assert_vector(files, "character", null_ok = TRUE)
  assert_vector(standard_only, "logical", len = 1L)
  assert_vector(compression_level, "numeric", len = 1L)
  assert_vector(as_dir, "logical", len = 1L)
  assert_vector(overwrite, "logical", len = 1L)
  assert_vector(quiet, "logical", len = 1L)

  if (path == tempdir()) error_tempfile_misused()

  # input checks that depend on more than one argument

  if (file.exists(path) & !overwrite) error_cannot_overwrite()
  if (!as_dir & !grepl("\\.zip$", path)) error_ext_must_be_zip()
  if (as_dir & grepl("\\.zip$", path)) error_path_must_be_dir()

  extra_files <- setdiff(files, names(gtfs_standards))
  if (standard_only & !is.null(files) & !identical(extra_files, character(0))) {
    error_non_standard_files(extra_files)
  }

  # if files is NULL then all 'gtfs' elements should be written

  if (is.null(files)) files <- names(gtfs)

  # remove '.' from 'files' if it exists ({tidytransit} may place some auxiliary
  # tables in a sub-list named '.')

  files <- setdiff(files, ".")

  # remove extra files from 'files' if 'standard_only' is set to TRUE
  # 'extra_files' is re-evaluated because 'files' might have changed in the
  # lines above

  extra_files <- setdiff(files, names(gtfs_standards))

  if (standard_only) files <- setdiff(files, extra_files)

  # throw an error if a specified file is not an element of 'gtfs'

  missing_files <- setdiff(files, names(gtfs))

  if (!identical(missing_files, character(0))) {
    error_missing_specified_file(missing_files)
  }

  # write files either to a temporary directory (if as_dir = FALSE), or to path
  # (if as_dir = TRUE)

  if (as_dir) {
    tmpd <- path
  } else {
    tmpd <- tempfile(pattern = "gtfsio")
  }

  unlink(tmpd, recursive = TRUE)
  dir.create(tmpd)

  # write files to 'tmpd'

  if (!quiet) message("Writing text files to ", tmpd)

  for (file in files) {

    filename <- paste0(file, ".txt")
    filepath <- file.path(tmpd, filename)

    if (!quiet) message("  - Writing ", filename)

    dt <- gtfs[[file]]

    # if 'standard_only' is set to TRUE, remove non-standard fields from 'dt'
    # before writing it to disk

    if (standard_only) {

      file_cols  <- names(dt)
      extra_cols <- setdiff(file_cols, names(gtfs_standards[[file]]))

      if (!identical(extra_cols, character(0))) dt <- dt[, !..extra_cols]

    }

    # print warning message if warning is raised and 'quiet' is FALSE
    withCallingHandlers(
      data.table::fwrite(dt, filepath),
      warning = function(cnd) {
        if (!quiet) message("    - ", conditionMessage(cnd))
      }
    )

  }

  # zip the contents of 'tmpd' to 'path', if as_dir = FALSE
  # remove the file/directory in 'path' (an error would already have been thrown
  # if 'path' pointed to an existing file that should not be overwritten).
  # this action prevents zip::zip() from crashing R when 'path' exists, but is a
  # directory, not a file
  # related issue: https://github.com/r-lib/zip/issues/76

  if (!as_dir) {

    unlink(path, recursive = TRUE)

    filepaths <- file.path(tmpd, paste0(files, ".txt"))

    zip::zip(
      path,
      filepaths,
      compression_level = compression_level,
      mode = "cherry-pick"
    )

    if (!quiet) message("GTFS object successfully zipped to ", path)

  }

  return(invisible(gtfs))

}


# errors ------------------------------------------------------------------


#' @include gtfsio_error.R
error_tempfile_misused <- parent_function_error(
  paste0(
    "Please use 'tempfile()' instead of 'tempdir()' to designate ",
    "temporary directories."
  ),
  subclass = "tempfile_misused"
)

error_cannot_overwrite <- parent_function_error(
  paste0(
    "'path' points to an existing file/directory, ",
    "but 'overwrite' is set to FALSE."
  ),
  subclass = "cannot_overwrite_file"
)

error_ext_must_be_zip <- parent_function_error(
  paste0(
    "'path' must have '.zip' extension. ",
    "If you meant to create a directory please set 'as_dir' to TRUE."
  ),
  subclass = "ext_must_be_zip"
)

error_path_must_be_dir <- parent_function_error(
  "'path' cannot have '.zip' extension when 'as_dir' is TRUE.",
  subclass = "path_must_be_dir"
)

error_non_standard_files <- function(extra_files) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "Non-standard file specified in 'files', ",
    "even though 'standard_only' is set to TRUE: ",
    paste0("'", extra_files, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "non_standard_files", call = parent_call)
}

error_missing_specified_file <- function(missing_files) {
  parent_call <- sys.call(-1)
  message <- paste0(
    "The provided GTFS object does not contain the following ",
    "elements specified in 'files': ",
    paste0("'", missing_files, "'", collapse = ", ")
  )

  gtfsio_error(message, subclass = "missing_specified_file", call = parent_call)
}
