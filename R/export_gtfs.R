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

  if (!inherits(gtfs, "gtfs"))
    stop("'gtfs' must inherit from the 'gtfs' class.")

  if (!is.character(path) | length(path) != 1)
    stop("'path' must be a string (a character vector of length 1).")

  if (!is.null(files) & !is.character(files))
    stop("'files' must either be a character vector or NULL.")

  if (!is.logical(standard_only) | length(standard_only) != 1)
    stop("'standard_only' must be a logical vector of length 1.")

  if (!is.numeric(compression_level) | length(compression_level) != 1)
    stop("'compression_level' must be a numeric vector of length 1.")

  if (!is.logical(as_dir) | length(as_dir) != 1)
    stop("'as_dir' must be a logical vector of length 1.")

  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("'overwrite' must be a logical vector of length 1.")

  if (!is.logical(quiet) | length(quiet) != 1)
    stop("'quiet' must be a logical vector of length 1.")

  # input checks that depend on more than one argument

  if (file.exists(path) & !overwrite)
    stop(
      "'path' points to an existing file/directory, ",
      "but 'overwrite' is set to FALSE."
    )

  if (!as_dir & !grepl("\\.zip$", path))
    stop(
      "'path' must have '.zip' extension. ",
      "If you meant to create a directory please set 'as_dir' to TRUE."
    )

  extra_files <- setdiff(files, names(gtfs_standards))
  if (standard_only & !is.null(files) & !identical(extra_files, character(0)))
    stop(
      "Non-standard file specified in 'files', ",
      "even though 'standard_only' is set to TRUE: ",
      paste0("'", extra_files, "'", collapse = ", ")
    )

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

  if (!identical(missing_files, character(0)))
    stop(
      "The provided GTFS object does not contain the following ",
      "elements specified in 'files': ",
      paste0("'", missing_files, "'", collapse = ", ")
    )

  # create temp directory where files should be written to

  tmpd <- tempfile(pattern = "gtfsio")
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

    data.table::fwrite(dt, filepath)

  }

  # write result to 'path'
  # remove the file/directory in 'path' (an error would already have been thrown
  # if 'path' pointed to an existing file that should not be overwritten).
  # this action prevents zip::zip() from crashing R when 'path' exists, but is a
  # directory, not a file
  # related issue: https://github.com/r-lib/zip/issues/76

  unlink(path, recursive = TRUE)

  # if 'as_dir' is TRUE, move 'tmpd' to 'path'. else, zip its content to 'path'

  if (as_dir) {

    unlink(path, recursive = TRUE)
    file.rename(tmpd, path)

    if (!quiet)
      message("GTFS directory successfully moved from ", tmpd, " to ", path)

  } else {

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
