#' Export GTFS objects
#'
#' Writes GTFS objects to disk as GTFS transit feeds. The object must be
#' formatted according to the standards for reading and writing GTFS transit
#' feeds, as specified in \code{\link{get_gtfs_standards}}. If present, does not
#' write auxiliary tables held in a sub-list named \code{"."}.
#'
#' @param gtfs A GTFS object.
#' @param path A string. Where the resulting \code{.zip} file must be written
#'   to.
#' @param files A character vector. The name of the elements to be written to
#'   the feed.
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
#' export_gtfs(gtfs, tempfile(fileext = ".zip"))
#'
#' @export
export_gtfs <- function(gtfs,
                        path,
                        files = NULL,
                        overwrite = TRUE,
                        quiet = TRUE) {

  # input checking

  if (!inherits(gtfs, "gtfs"))
    stop("'gtfs' must inherit from the 'gtfs' class.")

  if (!is.character(path) | length(path) != 1)
    stop("'path' must be a string (a character vector of length 1).")

  if (!grepl("\\.zip$", path))
    stop("'path' must have '.zip' extension.")

  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("'overwrite' must be a logical vector of length 1.")

  if (file.exists(path) & !overwrite)
    stop("The file pointed by 'path' exists, but 'overwrite' is set to FALSE.")

  if (!is.logical(quiet) | length(quiet) != 1)
    stop("'quiet' must be a logical vector of length 1.")

  if (!is.null(files) & !is.character(files))
    stop("'files' must either be 'NULL' or a character vector.")

  # if files is NULL then all 'gtfs' elements should be written

  if (is.null(files)) files <- names(gtfs)

  # remove '.' from 'files' if it exists (e.g. {tidytransit} may place some
  # auxiliary tables in a sub-list named '.')

  files <- setdiff(files, ".")

  # throw an error if a specified file is not an element of 'gtfs'

  missing_files <- setdiff(files, names(gtfs))

  if (!identical(missing_files, character(0)))
    stop(
      "The provided GTFS object does not contain the following ",
      "elements specified in 'files': ",
      paste0("'", missing_files, "'", collapse = ", ")
    )

}
