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
#' @return A GTFS object: a named \code{list} of \code{data.table} objects, each
#'   table corresponding to a distinct text file from the given GTFS feed.
#'
#' @seealso \code{\link{get_gtfs_standards}}
#'
#' @family io
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

    if (!quiet) cli::cli_alert_success(paste0("File downloaded to ", tmp, "."))

    path <- tmp

  }

  # GTFS standards

  gtfs_standards <- get_gtfs_standards()

}
