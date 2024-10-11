remove_file_ext = function(file) {
  fs::path_ext_remove(file)
}

append_file_ext = function(file) {
  vapply(file, function(.f) {
    file_ext <- gtfsio::gtfs_reference[[remove_file_ext(.f)]][["file_ext"]]
    if (is.null(file_ext)) {
      # use default for argument-specified non-standard files,
      # behaviour defined in test_import_gtfs.R#292
      file_ext <- "txt"
    }
    if (!has_extension(.f, file_ext)) {
      .f <- fs::path_ext_set(.f, file_ext)
    }
    return(.f)
  }, ".txt", USE.NAMES = FALSE)
}

#' Vectorized assertion of path extensions
#'
#' @param path Vector of file paths
#' @param ext File extension to be asserted for each `path`
#'
#' @return Logical vector of same length as `path`, with `TRUE` for each element
#' with specified extension, `FALSE` otherwise.
#'
#' @noRd
has_extension <- function(path, ext = "zip") {
  fs::path_ext(path) == ext
}
