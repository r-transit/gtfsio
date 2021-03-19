#' gtfsio: Read and Write General Transit Feed Specification (GTFS) Data
#'
#' Provides a \code{gtfs} class to represent General Transit Feed Specification
#' (GTFS) files, and fast and flexible tools to read and write such files.
#'
#' @docType package
#' @name gtfsio
#' @aliases gtfsio-package
#'
#' @importFrom data.table %chin% :=
"_PACKAGE"

# prevent data.table's NSE-related notes
utils::globalVariables(c("..fields", "..extra_cols"))
