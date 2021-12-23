#' gtfsio: Read and Write General Transit Feed Specification (GTFS) Data
#'
#' Tools for the development of GTFS-related packages. Establishes a standard
#' for representing GTFS feeds using R data types. Provides fast and flexible
#' functions to read and write GTFS feeds while sticking to this standard.
#' Defines a basic \code{gtfs} class which is meant to be extended by packages
#' that depend on it. And offers utility functions that support checking the
#' structure of GTFS objects.
#'
#' @docType package
#' @keywords internal
#'
#' @importFrom data.table %chin% :=
"_PACKAGE"

# prevent data.table's NSE-related notes
utils::globalVariables(c("..fields", "..extra_cols"))
