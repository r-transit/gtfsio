#' Generate GTFS standards
#'
#' @description
#' Generates a list specifying the standards to be used when reading and writing
#' GTFS feeds with R. Each list element (also a list) represents a distinct GTFS
#' table, and describes:
#'
#' - whether the table is required, optional or conditionally required;
#' - the fields that compose the table, including which R data type is best
#' suited to represent it, whether the field is required, optional or
#' conditionally required, and which values it can assume (most relevant to GTFS
#' `ENUM`s.
#'
#' Note: the standards list is based on the specification as revised in May 9th,
#' 2022.
#'
#' @return A named list, in which each element represents the R equivalent of
#'   each GTFS table standard.
#'
#' @section Details:
#' GTFS standards were derived from [GTFS Schedule
#' Reference](https://gtfs.org/schedule/reference/). The R data types chosen to
#' represent each GTFS data type are described below:
#'
#' - Color = `character`
#' - Currency amount = `numeric`
#' - Currency code = `character`
#' - Date = `integer`
#' - Email = `character`
#' - ENUM = `integer`
#' - ID = `character`
#' - Integer = `integer`
#' - Language code = `character`
#' - Latitude = `numeric`
#' - Longitude = `numeric`
#' - Float = `numeric`
#' - Phone number = `character`
#' - Text = `character`
#' - Time = `character`
#' - Timezone = `character`
#' - URL = `character`
#'
#' @examples
#' gtfs_standards <- get_gtfs_standards()
#'
#' @export
get_gtfs_standards <- function() {
  return(gtfs_standards_parsed)
}
