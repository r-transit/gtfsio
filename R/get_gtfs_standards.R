#' Generate GTFS standards
#'
#' @description
#' Generates a list specifying the standards to be used when reading and writing
#' GTFS feeds with R. Each list element (also a list) represents a distinct GTFS
#' table, and describes:
#'
#' - the fields that compose the table, including which R data type is best
#' suited to represent it, whether the field is required, optional or
#' conditionally required.
#' - whether the table is required, optional or conditionally required (as an
#' attribute)
#'
#' @return A named list, in which each element represents the R equivalent of
#'   each GTFS table standard.
#'
#' @section Details:
#' GTFS standards were derived from [GTFS Schedule
#' Reference](https://gtfs.org/schedule/reference/). The R data types chosen to
#' represent each GTFS data type are described below:
#' `r .doc_field_types()`
#'
#' @examples
#' gtfs_standards <- get_gtfs_standards()
#'
#' @export
get_gtfs_standards <- function() {
  files_list <- split(gtfsio_field_types, gtfsio_field_types$file)
  files_list <- lapply(files_list, function(feed_file) {
    type = feed_file$gtfsio_type
    names(type) <- feed_file$field_name
    type
  })
  return(files_list)
}

get_field_types = function(file) {
  if(endsWith(file, ".geojson") || endsWith(file, ".txt")) stop()
  types = gtfsio_field_types$gtfsio_type[gtfsio_field_types$file == file]
  names(types) <- gtfsio_field_types$field_name[gtfsio_field_types$file == file]
  stopifnot(!is.null(names(types)))
  return(types)
}

.doc_field_types = function() { # nocov start
  type_assignment <- unique(gtfsio_field_types[,c("Type", "gtfsio_type")])
  type_assignment <- type_assignment[!startsWith(type_assignment$Type, "Foreign ID"),]
  type_assignment <- type_assignment[order(type_assignment$gtfsio_type),]

  doc <- c("\\itemize{",
           paste0("\\item{", type_assignment$Type, " = \`",
                  type_assignment$gtfsio_type, "\`}"),
           "}\n")

  return(paste(doc, collapse = "\n"))
} # nocov end
