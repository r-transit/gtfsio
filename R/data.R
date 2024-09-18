#' GTFS reference
#'
#' The data from the official GTFS specification document parsed to a list. Revision date:
#' ``r attributes(gtfs_reference)$revision_date``.
#'
#' @format
#' A list with data for every GTFS file. Each named list element (also a list) has
#' specifications for one GTFS file in the following structure:
#' \itemize{
#'   \item{`File_Name`: file name including file extension (txt or geojson)}
#'   \item{`File_Presence`: Presence condition applied to the file}
#'   \item{`file`: file name without file extension}
#'   \item{`file_ext`: file extension}
#'   \item{`fields`: data.frame with parsed field specification (columns:
#'         `Field_Name`, `Type`, `Presence`, `Description`, `gtfsio_type`)}
#'   \item{`primary_key`: primary key as vector}
#'   \item{`field_types`: named vector on how GTFS types (values) should be read in gtfsio
#'                        (names). Values are the same as in `fields`.}
#' }
#'
#' @details
#' GTFS Types are converted to R types in gtfsio according to the following list:
#' `r .doc_field_types()`
#'
#' @source [https://github.com/google/transit/blob/master/gtfs/spec/en/reference.md](https://github.com/google/transit/blob/master/gtfs/spec/en/reference.md)
#' @keywords data
"gtfs_reference"

.doc_field_types = function() { # nocov start
  fields <- lapply(gtfsio::gtfs_reference, `[[`, "fields")
  fields <- do.call("rbind", fields)

  type_assignment <- unique(fields[,c("Type", "gtfsio_type")])
  type_assignment <- type_assignment[!startsWith(type_assignment$Type, "Foreign ID"),]
  type_assignment <- type_assignment[order(type_assignment$gtfsio_type),]

  type_assignment <-  lapply(split(type_assignment, type_assignment$Type), function(ta) {
    if(nrow(ta) > 1) {
      ta$gtfsio_type <- paste0(ta$gtfsio_type, collapse = ", ")
      ta <- ta[1,]
    }
    ta
  })
  type_assignment <- do.call("rbind", type_assignment)

  doc <- c("\\itemize{",
           paste0("\\item{", type_assignment$Type, " = \`",
                  type_assignment$gtfsio_type, "\`}"),
           "}\n")

  return(paste(doc, collapse = "\n"))
} # nocov end
