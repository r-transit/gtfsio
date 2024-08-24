#' Parse the official GTFS reference markdown file and create a table
#' that assigns every GTFS field with an R-equivalent gtfsio datatype

library(dplyr)
source("parse_markdown.R")

# Parse current reference markdown to list of tables ####
fields_ref_list <- parse_fields("https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md")

# Bind list to table table containing all fields ####
f <- bind_fields_reference_list(fields_ref_list)

# Link gtfs types to R types ####
f$gtfsio_type <- NA

# Enum
f$gtfsio_type[f$Type == "Enum"] <- "integer"
# Correct non-integer enums (manual fix)
f[f$file == "translations.txt" & f$Field_Name == "table_name","gtfsio_type"] <- "character"

# ID: character
f$gtfsio_type[startsWith(f$Type, "Foreign ID")] <- "character"
f$gtfsio_type[startsWith(f$Type, "ID referencing")] <- "character"
f$gtfsio_type[f$Type %in% c("ID", "Foreign ID", "Unique ID")] <- "character"

# Text/Strings
f$gtfsio_type[f$Type %in% c("Text", "String")] <- "character"
f$gtfsio_type[f$Type %in% c("URL", "Language code", "Currency code", "Email",
                            "Phone number", "Timezone", "Color",
                            "Text or URL or Email or Phone number")] <- "character"

# Date and Time
f$gtfsio_type[f$Type == "Date"] <- "integer"
f$gtfsio_type[f$Type == "Time"] <- "character"

# Numerics
f$gtfsio_type[f$Type %in% c("Latitude", "Longitude", "Non-negative float",
                            "Positive float", "Float", "Currency amount")] <- "numeric"
f$gtfsio_type[f$Type %in% c("Non-negative integer", "Non-zero integer",
                            "Positive integer", "Non-null integer", "Integer")] <- "integer"

# Geojson
f$gtfsio_type[f$Type == "Array"] <- "geojson_array"
f$gtfsio_type[f$Type == "Object"] <- "geojson_object"

f$Field_Name[f$file == "locations.geojson"] <- gsub("&nbsp;", "", f$Field_Name[f$file == "locations.geojson"])
f$Field_Name[f$file == "locations.geojson"] <- gsub("\\\\", "", f$Field_Name[f$file == "locations.geojson"])
f$Field_Name[f$file == "locations.geojson"] <- gsub("-", "", f$Field_Name[f$file == "locations.geojson"])

if(any(is.na(f$gtfsio_type))) {
  stop("GTFS types without R equivalent found:\n", paste0(unique(f$Type[is.na(f$gtfsio_type)]), collapse = ", "))
}
stopifnot(all(!is.na(f$gtfsio_type)))

# save copy of table data ####
gtfsio_field_types = as.data.frame(fields)
write.csv(gtfsio_field_types, "gtfsio_field_conversion_types.csv", row.names = FALSE, eol = "\r", fileEncoding = "UTF-8")
usethis::use_data(gtfsio_field_types, internal = T, overwrite = T)
