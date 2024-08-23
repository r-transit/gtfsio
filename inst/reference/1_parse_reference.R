library(dplyr)

# Implemented according to documentation in get_gtfs_standards()
#' - Color = `character` (ok)
#' - Currency amount = `numeric` (ok)
#' - Currency code = `character` (ok)
#' - Date = `integer` (ok)
#' - Email = `character` (ok)
#' - ENUM = `integer` (except manual fix)
#' - ID = `character` (ok)
#' - Integer = `integer` (ok)
#' - Language code = `character` (ok)
#' - Latitude = `numeric` (ok)
#' - Longitude = `numeric` (ok)
#' - Float = `numeric` (ok)
#' - Phone number = `character` (ok)
#' - Text = `character` (ok)
#' - Time = `character` (ok)
#' - Timezone = `character` (ok)
#' - URL = `character` (ok)

source("parse_markdown.R")

# Parse current reference
ref = parse_fields("https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md")

# table containing all fields
fields = bind_rows(ref, .id = "file") |>
  rename(Field_Name = `Field Name`) |>
  mutate(Field_Name = gsub("`", "", Field_Name)) |>
  mutate(Presence = gsub("**", "", Presence, fixed = TRUE)) |>
  select(-Description)

# Link gtfs types to R types ####
fields$gtfsio_type <- NA

# Enum
fields$gtfsio_type[fields$Type == "Enum"] <- "integer"
# Correct non-integer enums (manual fix)
fields[fields$file == "translations.txt" & fields$Field_Name == "table_name","gtfsio_type"] <- "character"

# ID: character
fields$gtfsio_type[startsWith(fields$Type, "Foreign ID")] <- "character"
fields$gtfsio_type[startsWith(fields$Type, "ID referencing")] <- "character"
fields$gtfsio_type[fields$Type %in% c("ID", "Foreign ID", "Unique ID")] <- "character"

# Text/Strings
fields$gtfsio_type[fields$Type %in% c("Text", "String")] <- "character"
fields$gtfsio_type[fields$Type %in% c("URL", "Language code", "Currency code", "Email",
                                 "Phone number", "Timezone", "Color",
                                 "Text or URL or Email or Phone number")] <- "character"

# Date and Time
fields$gtfsio_type[fields$Type == "Date"] <- "integer"
fields$gtfsio_type[fields$Type == "Time"] <- "character"

# Numerics
fields$gtfsio_type[fields$Type %in% c("Latitude", "Longitude", "Non-negative float",
                                 "Positive float", "Float", "Currency amount")] <- "numeric"
fields$gtfsio_type[fields$Type %in% c("Non-negative integer", "Non-zero integer",
                                 "Positive integer", "Non-null integer", "Integer")] <- "integer"

# Geojson
fields$gtfsio_type[fields$Type == "Array"] <- "geojson_array"
fields$gtfsio_type[fields$Type == "Object"] <- "geojson_object"

fields$Field_Name[fields$file == "locations.geojson"] <- gsub("&nbsp;", "", fields$Field_Name[fields$file == "locations.geojson"])
fields$Field_Name[fields$file == "locations.geojson"] <- gsub("\\\\", "", fields$Field_Name[fields$file == "locations.geojson"])
fields$Field_Name[fields$file == "locations.geojson"] <- gsub("-", "", fields$Field_Name[fields$file == "locations.geojson"])

stopifnot(all(!is.na(fields$gtfsio_type)))

# Presence ####
# TODO check
fields$gtfsio_presence <- NA
fields$gtfsio_presence[fields$Presence %in% c("Required")] <- "required"
fields$gtfsio_presence[fields$Presence %in% c("Conditionally Required", "Conditionally Forbidden")] <- "conditional"
fields$gtfsio_presence[fields$Presence %in% c("Optional", "Recommended")] <- "optional"

stopifnot(all(!is.na(fields$gtfsio_presence)))

# Save table data ####
write.csv(fields, "../../data/gtfsio_field_conversion_types.csv", row.names = FALSE)

# Save gtfsio_standard_data ####
gtfs_standards_parsed = fields |>
  split(fields$file) |>
  lapply(\(feed_file) {
    setNames(feed_file$gtfsio_type, feed_file$Field_Name)
  })

# save(gtfs_standards_parsed, file = "../../data/gtfs_standards_parsed.rda")
usethis::use_data(gtfs_standards_parsed, internal = T, overwrite = T)
