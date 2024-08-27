#' Parse the official GTFS reference markdown file and create a table
#' that assigns every GTFS field with an R-equivalent gtfsio datatype

library(dplyr)
source("parse_markdown.R")
reference.md = curl::curl_download("https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md", tempfile())

# Parse current reference markdown to list of tables and bind it ####
reference_fields = parse_fields(reference.md)
f <- bind_fields_reference_list(reference_fields)

# Link gtfs types to R types ####
f$gtfsio_type <- NA

# Enum
f$gtfsio_type[f$Type == "Enum"] <- "integer"
# Correct non-integer enums (manual fix)
f[f$File_Name == "translations.txt" & f$Field_Name == "table_name","gtfsio_type"] <- "character"

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

f$Field_Name[f$File_Name == "locations.geojson"] <- gsub("&nbsp;", "", f$Field_Name[f$File_Name == "locations.geojson"])
f$Field_Name[f$File_Name == "locations.geojson"] <- gsub("\\\\", "", f$Field_Name[f$File_Name == "locations.geojson"])
f$Field_Name[f$File_Name == "locations.geojson"] <- gsub("-", "", f$Field_Name[f$File_Name == "locations.geojson"])

if(any(is.na(f$gtfsio_type))) {
  stop("GTFS types without R equivalent found:\n", paste0(unique(f$Type[is.na(f$gtfsio_type)]), collapse = ", "))
}

# Rename columns, add file column without file extension ####
f <- f |>
  mutate(file = gsub("\\.txt$", "", gsub("\\.geojson$", "", File_Name))) |>
  as.data.frame()

# Parse reference file data ####
gtfs_reference_files = cleanup_files_reference(parse_files(reference.md))
gtfs_reference_files <- gtfs_reference_files |>
  tidyr::separate(File_Name, c("file", "file_ext"), sep = "\\.", remove = F) |>
  select(File_Name, File_Presence, file, file_ext) |>
  as.data.frame()

# Check file presence ####
file_presence1 = lapply(reference_fields, \(file) {
  trimws(gsub("\\*", "", attributes(file)$presence))
})
file_presence2 = as.list(gtfs_reference_files$File_Presence)
names(file_presence2) <- gtfs_reference_files$File_Name
stopifnot(identical(file_presence2, file_presence1))
rm(file_presence1); rm(file_presence2)

# Extract primary keys ####
primary_keys = lapply(reference_fields, \(file) {
  pk = attributes(file)$primary_key
  if(is.null(pk)) return(NULL)
  pk <- gsub("`", "", pk)
  pk <- gsub('\\"', "", pk)
  pk <- stringr::str_split_1(pk, ",")
  trimws(pk)
})

# Create gtfs_reference data object ####
gtfs_reference = gtfs_reference_files |>
  split(gtfs_reference_files$file) |>
  lapply(as.list)

for(file in names(gtfs_reference)) {
  fields = f[f$file == file,]
  fields <- select(fields, -file, -File_Name)
  gtfs_reference[[file]]$fields <- fields
  gtfs_reference[[file]][["primary_key"]] <- primary_keys[[file]]

  field_types = fields$gtfsio_type
  names(field_types) <- fields$Field_Name
  gtfs_reference[[file]][["field_types"]] <- field_types
}

usethis::use_data(gtfs_reference, internal = F, overwrite = T)
