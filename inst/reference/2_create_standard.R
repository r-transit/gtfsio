# gtfs_standard = get_gtfs_standards()
# saveRDS(gtfs_standard, "gtfs_standards.rds")
standards_expected = readRDS("gtfs_standards.rds")

load("../../data/gtfsio_field_conversion_types.rda")

str(standards_expected, 1)
str(standards_expected$agency, 1)

standards_parsed = fields |>
  split(fields$file) |>
  lapply(\(feed_file) {
    setNames(feed_file$gtfsio_type, feed_file$Field_Name)
    })

saveRDS(standards_parsed, "../../data/gtfs_standards_parsed.rds")
