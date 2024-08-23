# gtfs_standard = get_gtfs_standards()
# saveRDS(gtfs_standard, "gtfs_standards.rds")
gtfs_standards_old = readRDS("gtfs_standards_old.rds")

str(gtfs_standards_old, 1)
str(gtfs_standards_old$agency, 1)
