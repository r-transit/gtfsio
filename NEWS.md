# gtfsio 0.0.0.xxxx

## New features

### Functions

- `get_gtfs_standards()` - returns the standards for reading and writing GTFS feeds with R as a named list. Each element (also a list) represents a distinct GTFS text file, and describes: whether this file is required, optional or conditionally required; the fields that compose each file, including which R data type is best suited to represent it, whether the field is required, optional or conditionally required, and which values it can assume (most relevant to GTFS `ENUM`s.
- `import_gtfs()` - imports GTFS transit feeds from either a local `.zip` file or an URL. Columns are parsed according to the standards specified in `get_gtfs_standards()`.

## Notes
