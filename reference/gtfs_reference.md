# GTFS reference

The data from the official GTFS specification document parsed to a list.
Revision date: `2026-04-27`.

## Usage

``` r
gtfs_reference
```

## Format

A list with data for every GTFS file. Each named list element (also a
list) has specifications for one GTFS file in the following structure:

- `File_Name`: file name including file extension (txt or geojson)

- `File_Presence`: Presence condition applied to the file

- `file`: file name without file extension

- `file_ext`: file extension

- `fields`: data.frame with parsed field specification (columns:
  `Field_Name`, `Type`, `Presence`, `Description`, `gtfsio_type`)

- `primary_key`: primary key as vector

- `field_types`: named vector on how GTFS types (values) should be read
  in gtfsio (names). Values are the same as in `fields`.

## Source

<https://github.com/google/transit/blob/master/gtfs/spec/en/reference.md>

## Details

GTFS Types are converted to R types in gtfsio according to the following
list:

- Array = `geojson_array`

- Color = `character`

- Currency amount = `numeric`

- Currency code = `character`

- Date = `integer`

- Email = `character`

- Enum = `character, integer`

- Float = `numeric`

- ID = `character`

- Integer = `integer`

- Language code = `character`

- Latitude = `numeric`

- Local time = `character`

- Longitude = `numeric`

- Non-negative float = `numeric`

- Non-negative integer = `integer`

- Non-null integer = `integer`

- Non-zero integer = `integer`

- Object = `geojson_object`

- Phone number = `character`

- Positive float = `numeric`

- Positive integer = `integer`

- String = `character`

- Text = `character`

- Text or URL or Email or Phone number = `character`

- Time = `character`

- Timezone = `character`

- URL = `character`

- Unique ID = `character`
