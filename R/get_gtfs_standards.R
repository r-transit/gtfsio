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
  agency <- list(
    file_spec       = "req",
    agency_id       = list("id",            "cond"),
    agency_name     = list("text",          "req"),
    agency_url      = list("url",           "req"),
    agency_timezone = list("timezone",      "req"),
    agency_lang     = list("language_code", "opt"),
    agency_phone    = list("phone_number",  "opt"),
    agency_fare_url = list("url",           "opt"),
    agency_email    = list("email",         "opt")
  )

  stops <- list(
    file_spec           = "req",
    stop_id             = list("id",        "req"),
    stop_code           = list("text",      "opt"),
    stop_name           = list("text",      "cond"),
    tts_stop_name       = list("text",      "opt"),
    stop_desc           = list("text",      "opt"),
    stop_lat            = list("latitude",  "cond"),
    stop_lon            = list("longitude", "cond"),
    zone_id             = list("id",        "cond"),
    stop_url            = list("url",       "opt"),
    location_type       = list("enum",      "opt", c(0, 1, 2, 3, 4)),
    parent_station      = list("id",        "cond"),
    stop_timezone       = list("timezone",  "opt"),
    wheelchair_boarding = list("enum",      "opt", c(0, 1, 2)),
    level_id            = list("id",        "opt"),
    platform_code       = list("text",      "opt")
  )

  routes <- list(
    file_spec          = "req",
    route_id           = list("id",       "req"),
    agency_id          = list("id",       "cond"),
    route_short_name   = list("text",     "cond"),
    route_long_name    = list("text",     "cond"),
    route_desc         = list("text",     "opt"),
    route_type         = list("enum",     "req", c(0, 1, 2, 3, 4, 5, 6, 7, 11,
                                                   12)),
    route_url          = list("url",      "opt"),
    route_color        = list("color",    "opt"),
    route_text_color   = list("color",    "opt"),
    route_sort_order   = list("integer",  "opt"),
    continuous_pickup  = list("enum",     "opt", c(0, 1, 2, 3)),
    continuous_dropoff = list("enum",     "opt", c(0, 1, 2, 3)),
    network_id         = list("id",       "opt")
  )

  trips <- list(
    file_spec             = "req",
    route_id              = list("id",  "req"),
    service_id            = list("id",  "req"),
    trip_id               = list("id",  "req"),
    trip_headsign         = list("text", "opt"),
    trip_short_name       = list("text", "opt"),
    direction_id          = list("enum", "opt", c(0, 1)),
    block_id              = list("id",   "opt"),
    shape_id              = list("id",   "cond"),
    wheelchair_accessible = list("enum", "opt", c(0, 1, 2)),
    bikes_allowed         = list("enum", "opt", c(0, 1, 2))
  )

  stop_times <- list(
    file_spec           = "req",
    trip_id             = list("id",      "req"),
    arrival_time        = list("time",    "cond"),
    departure_time      = list("time",    "cond"),
    stop_id             = list("id",      "req"),
    stop_sequence       = list("integer", "req"),
    stop_headsign       = list("text",    "opt"),
    pickup_type         = list("enum",    "opt", c(0, 1, 2, 3)),
    drop_off_type       = list("enum",    "opt", c(0, 1, 2, 3)),
    continuous_pickup   = list("enum",    "opt", c(0, 1, 2, 3)),
    continuous_drop_off = list("enum",    "opt", c(0, 1, 2, 3)),
    shape_dist_traveled = list("float",   "opt"),
    timepoint           = list("enum",    "opt", c(0, 1))
  )

  calendar <- list(
    file_spec   = "cond",
    service_id  = list("id",   "req"),
    monday      = list("enum", "req", c(0, 1)),
    tuesday     = list("enum", "req", c(0, 1)),
    wednesday   = list("enum", "req", c(0, 1)),
    thursday    = list("enum", "req", c(0, 1)),
    friday      = list("enum", "req", c(0, 1)),
    saturday    = list("enum", "req", c(0, 1)),
    sunday      = list("enum", "req", c(0, 1)),
    start_date  = list("date", "req", c(0, 1)),
    end_date    = list("date", "req", c(0, 1))
  )

  calendar_dates <- list(
    file_spec      = "cond",
    service_id     = list("id",   "req"),
    date           = list("date", "req"),
    exception_type = list("enum", "req", c(1, 2))
  )

  fare_attributes <- list(
    file_spec         = "opt",
    fare_id           = list("id",            "req"),
    price             = list("float",         "req"),
    currency_type     = list("currency_code", "req"),
    payment_method    = list("enum",          "req", c(0, 1)),
    transfers         = list("enum",          "req", c(0, 1, 2)),
    agency_id         = list("id",            "cond"),
    transfer_duration = list("integer",       "opt")
  )

  fare_rules <- list(
    file_spec      = "opt",
    fare_id        = list("id", "req"),
    route_id       = list("id", "opt"),
    origin_id      = list("id", "opt"),
    destination_id = list("id", "opt"),
    contains_id    = list("id", "opt")
  )

  fare_products <- list(
    file_spec         = "opt",
    fare_product_id   = list("id",              "req"),
    fare_product_name = list("text",            "opt"),
    amount            = list("currency_amount", "req"),
    currency          = list("currency_code",   "req")
  )

  fare_leg_rules <- list(
    file_spec       = "opt",
    leg_group_id    = list("id", "opt"),
    network_id      = list("id", "opt"),
    from_area_id    = list("id", "opt"),
    to_area_id      = list("id", "opt"),
    fare_product_id = list("id", "req")
  )

  fare_transfer_rules <- list(
    file_spec           = "opt",
    from_leg_group_id   = list("id",      "opt"),
    to_leg_group_id     = list("id",      "opt"),
    transfer_count      = list("integer", "cond"),
    duration_limit      = list("integer", "opt"),
    duration_limit_type = list("enum",    "cond", c(0, 1, 2, 3)),
    fare_transfer_type  = list("enum",    "req",  c(0, 1, 2)),
    fare_product_id     = list("id",      "opt")
  )

  areas <- list(
    file_spec = "opt",
    area_id   = list("id",   "req"),
    area_name = list("text", "opt")
  )

  stop_areas <- list(
    file_spec = "opt",
    area_id   = list("id", "req"),
    stop_id   = list("id", "opt")
  )

  shapes <- list(
    file_spec           = "opt",
    shape_id            = list("id",        "req"),
    shape_pt_lat        = list("latitude",  "req"),
    shape_pt_lon        = list("longitude", "req"),
    shape_pt_sequence   = list("integer",   "req"),
    shape_dist_traveled = list("float",     "opt")
  )

  frequencies <- list(
    file_spec    = "opt",
    trip_id      = list("id",      "req"),
    start_time   = list("time",    "req"),
    end_time     = list("time",    "req"),
    headway_secs = list("integer", "req"),
    exact_times  = list("enum",    "opt", c(0, 1))
  )

  transfers <- list(
    file_spec         = "opt",
    from_stop_id      = list("id",      "cond"),
    to_stop_id        = list("id",      "cond"),
    from_route_id     = list("id",      "opt"),
    to_route_id       = list("id",      "opt"),
    from_trip_id      = list("id",      "cond"),
    to_trip_id        = list("id",      "cond"),
    transfer_type     = list("enum",    "req", c(0, 1, 2, 3, 4, 5)),
    min_transfer_time = list("integer", "opt")
  )

  pathways <- list(
    file_spec              = "opt",
    pathway_id             = list("id",      "req"),
    from_stop_id           = list("id",      "req"),
    to_stop_id             = list("id",      "req"),
    pathway_mode           = list("enum",    "req", c(1, 2, 3, 4, 5, 6, 7)),
    is_bidirectional       = list("enum",    "req", c(0, 1)),
    length                 = list("float",   "opt"),
    traversal_time         = list("integer", "opt"),
    stair_count            = list("integer", "opt"),
    max_slope              = list("float",   "opt"),
    min_width              = list("float",   "opt"),
    signposted_as          = list("text",    "opt"),
    reversed_signposted_as = list("text",    "opt")
  )

  levels <- list(
    file_spec   = "cond",
    level_id    = list("id",    "req"),
    level_index = list("float", "req"),
    level_name  = list("text",  "opt")
  )

  translations <- list(
    file_spec     = "opt",
    table_name    = list("enum",          "req", c("agency", "stops", "routes",
                                                   "trips", "stop_times",
                                                   "feed_info", "pathways",
                                                   "levels", "attribution")),
    field_name    = list("text",          "req"),
    language      = list("language_code", "req"),
    translation   = list(c("text", "url", "email", "phone_number"), "req"),
    record_id     = list("id", "cond"),
    record_sub_id = list("id", "cond"),
    field_value   = list(c("text", "url", "email", "phone_number"), "cond")
  )

  feed_info <- list(
    file_spec           = "cond",
    feed_publisher_name = list("text",          "req"),
    feed_publisher_url  = list("url",           "req"),
    feed_lang           = list("language_code", "req"),
    default_lang        = list("language_code", "opt"),
    feed_start_date     = list("date",          "opt"),
    feed_end_date       = list("date",          "opt"),
    feed_version        = list("text",          "opt"),
    feed_contact_email  = list("email",         "opt"),
    feed_contact_url    = list("url",           "opt")
  )

  attributions <- list(
    file_spec         = "opt",
    attribution_id    = list("id",           "opt"),
    agency_id         = list("id",           "opt"),
    route_id          = list("id",           "opt"),
    trip_id           = list("id",           "opt"),
    organization_name = list("text",         "req"),
    is_producer       = list("enum",         "opt", c(0, 1)),
    is_operator       = list("enum",         "opt", c(0, 1)),
    is_authority      = list("enum",         "opt", c(0, 1)),
    attribution_url   = list("url",          "opt"),
    attribution_email = list("email",        "opt"),
    attribution_phone = list("phone_number", "opt")
  )

  # create gtfs_standards object

  gtfs_standards <- list(
    agency = agency,
    stops = stops,
    routes = routes,
    trips = trips,
    stop_times = stop_times,
    calendar = calendar,
    calendar_dates = calendar_dates,
    fare_attributes = fare_attributes,
    fare_rules = fare_rules,
    fare_products = fare_products,
    fare_leg_rules = fare_leg_rules,
    fare_transfer_rules = fare_transfer_rules,
    areas = areas,
    stop_areas = stop_areas,
    shapes = shapes,
    frequencies = frequencies,
    transfers = transfers,
    pathways = pathways,
    levels = levels,
    translations = translations,
    feed_info = feed_info,
    attributions = attributions
  )

  # define R types most similar to GTFS reference types

  r_equivalents <- c(
    color           = "character",
    currency_amount = "numeric",
    currency_code   = "character",
    date            = "integer",
    email           = "character",
    enum            = "integer",
    id              = "character",
    integer         = "integer",
    language_code   = "character",
    latitude        = "numeric",
    longitude       = "numeric",
    float           = "numeric",
    phone_number    = "character",
    text            = "character",
    time            = "character",
    timezone        = "character",
    url             = "character"
  )

  # translate GTFS reference types to R types

  gtfs_standards <- lapply(gtfs_standards, translate_types, r_equivalents)

  # correct a small special case:
  # 'translations' 'table_name' field is an ENUM, but its allowed values are
  # strings, not integers. this results in a warning when importing the GTFS
  # using data.table::fread(). so change R equivalent to character, instead of
  # integer

  gtfs_standards$translations$table_name[[1]] <- "character"

  return(gtfs_standards)
}

#' Translate GTFS specification types to R equivalent types
#'
#' @param text_file A named `list` containing a GTFS text file specification, as
#'   described in the body of [get_gtfs_standards].
#' @param r_equivalents A named `character vector`, in which each name is the
#'   GTFS specification type and the content its R equivalent.
#'
#' @return A named `list` holding a GTFS text file specification, but with
#'   R data types instead of GTFS spec data types.
#'
#' @keywords internal
translate_types <- function(text_file, r_equivalents) {

  # for each text_file element:
  # - check if it's a list.
  #  - if it's not, then it's the 'file_spec' element. return 'file_spec' value
  #  - if it is, replace first entry (GTFS spec type) for an equivalent R type

  text_file <- lapply(
    text_file,
    function(i) {
      if (!is.list(i)) return(i)

      new_spec <- i
      gtfs_type <- new_spec[[1]]
      r_type <- r_equivalents[gtfs_type]

      # some 'translations' fields ('translation' and 'field_value') might have
      # more than one GTFS data type.
      # their R equivalent is always 'character', no matter the GTFS type, so we
      # can safely get only the first value ('character')

      if (length(r_type) > 1) r_type <- r_type[1]

      new_spec[[1]] <- r_type

      return(new_spec)
    }
  )
}
