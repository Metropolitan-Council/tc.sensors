#' Build Mayfly API request
#'
#' @description Create an httr2 request object for the Mayfly API endpoints.
#'   The Mayfly API uses query parameters (not path segments) for filtering.
#'   For example: `/mayfly/counts?district=metro&year=2025&date=20250601&detector=100`
#'
#' @param endpoint character, API endpoint path (e.g., "counts", "occupancy",
#'   "speed", "length", "districts", "years", "dates", "detectors")
#' @param district character, MnDOT district code. Default is "metro".
#'   Use [mayfly_get_districts()] to see available districts.
#' @param year character or numeric, year for data query. Optional.
#' @param date character, date in YYYYMMDD format. Optional.
#' @param detector character, detector ID. Optional.
#'
#' @return httr2_request object
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_url_path_append req_url_query
mayfly_request <- function(endpoint,
                           district = "metro",
                           year = NULL,
                           date = NULL,
                           detector = NULL) {
  base_url <- "https://data.dot.state.mn.us"

  # Build query parameters
  query_params <- list(district = district)

  if (!is.null(year)) {
    query_params$year <- as.character(year)
  }

  if (!is.null(date)) {
    query_params$date <- date
  }

  if (!is.null(detector)) {
    query_params$detector <- as.character(detector)
  }

  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("mayfly", endpoint) %>%
    httr2::req_url_query(!!!query_params)

  return(req)
}


#' Perform Mayfly API request and parse response
#'
#' @description Execute a Mayfly API request and parse the JSON response.
#'   The Mayfly API returns `text/plain` content type, so this function
#'   uses [httr2::resp_body_string()] and [jsonlite::fromJSON()] to parse.
#'
#' @param req httr2_request object from mayfly_request()
#' @param .quiet logical, whether to suppress error messages. Default TRUE
#'
#' @return Parsed JSON response as vector or list, or NULL on failure
#'
#' @keywords internal
#'
#' @importFrom httr2 req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert_danger
mayfly_perform <- function(req, .quiet = TRUE) {
  result <- NULL

  error_handler <- if (.quiet) {
    function(e) {
      return(NULL)
    }
  } else {
    function(e) {
      cli::cli_alert_danger("API request failed: {conditionMessage(e)}")
      return(NULL)
    }
  }

  tryCatch(
    {
      resp <- httr2::req_perform(req)
      body_str <- httr2::resp_body_string(resp)
      result <- jsonlite::fromJSON(body_str)
    },
    error = error_handler
  )

  return(result)
}


#' Parse date in flexible format
#'
#' @description Parse dates in either "YYYY-MM-DD" or "YYYYMMDD" format
#'   and return both formats.
#'
#' @param date_input character, date in either "YYYY-MM-DD" or "YYYYMMDD" format
#'
#' @return list with components:
#'   - date_standard: date in "YYYY-MM-DD" format
#'   - date_yyyymmdd: date in "YYYYMMDD" format
#'   - year: year as character
#'   - date_obj: Date object
#'
#' @keywords internal
#'
#' @importFrom cli cli_abort
parse_date_flexible <- function(date_input) {
  # Validate input
  if (is.null(date_input) || length(date_input) == 0) {
    cli::cli_abort("Date input cannot be NULL or empty")
  }

  # Convert to character if it's a Date object
  if (inherits(date_input, "Date")) {
    date_input <- as.character(date_input)
  }

  # Check for NA or empty after conversion
  if (anyNA(date_input) || identical(date_input, "")) {
    cli::cli_abort("Invalid date format. Expected 'YYYY-MM-DD' or 'YYYYMMDD', got empty or NA")
  }

  # Try to detect format based on length and presence of dashes
  if (grepl("-", date_input)) {
    # Format: YYYY-MM-DD
    date_obj <- tryCatch(
      as.Date(date_input, format = "%Y-%m-%d"),
      error = function(e) NULL
    )
    if (is.null(date_obj) || is.na(date_obj)) {
      cli::cli_abort("Invalid date format. Expected 'YYYY-MM-DD' or 'YYYYMMDD', got: {date_input}")
    }
  } else if (nchar(date_input) == 8) {
    # Format: YYYYMMDD
    date_obj <- tryCatch(
      as.Date(date_input, format = "%Y%m%d"),
      error = function(e) NULL
    )
    if (is.null(date_obj) || is.na(date_obj)) {
      cli::cli_abort("Invalid date format. Expected 'YYYY-MM-DD' or 'YYYYMMDD', got: {date_input}")
    }
  } else {
    cli::cli_abort("Invalid date format. Expected 'YYYY-MM-DD' or 'YYYYMMDD', got: {date_input}")
  }

  # Return standardized formats
  list(
    date_standard = format(date_obj, "%Y-%m-%d"),
    date_yyyymmdd = format(date_obj, "%Y%m%d"),
    year = format(date_obj, "%Y"),
    date_obj = date_obj
  )
}


# Package environment for caching
.mayfly_cache <- new.env(parent = emptyenv())


#' Validate that requested date is available in Mayfly
#'
#' @description Check if a requested date has data available in the Mayfly API
#'   before attempting to fetch data. Caches available dates per year to minimize
#'   API calls.
#'
#' @param pull_date character, date in YYYY-MM-DD format
#' @param district character, district code. Default is "metro"
#' @param .quiet logical, whether to suppress messages. Default TRUE
#'
#' @return TRUE if date is available, throws error otherwise
#'
#' @keywords internal
#'
#' @importFrom cli cli_abort cli_alert_info
validate_date_available <- function(pull_date, district = "metro", .quiet = TRUE) {
  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)
  pull_date_std <- date_parsed$date_standard
  pull_year <- date_parsed$year
  date_yyyymmdd <- date_parsed$date_yyyymmdd

  # Check cache first
  cache_key <- paste0(district, "_", pull_year)

  if (!exists(cache_key, envir = .mayfly_cache)) {
    # Fetch available dates for this year
    if (!.quiet) {
      cli::cli_alert_info("Fetching available dates for {pull_year}...")
    }

    req <- mayfly_request(
      endpoint = "dates",
      district = district,
      year = pull_year
    )

    available_dates <- mayfly_perform(req, .quiet = .quiet)

    if (is.null(available_dates) || length(available_dates) == 0) {
      cli::cli_abort(
        "Unable to fetch available dates for {district} district in {pull_year}. API may be unavailable."
      )
    }

    # Cache the results
    assign(cache_key, available_dates, envir = .mayfly_cache)
  } else {
    available_dates <- get(cache_key, envir = .mayfly_cache)
  }

  # Check if requested date is available
  if (!(date_yyyymmdd %in% available_dates)) {
    available_range <- range(available_dates)
    cli::cli_abort(c(
      "Date {pull_date} ({date_yyyymmdd}) is not available in the Mayfly API.",
      "i" = "Available date range for {pull_year}: {available_range[1]} to {available_range[2]}",
      "i" = "Total dates available: {length(available_dates)}"
    ))
  }

  return(TRUE)
}


#' Get list of available districts
#'
#' @return character vector of district codes
#'
#' @keywords internal
#'
#' @export
mayfly_get_districts <- function() {
  req <- mayfly_request(endpoint = "districts")
  districts <- mayfly_perform(req, .quiet = TRUE)
  return(districts)
}


#' Get list of available years for a district
#'
#' @param district character, district code. Default is "metro"
#'
#' @return character vector of available years
#'
#' @keywords internal
#'
#' @export
mayfly_get_years <- function(district = "metro") {
  req <- mayfly_request(endpoint = "years", district = district)
  years <- mayfly_perform(req, .quiet = TRUE)
  return(years)
}


#' Get list of available dates for a district and year
#'
#' @param district character, district code. Default is "metro"
#' @param year character or numeric, year to query
#'
#' @return character vector of available dates in YYYYMMDD format
#'
#' @keywords internal
#'
#' @export
mayfly_get_dates <- function(district = "metro", year) {
  req <- mayfly_request(
    endpoint = "dates",
    district = district,
    year = year
  )
  dates <- mayfly_perform(req, .quiet = TRUE)
  return(dates)
}


#' Get list of available corridors for a district
#'
#' @param district character, district code. Default is "metro"
#' @param year character or numeric, year to query. Optional.
#' @param date character, date in YYYYMMDD format. Optional.
#'
#' @return character vector of corridor names
#'
#' @keywords internal
#'
#' @export
mayfly_get_corridors <- function(district = "metro", year = NULL, date = NULL) {
  req <- mayfly_request(
    endpoint = "corridors",
    district = district,
    year = year,
    date = date
  )
  corridors <- mayfly_perform(req, .quiet = TRUE)
  return(corridors)
}
