# write R script that pulls the headway data for all sensors in config
library(tc.sensors)
library(dplyr)
library(purrr)
library(cli)

# Get configuration data
config_all <- pull_configuration()

# Filter for active mainline sensors
config <- config_all %>%
    dplyr::filter(
        detector_abandoned == "f",
        detector_category == ""
    )

# Use recent date
yesterday <- Sys.Date() - 2

cli_alert_info("Testing all {nrow(config)} sensors for headway data on {yesterday}")

# Check headway data for all sensors
sensors_with_data <- purrr::map_dfr(1:nrow(config), ~ {
    sensor_name <- config$detector_name[[.x]]

    if (.x %% 100 == 0) {
        cli_alert("Progress: {.x}/{nrow(config)} sensors checked")
    }

    # Try to pull headway data
    result <- tryCatch(
        {
            headway_data <- pull_sensor_headway(
                sensor = sensor_name,
                pull_date = yesterday,
                fill_gaps = TRUE
            )

            non_na_count <- sum(!is.na(headway_data$headway))

            if (non_na_count > 0) {
                return(data.frame(
                    sensor = sensor_name,
                    non_na_count = non_na_count,
                    stringsAsFactors = FALSE
                ))
            } else {
                return(NULL)
            }
        },
        error = function(e) {
            return(NULL)
        }
    )

    result
})

cli_alert_success("Found {nrow(sensors_with_data)} sensors with headway data")

print(sensors_with_data)
