# tc.sensors (development version)

# tc.sensors 0.2.0.90001

* Patch `pull_extension()` internal function. Remove port 8080 from URL. See [issue #13](https://github.com/Metropolitan-Council/tc.sensors/issues/13) and [PR #14](https://github.com/Metropolitan-Council/tc.sensors/pull/14).

# tc.sensors 0.2.0

* **NEW** functions!
  * `add_distance()` calculates the distance between sensors based on a given corridor and direction. Interpolates missing values by default.
  * `generate_spatial_lines()` generates an `sf` object with a `LINESTRING` for each given corridor, corridor direction, and corridor category. Limitations are noted in documentation.
* Function update `aggregate_sensor_data()` is now `aggregate_sensor()`
  * `_data()` is superfluous, so was removed. 
  * New name reinforces the function's purpose, which is to aggregate data from a single sensor.
  * New logical paramenters
    - `replace_impossible`, replace impossible volume and occupancy values in the 30-second (raw) data with `NA`
    - `interpolate_missing`, interpolate missing volume and occupancy using a rolling mean of the two neighboring observations (before and after).
    - `occupancy_threshold`, set a threshold for occupancy when calculating speed. Very low occupancy percentages were causing 
* Made significant progress on "Calculate speed and delay" vignette
  * Chose a subset of sensors to consistently work with
  * Used `{mice}` to impute null speed values with a random forest model based on total volume, total occupancy, hour, and day type. 
  * Added bibliography with regional and international relevant literature
  * New plots
    - Plot proportion of all speed values that are `NA` by hour for 10-minute and 1-hour aggregations.
    - Plot relationship between speed, occupancy % and relative volume for 10-minute and 1-hour aggregations.
    - Plot observed and imputed speed density at 1-hour aggregations by day type.
    - Plot imputed speed values at 1-hour aggregation by hour and day type.
    - Plot speed and delay using the median speed from hours 1-5 and 20-24 (chosen arbitrarily) by hour. 
* General updates
  * Shift from using `{dplyr}` to `{data.table}`
  * Added a `NEWS.md` file to track changes to the package.
