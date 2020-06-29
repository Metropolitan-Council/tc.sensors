
# tc.sensors

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Website MnDOT
JSON](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](http://data.dot.state.mn.us:8080/trafdat/metro/2018/20181021/5474.c30.json)
[![R build
status](https://github.com/Metropolitan-Council/tc.sensors/workflows/R-CMD-check/badge.svg)](https://github.com/Metropolitan-Council/tc.sensors/actions)
<!-- badges: end -->

## Overview

A package for pulling data for Minnesota Department of Transportation
(MnDOT) loop detectors installed on the Minnesota Freeway system in
30-second interval measurements of occupancy and volume, data which are
pushed daily to a public JSON feed.

## Installation

``` r
remotes::install_github("Metropolitan-Council/tc.sensors")
```

## Documentation

To access documentation and for help on how to use the package, run
`?<FUNCTION-NAME>` (e.g. `?pull_sensor`, `?pull_configuration`,
`?pull_sensor_ids`). Access vignettes in the vignettes file to see
examples of end-to-end workflows for pulling and storing data locally en
masse. Check back for a vignettes for calculating speeds, reference
speeds, delay, and VMT from the resulting files.

## Contributors

  - **Maintainer** Liz Roten (<liz.roten@metc.state.mn.us>)  
  - Nicole Sullivan
    [github.com/sullivannicole](https://github.com/sullivannicole)

## Code of Conduct

Please note that the `{tc.sensors}` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
