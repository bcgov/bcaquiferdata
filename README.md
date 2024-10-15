
<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R-CMD-check](https://github.com/bcgov/bcaquiferdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/bcaquiferdata/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# bcaquiferdata

This is an R package for processing BC Gov GWELLS data into formats more
suitable for other analyses (e.g., Strater, Voxler, ArcHydro).

The functions provided here allow users to filter GWELLS data by region
(shapefile), calculate and add elevation data from Lidar or TRIM, and
clean and categorize GWELLS lithology and yield data.

The Shiny app (a graphical user interface), provides a tool for data
exploration and exporting.

We’re still actively refining and adding to this tool, but it is
maturing.

## Quick Start

- Install/Update [**R**](https://cloud.r-project.org/) and
  [**RStudio**](https://rstudio.com/)
- Install pak - `install.packages("pak", dependencies = TRUE)`
- Install bcaquiferdata - `pak::pkg_install("bcgov/bcaquiferdata")`
  \[Update All if asked\]
- Launch the Shiny App - `aq_app()`

## In Detail

1.  [**Install/Update RStudio**](https://rstudio.com/) (if it’s been a
    while)

2.  [**Install/Update R**](https://cloud.r-project.org/) (if it’s been a
    while)

3.  **Open RStudio**

4.  **Install pak** In the console type the following and hit enter

    ``` r
    install.packages("pak", dependencies = TRUE)
    ```

5.  **Check that your system is ready** Install build tools if prompted

    ``` r
    pkgbuild::check_build_tools()
    ```

6.  **Install bcgwcat** In the console type the following and hit enter
    (be sure to install all updates if prompted!)

    ``` r
    pak::pkg_install("bcgov/bcgwcat")
    ```

## Development version

If you’re interested in testing out the developmental version of the
package, you can install it with
`pak::pkg_install("bcgov/bcgwcat@dev")`.

## Troubleshooting

If you run into errors right at the start, try **updating all packages
required by bcgwcat**

``` r
pak::pkg_install("bcgov/bcgwcat", upgrade = TRUE)
```

## Shiny User-Interface

To lauch the Shiny User-Interface, type this line into the R console at
the prompt (in the screen with `>`) and hit ‘Enter’.

``` r
bcaquiferdata::aq_app() # Launch the app
```

**See the
[tutorial](https://bcgov.github.io/bcaquiferdata/articles/bcaquiferdata.html)
for a more in-depth look at how to use the User Interface**

## Vignette/Tutorials

See more details on the [bcaquiferdata
website](https://bcgov.github.io/bcaquiferdata)

## Citing `bcaquiferdata`

You can cite this software as follows:

LaZerte S, Bieber C, Province of British Columbia (2024). *BC Aquifer
Data Tools*. R package version 0.0.3.9000,
<http://bcgov.github.io/bcaquiferdata/>.

## License

Copyright 2024 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the “License”); you may
not use this file except in compliance with the License. You may obtain
a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
