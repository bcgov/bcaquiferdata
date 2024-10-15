# Code Design

This file contains notes about code design and conventions with the aim of
making collaboration and future modifications easier.

## Naming
- Snake case is used wherever possible
- Test files are named `test-XX_DESCRIPTION.R`, where `XX` is the order they
should be run (try to test lower order functions first).

## Documentation
- Descriptions for parameter arguments should follow:
  - `@param arg_name Type. Description`
  - e.g., `@param lidar_dir Character. File path of where Lidar tiles should be stored.`
  - e.g., `@param region sf simple features object. Shape file of the region of interest.`

- Document repetitive arguments (ones found in more than one function) in the
`R/aa_common_docs.R` file (named to sort to the top of the folder), and use
`@inheritParams common_docs` in the function documentation. This way
duplicate documentation stays consistent.

- Use `@noRd` to document internal functions (documentation for developers that
  isn't compiled into the docs)

## Shiny helpers
- `aq_tt()` is a helper function to create tooltips

## Data sources
- TRIM DEM <https://catalogue.data.gov.bc.ca/dataset/7b4fef7e-7cae-4379-97b8-62b03e9ac83d>
  via `bcmaps::cded`
- Lidar DEM <https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03>
