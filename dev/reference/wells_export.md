# Export wells data for use in Strater and Voxler

Export wells data for use in Strater and Voxler

## Usage

``` r
wells_export(wells_sub, id, type, dir = ".", zip = FALSE, preview = FALSE)
```

## Arguments

- wells_sub:

  Data frame. Output of
  [`wells_elev()`](https://bcgov.github.io/bcaquiferdata/dev/reference/wells_elev.md)

- id:

  Character. Id to prepend to all output files e.g., "id_lith.csv"

- type:

  Character. Format in which to export. One of "strater", "voxler",
  "archydro", "leapfrog", or "surfer" (case-insensitive).

- dir:

  Character. Directory where files should be exported to. Defaults to
  working directory.

- zip:

  Logical. Whether to export a zip archive of the files to `dir`.

- preview:

  Logical. Whether to preview the exports (`TRUE`, return a list of data
  frames) or to actually export the data (`FALSE`, write the necessary
  files to the `dir` folder, default).

## Value

If `preview = FALSE`, a vector of file names (or if `zip = TRUE`, a
single filename of the zipped archive); if `preview = TRUE`, a list of
data frames.

## Examples

``` r
if (FALSE) { # interactive()

library(sf)

# Load a shape file defining the region of interest
creek <- st_read("misc/data/Clinton_Creek.shp")

# Get wells within this region
creek_wells <- wells_subset(creek)

# Fetch Lidar DEM
creek_lidar <- dem_region(creek)

# Collect wells in this region with added elevation from Lidar
creek_wells <- wells_elev(creek_wells, creek_lidar)

# Preview data for Strater
p <- wells_export(creek_wells, id = "clinton", type = "strater", preview = TRUE)
names(p)
p[["strater_lith"]]
p[["strater_collars"]]
p[["strater_wells"]]

# Export data for Strater
wells_export(creek_wells, id = "clinton", type = "strater", zip = TRUE)

# Export data for Voxler
wells_export(creek_wells, id = "clinton", type = "voxler", zip = TRUE)

# Export Arc Hydro
wells_export(creek_wells, id = "clinton", type = "archydro", zip = TRUE)

# Export Surver
wells_export(creek_wells, id = "clinton", type = "surfer", zip = TRUE)

wells_export(creek_wells, id = "clinton", type = "leapfrog", zip = TRUE)
}
```
