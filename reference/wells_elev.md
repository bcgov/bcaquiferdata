# Subset wells and add elevation

This function takes a region shape file and the DEM of a region (output
of
[`dem_region()`](http://bcgov.github.io/bcaquiferdata/reference/dem_region.md)),
subsets the wells data (from GWELLS) to this region and adds the
elevation data.

## Usage

``` r
wells_elev(wells_sub, dem, update = FALSE)
```

## Arguments

- wells_sub:

  sf spatial data frame. Subset of wells data output by
  [`wells_subset()`](http://bcgov.github.io/bcaquiferdata/reference/wells_subset.md)

- dem:

  stars simple features object. Output of
  [`dem_region()`](http://bcgov.github.io/bcaquiferdata/reference/dem_region.md).

- update:

  Logical. Force update of the data?

## Value

sf spatial data frame

## Examples

``` r
if (FALSE) { # interactive()

library(sf)
library(ggplot2)

# Load a shape file defining the region of interest
creek_sf <- st_read("misc/data/Clinton_Creek.shp")

# Get wells within this region
creek_wells <- wells_subset(creek_sf)

# Fetch Lidar DEM
creek_lidar <- dem_region(creek_sf)

# Collect wells in this region with added elevation from Lidar
creek_wells <- wells_elev(creek_wells, creek_lidar)

ggplot() +
  geom_sf(data = creek_sf) +
  geom_sf(data = creek_wells, aes(colour = elev), size = 0.5,
          fill = "NA", show.legend = FALSE) +
 coord_sf(datum = st_crs(3005)) # BC Albers

# OR Fetch TRIM DEM
creek_trim <- dem_region(creek_sf, type = "trim")

# Collect wells in this region with added elevation from Lidar
creek_wells <- wells_elev(creek_wells, creek_trim)

ggplot() +
  geom_sf(data = creek_sf) +
  geom_sf(data = creek_wells, aes(colour = elev), size = 0.5,
          fill = "NA", show.legend = FALSE) +
 coord_sf(datum = st_crs(3005)) # BC Albers
}
```
