# Subset wells and add elevation

This function takes a region shape file and the DEM of a region (output
of
[`dem_region()`](https://bcgov.github.io/bcaquiferdata/dev/reference/dem_region.md)),
subsets the wells data (from GWELLS) to this region and adds the
elevation data.

## Usage

``` r
wells_elev(wells_sub, dem, dem_extra = NULL, update = FALSE)
```

## Arguments

- wells_sub:

  sf spatial data frame. Subset of wells data output by
  [`wells_subset()`](https://bcgov.github.io/bcaquiferdata/dev/reference/wells_subset.md)

- dem:

  stars simple features object. Output of
  [`dem_region()`](https://bcgov.github.io/bcaquiferdata/dev/reference/dem_region.md).
  Primary source of elevation data.

- dem_extra:

  stars simple features object. Output of
  [`dem_region()`](https://bcgov.github.io/bcaquiferdata/dev/reference/dem_region.md).
  Optional secondary source of elevation data. Useful in situations
  where the primary source is incomplete. **Use with caution: Combining
  elevations measured though different techniques may introduce
  artifacts (See Details)**.

- update:

  Logical. Force update of the data?

## Value

sf spatial data frame

## Details

Because combining elevation data measured from different sources can
introduce artifacts, if two sources of elevation are provided (i.e. both
`dem` and `dem_extra`), the data will contain extra columns for
assessment. Specifically, there will be three elevation columns, rather
than just one. `elev1` contains elevations from the primary source
(`dem`), `elev2` contains elevations from the secondary source
(`dem_extra`), `elev` contains the combined elevation data, `elev1`
unless missing, then `elev2`.

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

# Dealing with missing data
mill_sf <- st_read("misc/data/MillBayWatershed.shp")
mill_wells <- wells_subset(mill_sf)

mill_lidar <- dem_region(mill_sf)
mill_trim <- dem_region(mill_sf, type = "trim")

mill_wells <- wells_elev(mill_wells, dem = mill_lidar, dem_extra = mill_trim)

# See how the elevation data is combined, `dem` (elev1) is the primary source.
select(mill_wells, well_tag_number, elev1, elev2, elev)

# Use local DEM
koksilah_sf <- st_read("misc/data/Koksilah_watershed4/Koksilah_watershed4.shp")
koksilah_wells <- wells_subset(koksilah_sf)
koksilah_dem <- dem_region(
  koksilah_sf, source = "misc/data/Koksilah_Watershed_DEM_2km_Buffer.tif")
koksilah_wells <- wells_elev(koksilah_wells, dem = koksilah_dem)

# Plot
p <- koksilah_wells |>
  st_transform(crs = st_crs(koksilah_dem))
plot(koksilah_dem, reset = FALSE, key.pos = NULL)
plot(p["elev"], add = TRUE, pch = 20)
}
```
