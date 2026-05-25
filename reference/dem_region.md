# Fetch and trim DEM of a region

This function takes a shape file of a region and creates a DEM of the
region. Lidar data is stored locally as tiles. Tiles are only downloaded
if they don't already exist unless `only_new = FALSE`. TRIM data is
obtained via the `bcmaps` package and stored locally as tiles. **Note:**
TRIM elevation is coarser than Lidar. Use Lidar unless it is missing for
your region of interest.

## Usage

``` r
dem_region(
  region,
  source = "lidar",
  out_file = NULL,
  overwrite = FALSE,
  buffer = 1,
  lidar_dir = NULL,
  only_new = TRUE,
  progress = httr::progress()
)
```

## Arguments

- region:

  sf simple features object. Shape file of the region of interest.

- source:

  Character. Source of DEM, "lidar", "trim" or a file path (or vector of
  file paths) to a custom DEM file. See Details.

- out_file:

  Character. File path of where to save tif of DEM clipped to `region`.

- overwrite:

  Logical. If `out_file` supplied, whether to overwrite this file if it
  already exists.

- buffer:

  Numeric. Percent buffer to apply to the `region` spatial file before
  cropping the DEM data to match. Increase this value if you find that
  wells on the edge of your area aren't been matched to elevations when
  using
  [`wells_elev()`](https://bcgov.github.io/bcaquiferdata/reference/wells_elev.md).

- lidar_dir:

  Character. File path of where Lidar tiles should be stored. Defaults
  to the cache directory. Only applies when `source = "lidar"`.

- only_new:

  Logical. Whether to download all Lidar tiles, or only new tiles that
  don't exist locally. Defaults to TRUE. Only apples when
  `source = "lidar"`.

- progress:

  Function. Progress bar to use. Generally leave as is.

## Value

stars spatiotemporal array object

## Details

Lidar tiles are the newest tile available. If you have reason to need a
historical file, contact the `bcaquiferdata` team to discuss your use
case.

## Data Source

Lidar data is obtained from the LidarBC portal. The `tiles` data frame
contains is an internally created data frame listing tiles and their
respective download locations. Tiles to download are selected based on
overlap between map tiles and the provided shapefile (`region`). These
Lidar tiles can be browsed and downloaded manually via the [LidarBC Open
LiDAR Data
Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)

The grid of map tiles is obtained from the BC Data Catalogue, [BCGS
1:20,000
Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)

TRIM data is obtained via the `bcmaps` package from the BC government
[Data
Catalogue](https://catalogue.data.gov.bc.ca/dataset/7b4fef7e-7cae-4379-97b8-62b03e9ac83d)
based on overlap between map tiles and the provided shapefile
(`region`).

If a file path or vector of file paths are provided, a local DEM is
loaded with `stars` and combined with
[`stars::st_mosaic()`](https://r-spatial.github.io/stars/reference/st_mosaic.html).
Note that it is assumed the data is elevation in metres.

## Saving to disk or memory

The fastest way to process these data is to use an stars proxy object
which keeps manipulations fast. However, to save a copy of the DEM, it
is *much* faster to use
[`sf::gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html)
(rather than
[`stars::write_stars()`](https://r-spatial.github.io/stars/reference/write_stars.html)
after).

Therefore, if `out_file` is NULL
[`stars::st_mosaic()`](https://r-spatial.github.io/stars/reference/st_mosaic.html)
and
[`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html)
will be used to manipulate a stars proxy object. Alternatively, if
`out_file` is provided,
[`sf::gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html)
will be used to combine, crop, and save a copy of the Lidar or Trim DEM
for the region provided. These means that slightly different methods are
used and it results in a very slightly different cropped region.
However, since the region file is buffered before cropping in either
method, it shouldn't affect well elevation calculations downstream.

## Examples

``` r
if (FALSE) { # interactive()

library(sf)
library(stars)

# Load a shape file defining the region of interest
creek_sf <- st_read("misc/data/Clinton_Creek.shp")

# Fetch Lidar DEM
creek_lidar <- dem_region(creek_sf)
plot(creek_lidar)

# Fetch TRIM DEM
creek_trim <- dem_region(creek_sf, source = "trim")
plot(creek_trim)

# Get dem and save to file
creek_lidar2 <- dem_region(creek_sf, out_file = "clinton_lidar.tif")
plot(creek_lidar)

creek_trim <- dem_region(creek_sf, source = "trim", out_file = "clinton_trim.tif")
plot(creek_trim)

# Use local DEM
koksilah_sf <- st_read("misc/data/Koksilah_watershed4/Koksilah_watershed4.shp")
koksilah_dem <- dem_region(
  koksilah_sf, source = "misc/data/Koksilah_Watershed_DEM_2km_Buffer.tif")
}
```
