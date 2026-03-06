# tiles

A spatial data frame of map tiles with corresponding links to Lidar
tiles.

## Usage

``` r
tiles
```

## Format

### `tiles`

A data frame with 7,129 rows and 5 columns:

- map_tile:

  Tile name

- geometry:

  Spatial data

- utm:

  Projection

- tile_name:

  Lidar tile name

- url:

  Link to Lidar tile

## Details

The spatial grid of map tiles is obtained from the BC Data Catalogue,
[BCGS 1:20,000
Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)

Links to Lidar tile urls are extracted from the list at the [LidarBC
Open LiDAR Data
Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)
