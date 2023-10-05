#' Flags
#'
#' A glossary of flag terms
#'
#' @format ## `flags`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{Flag}{flag name}
#'   \item{Description}{Flag description}
#' }
"flags"

#' tiles
#'
#' A spatial data frame of map tiles with corresponding links to Lidar tiles.
#'
#' The spatial grid of map tiles is obtained from the BC Data Catalogue,
#' [BCGS 1:20,000 Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)
#'
#' Links to Lidar tile urls are extracted from the list at the
#' [LidarBC Open LiDAR Data Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)
#'
#'
#' @format ## `tiles`
#' A data frame with 7,129 rows and 5 columns:
#' \describe{
#'   \item{map_tile}{Tile name}
#'   \item{geometry}{Spatial data}
#'   \item{utm}{Projection}
#'   \item{tile_name}{Lidar tile name}
#'   \item{url}{Link to Lidar tile}
#'
#' }
"tiles"
