# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

library(ggspatial)
library(sf)
library(dplyr)
library(patchwork)

region <- st_read("misc/data/Clinton_Creek.shp")
pnts <- wells_subset(region)

# Load DEM raster as combined (mosaic)
lidar <- dem_region(region)
trim <- dem_region(region, type = "trim")

nrow(trim)/150
nrow(lidar)/150
t <- stars::st_downsample(trim, n = 10)
l <- stars::st_downsample(lidar, n = 150)

plot(t["elev"])
plot(l["elev"])

tsf <- st_as_sf(t, as_points = FALSE, merge = TRUE)
lsf <- st_as_sf(l, as_points = FALSE, merge = TRUE)

pnts2 <- region |>
  wells_subset() |>
  wells_elev(l)

g1 <- ggplot(data = tsf) +
  ggthemes::theme_map() +
  ggplot2::theme(legend.position = "right") +
  ggplot2::geom_sf(data = region, linewidth = 2, fill = "white") +
  geom_sf(aes(fill = elev), colour = NA) +
  geom_sf(data = pnts) +
  geom_sf(data = pnts2, colour = "red", alpha = 0.5) +
  ggplot2::scale_fill_viridis_c(name = "Elevation (m)")
g2 <- ggplot(data = lsf) +
  ggthemes::theme_map() +
  ggplot2::theme(legend.position = "right") +
  ggplot2::geom_sf(data = region, linewidth = 2, fill = "white") +
  geom_sf(aes(fill = elev), colour = NA) +
  geom_sf(data = pnts) +
  geom_sf(data = pnts2, colour = "red", alpha = 0.5) +
  ggplot2::scale_fill_viridis_c(name = "Elevation (m)")

g1 + g2

