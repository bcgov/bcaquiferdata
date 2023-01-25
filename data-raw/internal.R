
lidar_url <- "https://nrs.objectstore.gov.bc.ca/gdwuts"

#bcdata::bcdc_get_record("bcgs-1-20-000-grid")
#bcdata::bcdc_tidy_resources('a61976ac-d8e8-4862-851e-d105227b6525')

tiles <- bcdata::bcdc_query_geodata('a61976ac-d8e8-4862-851e-d105227b6525') %>%
  dplyr::collect() %>%
  janitor::clean_names() %>%
  dplyr::select(map_tile) %>%
  dplyr::mutate(map_tile = tolower(map_tile))

tiles_utm <- tiles %>%
  sf::st_set_agr("constant") %>%# Suppress warnings about constant geometries
  sf::st_centroid() %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(coords = purrr::map(geometry, ~as.data.frame(sf::st_coordinates(.)))) %>%
  tidyr::unnest(coords) %>%
  dplyr::mutate(utm = (floor((.data$X + 180)/6) %% 60) + 1) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(map_tile, utm)


tiles <- dplyr::left_join(tiles, tiles_utm, by = "map_tile")

usethis::use_data(lidar_url, tiles, overwrite = TRUE)
