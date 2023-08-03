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

ui_wells <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Prepare Data",
    navset_card_pill(
      sidebar = sidebar(width = "20%",
        h4("Prepare data"),
        p("Filter GWELLs to watershed area and use Lidar or TRIM digital ",
        "elevation models to calculate well elevation"),

        uiOutput(ns("data_warning")),
        fileInput(
          ns("spatial_file"),
          label = paste0("Choose shape file(s) defining a watershed area ",
                         "(select multiple files using Ctrl, or use zip)"),
          buttonLabel = "Upload Spatial Data", multiple = TRUE),
        radioButtons(ns("dem_type"), strong("DEM source"), inline = TRUE,
                     choices = c("Lidar" = "lidar", "TRIM" = "trim")),
        h4("Messages"),
        verbatimTextOutput(ns("messages"), placeholder = TRUE),
        shinyWidgets::progressBar(
          title = "Current Lidar tile:",
          id = ns("lidar_progress"), value = 0, display_pct = TRUE)
      ),
      nav_panel("Maps",
                plotOutput(ns("map_plot"), height = "650px")),
      nav_panel("Wells Data",
                DT::dataTableOutput(ns("wells_table"))
      )
    )
  )
}

server_wells <- function(id, have_data) {

  moduleServer(id, function(input, output, session) {

    output$data_warning <- renderUI({
      if(have_data()) {
        w <- tagList(icon("check", style = "color:lightgreen;"),
                     "Data Available", p())
      } else {
        w <- tagList(icon("x", style = "color:red;"),
                     "Data Not Available (see Download Data tab)",
                     p())
      }
      w
    })

    # watershed -----------------------------
    watershed <- reactive({
      req(input$spatial_file)

      id <- showNotification("Loading spatial file...",
                             duration = NULL, closeButton = FALSE)
      type <- ext(input$spatial_file$datapath)

      if(all(c("shp", "shx", "dbf", "prj") %in% type)) {
        file.rename(input$spatial_file$datapath,
                    file.path(tempdir(), input$spatial_file$name))
        f <- file.path(tempdir(), input$spatial_file$name)[type == "shp"]
      } else if(nrow(input$spatial_file) == 1 && type == "zip"){
        f <- utils::unzip(input$spatial_file$datapath, list = TRUE)
        utils::unzip(input$spatial_file$datapath, exdir = tempdir())
        f <- stringr::str_subset(f$Name, "shp$") %>%
          file.path(tempdir(), .)
      } else {
        validate(need(
        FALSE,
        paste0("Cannot detect file type. Must be a shapefile including a ",
               "shp, shx, prj, and dbf file (can be zipped or multiple selected)")))
      }

      removeNotification(id)
      sf::st_read(f)
    })

    # map -----------------------------
    output$map_plot <- renderPlot({
      req(watershed(), wells(), dem())

      id <- showNotification("Plotting data...",
                             duration = NULL, closeButton = FALSE)

      # Down sample
      ds <- nrow(dem()) / 150
      temp <- stars::st_downsample(dem(), n = ds) %>%
        sf::st_as_sf(as_points = FALSE, merge = TRUE)

      g <- ggplot2::ggplot() +
        ggthemes::theme_map() +
        ggplot2::theme(legend.position = "right") +
        ggplot2::geom_sf(data = temp, ggplot2::aes(fill = .data$elev),
                         colour = NA) +
        ggplot2::geom_sf(data = watershed(), linewidth = 2, fill = NA) +
        ggplot2::geom_sf(data = wells(), size = 1,
                         ggplot2::aes(colour = is.na(.data$elev))) +
        ggplot2::scale_fill_viridis_c(name = "Elevation (m)") +
        ggplot2::labs(caption = "Note: DEM downsampled for plotting") +
        ggplot2::scale_colour_manual(
          name = "",
          labels = c("TRUE" = "Elevation Missing", "FALSE" = "Elevation Present"),
          values = c("TRUE" = "#CD4071FF", "FALSE" = "#180F3EFF"))

      removeNotification(id)

      g
    }, res = 100) %>%
      bindCache(input$spatial_file, input$dem_type)

    dem <- reactive({
      req(watershed(), input$dem_type)
      dem_region_shiny(input$dem_type, watershed(), session)
    })


    # wells ----------------------------------
    wells <- reactive({
      req(watershed(), dem())

      input$dem_type

      id <- showNotification("Filtering well data...",
                             type = "message",
                             duration = NULL, closeButton = FALSE)

      withCallingHandlers({
        message("Wells - Start")
        w <- wells_subset(watershed()) %>%
          wells_elev(dem())
        message("Wells - Done")
      },
      message = function(m) {
        shinyjs::html(id = NS(id, "messages"), html = m$message, add = TRUE)
      })

      removeNotification(id)
      w
    }) %>%
      bindCache(input$spatial_file, input$dem_type)

    # wells table -------------------------------
    output$wells_table <- DT::renderDataTable({
      wells() %>%
        sf::st_drop_geometry() %>%
        aq_dt()
    })

    # Outputs
    wells
  })
}

dem_region_shiny <- function(type, watershed, session) {

  id <- showNotification(paste0("Fetching ", type, " data..."),
                         duration = NULL, closeButton = FALSE)

  withCallingHandlers({
    message(stringr::str_to_title(type), " - Start")

    p <- shinyhttr::progress(session, id = "dem_progress")

    # Catch errors if have issues and try again
    l <- try(dem_region(watershed, progress = p, type = type), silent = TRUE)
    #l <- try(stop("testing"), silent = TRUE)
    if(inherits(l, "try-error")) {
      message("  Problem with ", type, " tiles, trying again...")
      l <- tryCatch(
        dem_region(watershed, progress = p, type = type),
        #stop("testing2"),
        error = function(cond) {
          message("  Problem fetching ", type, " tiles\n",
                  "  Error message: ", cond$message)
        })
    }
    message(stringr::str_to_title(type), " - Done")
  },
  message = function(m) {
    shinyjs::html(id = "messages", html = m$message, add = TRUE)
  })

  removeNotification(id)
  l
}
