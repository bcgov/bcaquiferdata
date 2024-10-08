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
        uiOutput(ns("elev_warning")),
        p(),
        fileInput(
          ns("spatial_file"),
          label = paste0("Choose shape file(s) defining a watershed area ",
                         "(select multiple files using Ctrl, or use zip)"),
          buttonLabel = "Upload Spatial Data", multiple = TRUE),
        radioButtons(
          ns("dem_combo"), strong("DEM source"), inline = TRUE,
          choiceNames = list("Lidar", span("TRIM", style = "margin-right:150px"),
                             "Lidar with TRIM", "TRIM with Lidar"),
          choiceValues = c("lidar", "trim", "lidar_trim", "trim_lidar")),
        radioButtons(
          ns("fix_bottom"), strong("Fix zero-width bottom lithology intervals?"), inline = TRUE,
          choices = list("Yes" = TRUE, "No" = FALSE)),
        h4("Messages"),
        verbatimTextOutput(ns("messages"), placeholder = TRUE)
      ),
      nav_panel("Maps",
                plotOutput(ns("map_plot"), height = "650px")),
      nav_panel("Wells Data",
                DT::dataTableOutput(ns("wells_table"))
      ),
      nav_panel("Info", includeMarkdown(
        system.file("extra_docs", "wells_desc.md",
                    package = "bcaquiferdata"))
      )
    )
  )
}

server_wells <- function(id, have_data) {

  moduleServer(id, function(input, output, session) {

    # warnings -------------------------------------
    output$data_warning <- renderUI({
      if(have_data()) {
        w <- tagList(icon("check", style = "color:lightgreen;"),
                     "Wells Data Available")
      } else {
        w <- tagList(icon("x", style = "color:red;"),
                     "Wells Data Not Available (see Download Data tab)")
      }
      w
    })

    output$elev_warning <- renderUI({
      req(input$dem_combo)
      if(stringr::str_detect(input$dem_combo, "_")) {
        w <- tagList(icon("triangle-exclamation", style = "color:orange;"),
                     "Multiple Elevation Sources (use caution)")
      } else {
        w <- tagList(icon("check", style = "color:lightgreen;"),
                     "Single Elevation Source")
      }
      w

    })

    # watershed -----------------------------
    watershed <- reactive({
      req(input$spatial_file)

      id <- showNotification("Loading spatial file...",
                             duration = NULL, closeButton = TRUE)
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
        {removeNotification(id)
          paste0("Cannot detect file type. Must be a shapefile including a ",
                 "shp, shx, prj, and dbf file (can be zipped or multiple selected)")
          }))
      }

      removeNotification(id)
      sf::st_read(f)
    })


    # elevation ----------------------------------
    dem_lidar <- reactive({
      req(watershed())
      dem_region_shiny("lidar", watershed(), session)
    }) |>
      bindCache(input$spatial_file)

    dem_trim <- reactive({
      req(watershed())
      dem_region_shiny("trim", watershed(), session)
    }) |>
      bindCache(input$spatial_file)

    dem_type1 <- reactive({
      req(input$dem_combo)
      stringr::str_extract(input$dem_combo, "^[^_]+")
    })

    dem_type2 <- reactive({
      req(input$dem_combo)
      if(stringr::str_detect(input$dem_combo, "_")) {
        stringr::str_extract(input$dem_combo, "[^_]+$")
      } else NULL
    })

    dem1 <- reactive({
      req(watershed(), dem_type1())
      if(dem_type1() == "lidar") dem_lidar() else dem_trim()
    }) |>
      bindCache(input$spatial_file, dem_type1())

    dem2 <- reactive({
      req(watershed())
      if(!is.null(dem_type2())) {
        if(dem_type2() == "lidar") dem_lidar() else dem_trim()
      } else NULL
    }) |>
      bindCache(input$spatial_file, dem_type2())

    # map -----------------------------

    # Down sample and convert to points
    dem_tiles1 <- reactive({
      ds <- nrow(dem1()) / 150
      stars::st_downsample(dem1(), n = ds) %>%
        sf::st_as_sf(as_points = FALSE)
    }) %>%
      bindCache(input$spatial_file, dem_type1())

    dem_tiles2 <- reactive({
      if(!is.null(dem_type2())) {
        ds <- nrow(dem2()) / 150
        stars::st_downsample(dem2(), n = ds) %>%
          sf::st_as_sf(as_points = FALSE)
      } else NULL
    }) %>%
      bindCache(input$spatial_file, dem_type2())


    output$map_plot <- renderPlot({
      req(watershed(), wells(), dem1())

      id <- showNotification("Plotting data...",
                             duration = NULL, closeButton = FALSE)

      g <- ggplot2::ggplot() +
        ggthemes::theme_map() +
        ggplot2::theme(legend.position = "right")

      title <- paste("Elevation Data:", dem_type1())
      if(!is.null(dem_tiles2())) {
        title <- paste(title, "supplemented with", dem_type2())
        g <- g +
          ggplot2::geom_sf(data = dem_tiles2(), ggplot2::aes(fill = .data$elev),
                           colour = NA)
      }

      g <- g +
        ggplot2::geom_sf(data = dem_tiles1(), ggplot2::aes(fill = .data$elev),
                         colour = NA) +
        ggplot2::geom_sf(data = sf::st_union(dem_tiles1()), colour = "black",
                         fill = NA, linewidth = 1) +
        ggplot2::geom_sf(data = watershed(), linewidth = 2, fill = NA) +
        ggplot2::geom_sf(data = wells(), size = 1,
                         ggplot2::aes(colour = is.na(.data$elev))) +
        ggplot2::scale_fill_viridis_c(name = "Elevation (m)") +
        ggplot2::labs(caption = paste0(
          "Note: DEM downsampled for plotting\n",
          "Source: ", stringr::str_remove(input$spatial_file$name[1], "\\..+$"))) +
        ggplot2::scale_colour_manual(
          name = "",
          labels = c("TRUE" = "Elevation Missing", "FALSE" = "Elevation Present"),
          values = c("TRUE" = "#CD4071FF", "FALSE" = "#180F3EFF")) +
        ggplot2::labs(title = tools::toTitleCase(title))

      removeNotification(id)

      g
    }, res = 100) %>%
      bindCache(input$spatial_file, input$dem_combo)


    # wells ----------------------------------
    wells <- reactive({
      req(watershed(), dem1())

      input$dem_combo

      id <- showNotification("Filtering well data...",
                             type = "message",
                             duration = NULL, closeButton = FALSE)

      withCallingHandlers({
        message("Wells - Start")
        w <- wells_subset(watershed(), fix_bottom = input$fix_bottom) %>%
          wells_elev(dem1(), dem2())
        message("Wells - Done")
      },
      message = function(m) {
        shinyjs::html(id = NS(id, "messages"), html = m$message, add = TRUE)
      })

      removeNotification(id)
      w
    }) %>%
      bindCache(input$spatial_file, input$dem_combo, input$fix_bottom)

    # wells table -------------------------------
    output$wells_table <- DT::renderDataTable({
      wells() %>%
        sf::st_drop_geometry() %>%
        aq_dt(filename = "wells")
    }, server = FALSE)

    # Outputs
    wells
  })
}

dem_region_shiny <- function(type, watershed, session) {

  id <- showNotification(paste0("Fetching ", type, " data..."),
                         duration = NULL, closeButton = FALSE)

  withCallingHandlers({
    message(stringr::str_to_title(type), " - Start")

    # Catch errors if have issues and try again
    l <- try(dem_region(watershed, type = type), silent = TRUE)
    #l <- try(stop("testing"), silent = TRUE)
    if(inherits(l, "try-error")) {
      message("  Problem with ", type, " tiles, trying again...")
      l <- tryCatch(
        dem_region(watershed, type = type),
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
