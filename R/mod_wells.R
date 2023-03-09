ui_wells <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      box(
        width = 12,
        title = "Process data",
        uiOutput(ns("data_warning")),
        fileInput(ns("spatial_file"),
                  label = "Choose spatial file defining watershed area",
                  buttonLabel = "Upload Spatial Data", multiple = TRUE),
      ),
      box(
        width = 12,
        title = "Options",
        checkboxInput(ns("toggle_lidar_plot"), "Plot Lidar")
      ),
      box(title = "Messages", width = 12, height = 350,
          div(style = "overflow-y:scroll;max-height:330px",
              verbatimTextOutput(ns("messages"), placeholder = TRUE)),
          shinyWidgets::progressBar(
            title = "Current Lidar tile:",
            id = ns("lidar_progress"), value = 0, display_pct = TRUE)
      )
    ),

    column(
      width = 8,
      tabBox(
        width = 12,
        id = "output",
        tabPanel("Maps", plotOutput(ns("map_plot"), height = "650px")),
        tabPanel("Wells Data",
                 DT::dataTableOutput(ns("wells_table")))
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


    watershed <- reactive({
      req(input$spatial_file)

      id <- showNotification("Loading spatial file...",
                             duration = NULL, closeButton = FALSE)
      type <- ext(input$spatial_file$datapath)

      if(any(type == "shp")) {
        file.rename(input$spatial_file$datapath,
                    file.path(tempdir(), input$spatial_file$name))
        f <- file.path(tempdir(), input$spatial_file$name)[type == "shp"]
      } else if(nrow(input$spatial_file) == 1 && type == "zip"){
        f <- unzip(input$spatial_file$datapath, list = TRUE)
        unzip(input$spatial_file$datapath, exdir = tempdir())
        f <- stringr::str_subset(f$Name, "shp$") %>%
          file.path(tempdir(), .)
      }
      removeNotification(id)
      sf::st_read(f)
    })

    output$map_plot <- renderPlot({
      req(watershed(), wells())

      id <- showNotification("Plotting data...",
                             duration = NULL, closeButton = FALSE)

      g <- ggplot2::ggplot() +
        ggthemes::theme_map() +
        ggplot2::theme(legend.position = "right") +
        ggplot2::geom_sf(data = watershed(), linewidth = 2, fill = "white")

      if(isTRUE(input$toggle_lidar_plot)) {

        showNotification("Plotting Lidar data...", id = id,
                         duration = NULL, closeButton = FALSE)
        ds <- nrow(lidar()) / 100
        temp <- stars::st_downsample(lidar(), n = ds) %>% # Downsample first
          sf::st_as_sf(as_points = FALSE, merge = TRUE)         # Convert to polygons

        g <- g +
          ggplot2::geom_sf(data = temp, ggplot2::aes(fill = elev),
                           colour = NA, alpha = 0.8) +
          ggplot2::scale_fill_viridis_c(name = "Elevation (m)")

      }

      removeNotification(id)

      g +
        ggplot2::geom_sf(data = wells(), size = 1,
                         ggplot2::aes(colour = is.na(.data$elev))) +
        ggplot2::scale_colour_manual(
          name = "",
          labels = c("Elevation Missing", "Elevation Present"),
          values = c("TRUE" = "#CD4071FF", "FALSE" = "#180F3EFF"))
    }, res = 100) %>%
      bindCache(input$spatial_file, input$toggle_lidar_plot)

    lidar <- reactive({
      req(watershed())
      id <- showNotification("Fetching lidar data...",
                             duration = NULL, closeButton = FALSE)

      withCallingHandlers({
        message("Lidar - Start")

        p <- shinyhttr::progress(session, id = "lidar_progress")

        # Catch errors if have download issues and try again
        l <- try(lidar_region(watershed(), progress = p), silent = TRUE)
        if(inherits(t, "try-error")) l <- lidar_region(watershed(), progress = p)
        message("Lidar - Done")
      },
      message = function(m) {
        shinyjs::html(id = "messages", html = m$message, add = TRUE)
      })

      removeNotification(id)
      l
    })


    wells <- reactive({
      req(watershed(), lidar())
      id <- showNotification("Filtering well data...",
                             type = "message",
                             duration = NULL, closeButton = FALSE)

      withCallingHandlers({
        message("Wells - Start")
        w <- wells_subset(watershed()) %>%
          wells_elev(lidar())
        message("Wells - Done")
      },
      message = function(m) {
        shinyjs::html(id = "messages", html = m$message, add = TRUE)
      })

      removeNotification(id)
      w
    })


    output$wells_table <- DT::renderDataTable({
      wells() %>%
        sf::st_drop_geometry() %>%
        aq_dt()
    })

    # Outputs
    wells
  })
}


ui_exports <- function(id) {

  fluidRow(
    column(
      width = 12,
      box(
        width = 12)
    )
  )
}
