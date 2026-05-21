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
      sidebar = sidebar(
        width = "20%",
        gap = "1em",
        h4("Prepare data"),
        p(
          "Filter GWELLs to watershed area and use Lidar or TRIM digital ",
          "elevation models to calculate well elevation."
        ),
        p(
          uiOutput(ns("data_warning"), inline = TRUE),
          br(),
          uiOutput(ns("elev_warning"), inline = TRUE)
        ),

        # Shape file
        fileInput(
          ns("spatial_file"),
          label = aq_tt(
            "Choose shape file(s) defining a watershed",
            "Select multiple files while holding down the 'Ctrl' button, or select a zipped collection"
          ),
          buttonLabel = "Upload Spatial Data",
          multiple = TRUE
        ),

        # Elevation
        radioButtons(
          ns("dem_combo"),
          strong("DEM source"),
          inline = TRUE,
          choiceNames = list(
            "Lidar",
            span("TRIM", style = "margin-right:150px"),
            "Lidar with TRIM",
            "Custom DEM"
          ),
          choiceValues = c(
            "lidar",
            "trim",
            "lidar_trim",
            "custom"
          ),
          selected = character(0)
        ),
        conditionalPanel(
          condition = "input.dem_combo == 'custom'",
          layout_columns(
            aq_tt(
              "Choose DEM file(s) supplying elevation for your watershed",
              "Select multiple files while holding down the 'Ctrl' button"
            ),
            uiOutput(ns("dem_file_path")),
            shinyFiles::shinyFilesButton(
              ns("dem_file"),
              label = "Upload Custom DEM",
              title = "Choose DEM files(s)",
              multiple = TRUE
            ),
            col_widths = 12,
            gap = 0
          ),
          ns = NS(id)
        ),

        # Fixes
        checkboxGroupInput(
          ns("fixes"),
          label = strong("Fix common problems"),
          inline = TRUE,
          #choices = list("Zero-width bottom lithology intervals" = "fix_bottom",
          #              "Missing well depth" = "fix_depth"),
          choiceNames = list(
            aq_tt(
              "Zero-width bottom lithology intervals",
              "Fixed by adding 1m to both the final lithology depth and the well depth"
            ),
            aq_tt(
              "Missing well depth",
              "Fixed by using the final lithology depth, if it exists"
            )
          ),
          choiceValues = list("fix_bottom", "fix_depth"),
          selected = c("fix_bottom", "fix_depth")
        ),

        h4("Messages"),
        verbatimTextOutput(ns("messages"), placeholder = TRUE)
      ),
      nav_panel(
        "Maps",
        aq_spinner(plotOutput(ns("map_plot"), height = "650px"))
      ),
      nav_panel("Wells Data", aq_dt_output(ns("wells_table"))),
      nav_panel(
        "Info",
        includeMarkdown(
          system.file("extra_docs", "wells_desc.md", package = "bcaquiferdata")
        )
      )
    )
  )
}

server_wells <- function(id, have_data) {
  moduleServer(id, function(input, output, session) {
    # ShinyFiles -------------
    # VPN fix adapted from ccviR: https://github.com/LandSciTech/ccviR

    timeout <- R.utils::withTimeout(
      {
        volumes <- c(
          `Working Directory` = fs::path_wd(),
          Home = fs::path_home(),
          `All Drives` = shinyFiles::getVolumes()()
        )
      },
      timeout = 200,
      onTimeout = "silent"
    )

    if (is.null(timeout)) {
      stop(
        "Unable to find drives",
        "This can occur if a VPN was in use but disconnected.",
        "To fix, either reconnect to the VPN or restart without connecting",
        call. = FALSE
      )
    }

    # warnings -------------------------------------
    output$data_warning <- renderUI({
      if (have_data()) {
        w <- tagList(
          aq_tt(
            span(
              icon("check", style = "color:lightgreen;"),
              "Wells Data Available"
            ),
            "The GWELLs data has been downloaded and processed"
          )
        )
      } else {
        w <- tagList(
          aq_tt(
            span(icon("x", style = "color:red;"), "Wells Data Not Available"),
            "See the Download Data tab to download the GWELLS data before proceeding"
          )
        )
      }
      w
    })

    output$elev_warning <- renderUI({
      req(input$dem_combo)
      if (stringr::str_detect(input$dem_combo, "_")) {
        w <- tagList(
          aq_tt(
            span(
              icon("triangle-exclamation", style = "color:orange;"),
              "Multiple Elevation Sources"
            ),
            "Use caution when combining elevation from multiple sources. See the Info pane for more details"
          )
        )
      } else {
        w <- tagList(
          aq_tt(
            span(
              icon("check", style = "color:lightgreen;"),
              "Single Elevation Source"
            ),
            "It is safest to use elevation from a single source"
          )
        )
      }
      w
    })

    # UI -------------------------------
    shinyFiles::shinyFileChoose(input, "dem_file", root = volumes)

    output$dem_file_path <- renderUI({
      req(!is.null(input$dem_file), !is.numeric(input$dem_file))
      if (is_ready(watershed())) {
        validate(
          need(
            dem_custom() != "no overlap",
            "No overlap between watershed and custom DEM"
          ),
          errorClass = "problem"
        )
      }
      tagList(
        span(
          "File: ",
          code(shinyFiles::parseFilePaths(roots = volumes, input$dem_file)$name)
        )
      )
    })

    # fixes ---------------------------
    fixes <- reactive({
      list(
        "fix_bottom" = "fix_bottom" %in% input$fixes,
        "fix_depth" = "fix_depth" %in% input$fixes
      )
    })

    # watershed -----------------------------
    watershed <- reactive({
      req(input$spatial_file)

      id <- showNotification(
        "Loading spatial file...",
        duration = NULL,
        closeButton = TRUE
      )
      type <- ext(input$spatial_file$datapath)

      if (all(c("shp", "shx", "dbf", "prj") %in% type)) {
        file.rename(
          input$spatial_file$datapath,
          file.path(tempdir(), input$spatial_file$name)
        )
        f <- file.path(tempdir(), input$spatial_file$name)[type == "shp"]
      } else if (nrow(input$spatial_file) == 1 && type == "zip") {
        f <- utils::unzip(input$spatial_file$datapath, list = TRUE)
        utils::unzip(input$spatial_file$datapath, exdir = tempdir())
        f <- stringr::str_subset(f$Name, "shp$") %>%
          file.path(tempdir(), .)
      } else {
        validate(need(
          FALSE,
          {
            removeNotification(id)
            paste0(
              "Cannot detect file type. Must be a shapefile including a ",
              "shp, shx, prj, and dbf file (can be zipped or multiple selected)"
            )
          }
        ))
      }
      removeNotification(id)
      sf::st_read(f)
    })

    # dem elevation ----------------------------------
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

    dem_custom <- reactive({
      req(watershed(), !is.null(input$dem_file), !is.numeric(input$dem_file))
      p <- shinyFiles::parseFilePaths(roots = volumes, input$dem_file)
      dem_region_shiny(p$datapath, watershed(), session)
    }) # |>
    #bindCache(input$spatial_file, input$dem_file)

    dem_source1 <- reactive({
      req(input$dem_combo)
      stringr::str_extract(input$dem_combo, "^[^_]+")
    })

    dem_source2 <- reactive({
      req(input$dem_combo)
      if (stringr::str_detect(input$dem_combo, "_")) {
        stringr::str_extract(input$dem_combo, "[^_]+$")
      } else {
        NULL
      }
    })

    dem1 <- reactive({
      req(watershed(), dem_source1())
      if (dem_source1() == "lidar") {
        dem_lidar()
      } else if (dem_source1() == "trim") {
        dem_trim()
      } else if (dem_source1() == "custom" && inherits(dem_custom(), "stars")) {
        dem_custom()
      }
    })

    dem2 <- reactive({
      req(watershed())
      if (!is.null(dem_source2())) {
        if (dem_source2() == "lidar") dem_lidar() else dem_trim()
      } else {
        NULL
      }
    })

    # map -----------------------------

    # Down sample and convert to points
    dem_tiles1 <- reactive({
      ds <- nrow(dem1()) / 150
      stars::st_downsample(dem1(), n = ds) %>%
        sf::st_as_sf(as_points = FALSE)
    }) %>%
      bindCache(input$spatial_file, dem_source1())

    dem_tiles2 <- reactive({
      if (!is.null(dem_source2())) {
        ds <- nrow(dem2()) / 150
        stars::st_downsample(dem2(), n = ds) %>%
          sf::st_as_sf(as_points = FALSE)
      } else {
        NULL
      }
    }) %>%
      bindCache(input$spatial_file, dem_source2())

    output$map_plot <- renderPlot(
      {
        validate(need(
          input$spatial_file,
          "Please load a watershed spatial file"
        ))
        validate(need(input$dem_combo, "Please choose a DEM source"))

        req(watershed(), wells(), dem1())

        id <- showNotification(
          "Plotting data...",
          duration = NULL,
          closeButton = FALSE
        )

        g <- ggplot2::ggplot() +
          ggthemes::theme_map() +
          ggplot2::theme(legend.position = "right")

        # Use file name if custom
        if (input$dem_combo == "custom") {
          nm <- shinyFiles::parseFilePaths(roots = volumes, input$dem_file)$name
        } else {
          nm <- dem_source1()
        }

        title <- paste("Elevation Data:", nm)
        if (!is.null(dem_tiles2())) {
          title <- paste(title, "supplemented with", dem_source2())
          g <- g +
            ggplot2::geom_sf(
              data = dem_tiles2(),
              ggplot2::aes(fill = .data$elev),
              colour = NA
            )
        }

        g <- g +
          ggplot2::geom_sf(
            data = dem_tiles1(),
            ggplot2::aes(fill = .data$elev),
            colour = NA
          ) +
          ggplot2::geom_sf(
            data = sf::st_union(dem_tiles1()),
            colour = "black",
            fill = NA,
            linewidth = 1
          ) +
          ggplot2::geom_sf(data = watershed(), linewidth = 2, fill = NA) +
          ggplot2::geom_sf(
            data = wells(),
            size = 1,
            ggplot2::aes(colour = is.na(.data$elev))
          ) +
          ggplot2::scale_fill_viridis_c(name = "Elevation (m)") +
          ggplot2::labs(
            caption = paste0(
              "Note: DEM downsampled for plotting\n",
              "Source: ",
              stringr::str_remove(input$spatial_file$name[1], "\\..+$")
            )
          ) +
          ggplot2::scale_colour_manual(
            name = "",
            labels = c(
              "TRUE" = "Elevation Missing",
              "FALSE" = "Elevation Present"
            ),
            values = c("TRUE" = "#CD4071FF", "FALSE" = "#180F3EFF")
          ) +
          ggplot2::labs(title = tools::toTitleCase(title))

        removeNotification(id)

        g
      },
      res = 100
    ) %>%
      bindCache(input$spatial_file, input$dem_combo, input$dem_file)

    # wells ----------------------------------
    wells <- reactive({
      req(watershed(), dem1())

      input$dem_combo

      id <- showNotification(
        "Filtering well data...",
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )

      withCallingHandlers(
        {
          message("Wells - Start")
          w <- wells_subset(
            watershed(),
            fix_bottom = fixes()$fix_bottom,
            fix_depth = fixes()$fix_depth
          ) %>%
            wells_elev(dem1(), dem2())
          message("Wells - Done")
        },
        message = function(m) {
          shinyjs::html(id = "messages", html = m$message, add = TRUE)
        }
      )

      removeNotification(id)
      w
    }) %>%
      bindCache(input$spatial_file, input$dem_combo, input$dem_file, fixes())

    # wells table -------------------------------
    output$wells_table <- DT::renderDataTable(
      {
        wells() %>%
          sf::st_drop_geometry() %>%
          aq_dt(filename = "wells")
      },
      server = FALSE
    )

    # Outputs
    list(
      wells = wells,
      watershed = watershed,
      dem = reactive({
        if (!is.null(dem2())) {
          list(dem1(), dem2()) %>%
            rlang::set_names(c(dem_source1(), dem_source2()))
        } else {
          list(dem1()) %>% rlang::set_names(dem_source1())
        }
      })
    )
  })
}

dem_region_shiny <- function(source, watershed, session) {
  id <- showNotification(
    paste0("Fetching ", source, " data..."),
    duration = NULL,
    closeButton = FALSE
  )

  source_name <- dplyr::if_else(
    source %in% c("lidar", "trim"),
    stringr::str_to_title(source),
    fs::path_file(source)
  )

  withCallingHandlers(
    {
      message("DEM - ", source_name)

      # Catch errors if have issues and try again
      if (source %in% c("lidar", "trim")) {
        l <- try(dem_region(watershed, source = source), silent = TRUE)
        #l <- try(stop("testing"), silent = TRUE)

        if (inherits(l, "try-error")) {
          message("  Problem with ", source_name, ", trying again...")
          l <- tryCatch(
            dem_region(watershed, source = source),
            #stop("testing2"),
            error = function(cond) {
              message(
                "  Problem fetching ",
                source_name,
                "\n",
                "  Error message: ",
                cond$message
              )
            }
          )
        }
      } else {
        l <- tryCatch(
          dem_region(watershed, source = source),
          error = function(cond) {
            message(
              "  Problem processing ",
              source_name,
              "\n",
              "  Error message: ",
              cond$message
            )
            return("no overlap")
          }
        )
      }

      message("Done - ", source_name)
    },
    message = function(m) {
      shinyjs::html(id = "messages", html = m$message, add = TRUE)
    }
  )

  removeNotification(id)
  l
}
