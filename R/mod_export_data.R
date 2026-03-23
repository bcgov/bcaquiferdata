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

#' Test Export Module
#'
#' @param watershed Character. Path to watershed spatial file to test with.
#' @param source Character vector. "lidar", "trim" or both to specify sources.
#'
#' @returns Launches test version of Export Module
#'
#' @noRd
#' @examples
#' test_mod_export() # Clinton creek lidar
#' test_mod_export(source = c("lidar", "trim")) # Clinton creek lidar and trim
#' test_mod_export(source = "trim") # Clinton creek trim
#'
#' w <- "misc/data/Koksilah_watershed4/Koksilah_watershed4.shp"
#' test_mod_export(w)
#' test_mod_export(w, "trim")

test_mod_export <- function(
  watershed = "misc/data/Clinton_Creek.shp",
  source = "lidar"
) {
  watershed <- sf::st_read(watershed)
  wells <- wells_subset(watershed)
  dem <- purrr::map(source, \(s) dem_region(watershed, source = s)) |>
    rlang::set_names(source)
  if (length(dem) == 1) {
    wells <- wells_elev(wells, dem[[1]])
  } else {
    wells <- wells_elev(wells, dem[[1]], dem[[2]])
  }

  ui <- tagList(
    page_navbar(
      title = "BC Aquifer Data",
      theme = aq_theme(),
      ui_export_data("export_data")
    ),
    shinyjs::useShinyjs() # Set up shinyjs
  )

  server <- function(input, output, session) {
    wells_list <- list(
      wells = reactive(wells),
      watershed = reactive(watershed),
      dem = reactive(dem)
    )

    server_export_data("export_data", wells_list)
  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))
}


ui_export_data <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Exports",
    navset_card_pill(
      sidebar = sidebar(
        textInput(
          ns("export_id"),
          h4(aq_tt("File ID", "Prefix for the files to be exported")),
          value = "xxx"
        ),
        p(),
        h4(aq_tt("Output folder", "Where should the exported files be saved?")),
        textOutput(ns("export_dir")),
        p(),
        shinyDirButton(
          ns("choose_export_dir"),
          "Choose output folder",
          "Choose where to save files"
        ),
        uiOutput(ns("fixes"), inline = TRUE)
      ),

      # UI - Strater -------------
      nav_panel(
        title = "Strater",
        actionButton(ns("export_strater"), "Export", width = 150),
        textOutput(ns("feedback_strater")),
        navset_card_tab(
          nav_panel(
            title = h5(
              "Lithology (",
              textOutput(ns("strater_f1"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_strater_f1"))
          ),

          nav_panel(
            title = h5(
              "Collars (",
              textOutput(ns("strater_f2"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_strater_f2"))
          ),

          nav_panel(
            title = h5(
              "Wells (",
              textOutput(ns("strater_f3"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_strater_f3"))
          )
        )
      ),

      # UI - Voxler -------------
      nav_panel(
        title = "Voxler",
        actionButton(ns("export_voxler"), "Export", width = 150),
        textOutput(ns("feedback_voxler")),

        h3("Voxler file (", textOutput(ns("voxler_f1"), container = code), ")"),
        aq_dt_output(ns("table_voxler_f1"))
      ),

      # UI - ArcHydro -------------
      nav_panel(
        title = "ArcHydro",
        actionButton(ns("export_archydro"), "Export", width = 150),
        textOutput(ns("feedback_archydro")),
        navset_card_tab(
          nav_panel(
            title = h5(
              "Wells (",
              textOutput(ns("archydro_f1"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_archydro_f1"))
          ),

          nav_panel(
            title = h5(
              "HGU ID (lithology ",
              textOutput(ns("archydro_f2"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_archydro_f2"))
          ),

          nav_panel(
            title = h5(
              "BH (lithology index ",
              textOutput(ns("archydro_f3"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_archydro_f3"))
          )
        )
      ),

      # UI - Leapfrog -------------
      nav_panel(
        title = "Leapfrog",
        actionButton(ns("export_leapfrog"), "Export", width = 150),
        textOutput(ns("feedback_leapfrog")),
        navset_card_tab(
          nav_panel(
            title = h5(
              "Collars (",
              textOutput(ns("leapfrog_f1"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_leapfrog_f1"))
          ),

          nav_panel(
            title = h5(
              "Intervals (",
              textOutput(ns("leapfrog_f2"), container = code),
              ")"
            ),
            aq_dt_output(ns("table_leapfrog_f2"))
          )
        )
      ),

      # UI - Surfer -------------
      nav_panel(
        title = "Surfer",
        actionButton(ns("export_surfer"), "Export", width = 150),
        textOutput(ns("feedback_surfer")),

        h3("Surfer file (", textOutput(ns("surfer_f1"), container = code), ")"),
        aq_dt_output(ns("table_surfer_f1"))
      ),

      # UI - DEM -------------
      nav_panel(
        title = "Spatial Files",
        uiOutput(ns("ui_dem"))
      )
    )
  )
}

server_export_data <- function(id, wells_list) {
  moduleServer(id, function(input, output, session) {
    # Setup ------------------
    rlang::env_bind(rlang::current_env(), !!!wells_list)
    ns <- session$ns

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

    shinyDirChoose(
      input,
      "choose_export_dir",
      session = session,
      roots = volumes
    )

    # Setup Directory ------------------
    export_id <- reactive(janitor::make_clean_names(input$export_id))

    export_dir <- reactive({
      if (is.integer(input$choose_export_dir)) {
        NA_character_
      } else {
        parseDirPath(volumes, input$choose_export_dir)
      }
    })

    output$export_dir <- renderText({
      if (is.na(export_dir())) "No output directory selected" else export_dir()
    })

    # Spatial UI ------------------------------------
    output$ui_dem <- renderUI({
      btns <- purrr::map(names(dem()), \(s) {
        actionButton(
          ns(paste0("export_dem_", s)),
          paste("Export", stringr::str_to_title(s), "DEM"),
          width = 200
        )
      })

      cols <- purrr::map(names(dem()), \(s) {
        card(
          textOutput(ns(paste0("feedback_dem_", s))),
          h3(
            stringr::str_to_title(s),
            "DEM file (",
            textOutput(ns(paste0("dem_", s)), container = code),
            ")"
          ),
          em("Downsampled Preview"),
          aq_spinner(plotOutput(ns(paste0("plot_dem_", s))))
        )
      })

      if (length(dem()) > 1) {
        t <- tagList(layout_column_wrap(!!!btns), layout_column_wrap(!!!cols))
      } else {
        t <- tagList(btns, cols)
      }
      t
    })

    # Setup ----------------------
    feedback <- reactiveVal(list())

    observe({
      feedback(list())
    }) %>%
      bindEvent(wells())

    # Functions
    observe_export <- function(type) {
      observe({
        req(!is.na(export_dir()))
        wells_export(wells(), id = export_id(), type = type, dir = export_dir())
        f <- feedback()
        f[[type]] <- paste(
          stringr::str_to_title(type),
          "files exported"
        )
        feedback(f)
      }) %>%
        bindEvent(input[[paste0("export_", type)]], ignoreInit = TRUE)
    }

    observe_export_dem <- function(source) {
      observe({
        req(!is.na(export_dir()))
        dem_file <- file.path(
          export_dir(),
          stringr::str_subset(files(), source)
        )

        id <- showNotification(
          tagList(
            paste("Saving", stringr::str_to_title(source), "DEM..."),
            br(),
            span("File: ", dem_file, style = "font-size:80%"),
            br(),
            span(
              "This may take a few minutes, message will disappear when complete",
              style = "font-size:80%"
            )
          ),
          closeButton = FALSE,
          duration = NULL
        )

        dem_region(
          watershed(),
          source = source,
          overwrite = TRUE,
          out_file = dem_file
        )

        removeNotification(id)
        f <- feedback()
        f[[paste0("dem_", source)]] <- paste(
          stringr::str_to_title(source),
          "DEM file exported"
        )
        feedback(f)
      }) %>%
        bindEvent(input[[paste0("export_dem_", source)]], ignoreInit = TRUE)
    }

    feedback_output <- function(type) {
      req(!is.null(volumes))
      renderText({
        validate(need(
          export_dir() != "No output directory selected",
          "Please choose an output directory"
        ))
        validate(need(
          dir.exists(export_dir()),
          "The choose output directory does not exist, please
                    choose another one"
        ))
        feedback()[[type]]
      })
    }

    # Messaging --------------------

    # If any things need to be fixed (and haven't already), let the user know
    output$fixes <- renderUI({
      f1 <- any(wells()$flag_int_bottom) & !any(wells()$fix_int_bottom)
      f2 <- any(wells()$flag_depth_mismatch)

      if (f1 || f2) {
        t <- tagList(strong("LeapFrog Export:"), br())
      } else {
        t <- tagList()
      }

      if (f1) {
        t <- tagList(
          t,
          p(
            "Forcing thickness of bottom lithology intervals from 0m to 1m in wells:",
            br(),
            tags$ul(
              lapply(
                unique(wells()$well_tag_number[wells()$flag_int_bottom]),
                htmltools::tags$li
              )
            )
          )
        )
      }

      if (f2) {
        t <- tagList(
          t,
          p(
            "Forcing well depth to equal depth of the final lithology interval",
            br(),
            tags$ul(
              lapply(
                unique(wells()$well_tag_number[wells()$flag_depth_mismatch]),
                htmltools::tags$li
              )
            )
          )
        )
      }

      t
    })

    # Setup File IDs ------------------
    files <- reactive({
      paste0(
        export_id(),
        c(
          "_lith.csv",
          "_collars.csv",
          "_wls.csv",
          "_voxler.csv",
          "_archydro_well.csv",
          "_archydro_hguid.csv",
          "_archydro_bh.csv",
          "_leapfrog_collars.csv",
          "_leapfrog_intervals.csv",
          "_surfer.csv",
          "_dem_lidar.tif",
          "_dem_trim.tif"
        )
      )
    })

    # File headers
    output$strater_f1 <- renderText(files()[1])
    output$strater_f2 <- renderText(files()[2])
    output$strater_f3 <- renderText(files()[3])
    output$voxler_f1 <- renderText(files()[4])
    output$archydro_f1 <- renderText(files()[5])
    output$archydro_f2 <- renderText(files()[6])
    output$archydro_f3 <- renderText(files()[7])
    output$leapfrog_f1 <- renderText(files()[8])
    output$leapfrog_f2 <- renderText(files()[9])
    output$surfer_f1 <- renderText(files()[10])
    output$dem_lidar <- renderText(files()[11])
    output$dem_trim <- renderText(files()[12])

    # Export previews ---------------

    ## Data ----------------------------
    exp_strater <- reactive({
      wells_export(
        wells(),
        type = "strater",
        preview = TRUE
      )
    })
    exp_voxler <- reactive({
      wells_export(
        wells(),
        type = "voxler",
        preview = TRUE
      )
    })
    exp_archydro <- reactive({
      wells_export(
        wells(),
        type = "archydro",
        preview = TRUE
      )
    })
    exp_leapfrog <- reactive({
      wells_export(
        wells(),
        type = "leapfrog",
        preview = TRUE
      )
    })
    exp_surfer <- reactive({
      wells_export(
        wells(),
        type = "surfer",
        preview = TRUE
      )
    })

    ## Tables -------------------------------------------------
    output$table_strater_f1 <- DT::renderDataTable({
      aq_dt(exp_strater()[[1]], minimal = TRUE)
    })
    output$table_strater_f2 <- DT::renderDataTable(aq_dt(
      exp_strater()[[2]],
      minimal = TRUE
    ))
    output$table_strater_f3 <- DT::renderDataTable(aq_dt(
      exp_strater()[[3]],
      minimal = TRUE
    ))

    output$table_voxler_f1 <- DT::renderDataTable(aq_dt(
      exp_voxler()[[1]],
      minimal = TRUE
    ))

    output$table_archydro_f1 <- DT::renderDataTable(aq_dt(
      exp_archydro()[[1]],
      minimal = TRUE
    ))
    output$table_archydro_f2 <- DT::renderDataTable(aq_dt(
      exp_archydro()[[2]],
      minimal = TRUE
    ))
    output$table_archydro_f3 <- DT::renderDataTable(aq_dt(
      exp_archydro()[[3]],
      minimal = TRUE
    ))

    output$table_leapfrog_f1 <- DT::renderDataTable({
      aq_dt(
        exp_leapfrog()[[1]],
        minimal = TRUE
      )
    })
    output$table_leapfrog_f2 <- DT::renderDataTable(aq_dt(
      exp_leapfrog()[[2]],
      minimal = TRUE
    ))

    output$table_surfer_f1 <- DT::renderDataTable(aq_dt(
      exp_surfer()[[1]],
      minimal = TRUE
    ))

    ## Plots ---------------------------
    output$plot_dem_lidar <- renderPlot({
      dem_lidar <- dem()[["lidar"]]
      req(!is.null(dem_lidar))
      ds <- nrow(dem_lidar) / 75
      d <- stars::st_downsample(dem_lidar, n = ds)
      plot(d)
    })

    output$plot_dem_trim <- renderPlot({
      dem_trim <- dem()[["trim"]]
      req(!is.null(dem_trim))
      ds <- nrow(dem_trim) / 75
      d <- stars::st_downsample(dem_trim, n = ds)
      plot(d)
    })

    # Export files ---------------------
    ## Wells ------------------------
    output$feedback_strater <- feedback_output("strater")
    observe_export("strater")

    output$feedback_voxler <- feedback_output("voxler")
    observe_export("voxler")

    output$feedback_archydro <- feedback_output("archydro")
    observe_export("archydro")

    output$feedback_leapfrog <- feedback_output("leapfrog")
    observe_export("leapfrog")

    output$feedback_surfer <- feedback_output("surfer")
    observe_export("surfer")

    output$feedback_dem_lidar <- feedback_output("dem_lidar")
    observe_export_dem("lidar")

    output$feedback_dem_trim <- feedback_output("dem_trim")
    observe_export_dem("trim")
  })
}
