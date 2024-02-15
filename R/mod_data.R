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

ui_data <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Data Status",
    layout_columns(
      col_widths = c(6, 6, 12),
      row_heights = c(8, 17),
      card(
        card_header(h4("Data Status")),
        card_body(
          layout_column_wrap(
            width = NULL,
            style = htmltools::css(grid_template_columns = "3fr 5fr"),
            fill = FALSE,
            card_body(fillable = FALSE, uiOutput(ns("data_status"))),
            card_body(
              fillable = FALSE,
              strong("Cache directories"), br(),
              uiOutput(ns("cache_status")),
              actionButton(ns("data_download"), "Fetch/Update GWELLS data",
                           class = "btn-success m-2"),
              actionButton(ns("data_cache"), "Clear cache",
                           class = "btn-warning m-2")
            )
          )
        )
      ),
      card(card_header(h4("Details")), tableOutput(ns("data_meta"))),

      card(card_header("Messages"),
           div(style = "overflow-y:scroll;max-height:330px",
               verbatimTextOutput(ns("messages"), placeholder = TRUE))
      )
    )
  )
}

server_data <- function(id) {


  moduleServer(id, function(input, output, session) {

    data_check <- reactiveVal(TRUE)

    # Cache status ----
    output$cache_status <- renderUI({

      data_check()

      aq <- tagList("bcaquiferdata ", code(cache_dir()))
      if(!dir.exists(cache_dir())) aq <- tagList(aq, tags$small("(to be created)"))

      d <- file.path(bcmaps:::data_dir(), "cded")
      mp <- tagList("bcmaps ", code(d))
      if(!dir.exists(d)) mp <- tagList(mp, tags$small("(to be created)"))

      tagList(tags$ul(tags$li(aq), tags$li(mp)))

    })

    # Check data status ----
    meta <- reactive(cache_meta()) %>% bindEvent(data_check())
    have_data <- reactive(data_ready()) %>% bindEvent(data_check())

    # Output metadata ----
    output$data_meta <- renderTable({
      meta() %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            values_transform = as.character,
                            names_to = "Step", values_to = "Status")
    })

    # Output data status ----
    output$data_status <- renderUI({

      if(!have_data()) {
        v <- value_box(
          title = "Status",
          value = "Data Missing",
          theme_color = "danger", class = "nopad p-0",
          p("Please download GWELLS data"))
      } else {
        v <- value_box(
          title = "Status",
          value = "Data Found",
          theme_color = "success", class = "nopad p-0",
          p("Last downloaded: ", as.character(meta()$GWELLS_downloaded)))
      }
      data_check(FALSE)
      v
    }) %>% bindEvent(data_check())

    # Download -----------
    observe({

      msg_id <- showNotification("Downloading GWELLS data...",
                             duration = NULL, closeButton = FALSE)

      withCallingHandlers({
        message("**Fetch Data**")
        # Catch errors if have download issues and try again
        data_update(permission = TRUE)
        message("**Data Fetched**")

        data_check(TRUE)
      },
      message = function(m) {
        shinyjs::html(id = "messages", html = m$message, add = TRUE)
      })

      removeNotification(msg_id)
    }) %>% bindEvent(input$data_download)


    # Delete cache -----------------

    # Confirm
    observe({
      showModal(modalDialog(
        title = "Delete Cache?",
        span("Are you sure you want to delete the cache?",
             "You will have to re-download and process it again"),
        footer = tagList(
          actionButton(NS(id, "data_cache_confirm"), "Yes, delete the cache"),
          modalButton("Cancel"))
      ))
    }) %>% bindEvent(input$data_cache)

    # Proceed
    observe({
      removeModal()
      withCallingHandlers({
        cache_clean(bcmaps_cded = TRUE)
      },
      message = function(m) {
        shinyjs::html(id = "messages", html = m$message, add = TRUE)
      })

      data_check(TRUE)
    }) %>% bindEvent(input$data_cache_confirm)

    # Outputs -----------
    have_data
  })
}
