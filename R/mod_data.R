ui_data <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,

      box(
        title = "Update data", width = 6,
        valueBoxOutput(ns("data_status"), width = 6),
        strong("Note:"),
        "A cache directory will be created if it doesn't already exist", p(),
        actionButton(ns("data_download"), "Fetch GWELLS data"),
        actionButton(ns("data_cache"), "Clear GWELLS data cache")
      ),
      box(title = "Data Status", width = 6, tableOutput(ns("data_meta"))),

      box(title = "Messages", width = 12, height = 350,
          div(style = "overflow-y:scroll;max-height:330px",
              verbatimTextOutput(ns("messages"), placeholder = TRUE))
      ),


    )
  )
}

server_data <- function(id) {


  moduleServer(id, function(input, output, session) {

    data_check <- reactiveVal(TRUE)

    # Check data status
    meta <- reactive({
      browser()
      cache_meta()
    }) %>% bindEvent(data_check())
    #have_data <- reactive(data_ready()) %>% bindEvent(data_check())

    # Output metadata
    output$data_meta <- renderTable({
      browser()
      meta() %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Step", values_to = "Status")
    })

    # Output data status
    output$data_status <- renderValueBox({
      browser()

      h <- meta()$wells_processed != "" & meta()$lith_processed != ""
      if(!h) {
        v <- valueBox(value = "Missing Data",
                      subtitle = "Please download GWELLS data",
                      color = "red")
      } else {
        v <- valueBox(
          value = "Found Data",
          subtitle = paste0("Last downloaded: ", meta()$GWELLS_downloaded),
          color = "green")
      }
      data_check(FALSE)
      v
    }) %>% bindEvent(data_check())

    observe({
      browser()

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

    observe({
      removeModal()
      withCallingHandlers({
        cache_clean()
      },
      message = function(m) {
        shinyjs::html(id = "messages", html = m$message, add = TRUE)
      })

      data_check(TRUE)
    }) %>% bindEvent(input$data_cache_confirm)

    # Outputs
    #have_data
  })
}
