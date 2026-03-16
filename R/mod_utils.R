aq_tt <- function(trigger, ..., alt = "More information") {
  bslib::tooltip(
    htmltools::span(
      trigger,
      bsicons::bs_icon("info-circle", title = alt)
    ),
    ...
  )
}


# File names - https://stackoverflow.com/a/56276939
aq_dt <- function(data, filename = NULL, minimal = FALSE) {
  if (minimal) {
    opts <- list(dom = "tp")
    ext <- list()
  } else {
    filename <- paste0(filename, "-", Sys.Date())
    opts <- list(
      dom = 'Bfrtip',
      buttons = list(
        I('colvis'),
        list(extend = 'csv', title = filename),
        list(extend = 'excel', title = filename)
      )
    )
    ext <- "Buttons"
  }

  data %>%
    DT::datatable(
      rownames = FALSE,
      fillContainer = TRUE,
      options = append(
        list(pageLength = 14, scrollX = TRUE),
        opts
      ),
      extensions = ext
    )
}
