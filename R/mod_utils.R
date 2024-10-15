aq_tt <- function(trigger, ..., alt = "tooltip") {
  bslib::tooltip(
    htmltools::span(
      trigger,
      bsicons::bs_icon("info-circle", title = alt)),
    ...
  )
}
