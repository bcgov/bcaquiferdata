aq_tt <- function(trigger, ..., alt = "More information") {
  bslib::tooltip(
    htmltools::span(
      trigger,
      bsicons::bs_icon("info-circle", title = alt)),
    ...
  )
}
