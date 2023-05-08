
ext <- function(file) {
 stringr::str_extract(file, "(?<=.)[[:alpha:]]{2,4}$")
}


have_reactive <- function(reactive) {
  !inherits(try(reactive, silent = TRUE), "try-error")
}

# Regular expression patterns

# Match Fraction
p_fraction <- function() {
  "(\\d+ )?(\\d( )?/( )?\\d)"
}

# Match double number
p_dbl <- function() {
  "((\\d+\\.)?\\d+)"
}

# Match double with range
p_range <- function() {
  r <- "(to|-)"
  paste0("(", p_dbl(), "( )?", r, "( )?", p_dbl(), ")")
}


fix_fraction <- function(x) {
  f <- stringr::str_extract_all(x, p_fraction()) %>%
    unlist() %>%
    unique()

  if(length(unlist(f)) > 0) {
    n <- f %>%
      purrr::map_chr(
        ~ stringr::str_replace_all(.x, "( )?/( )?", "/") %>%
          stringr::str_split(" ") %>%
          purrr::map(\(x) purrr::map_vec(x, \(x) eval(parse(text = x)))) %>%
          purrr::map_dbl(~sum(.x)) %>%
          as.character()) %>%
      stats::setNames(paste0("(?<!\\d( )?)", f, "(?!( )?\\d)"))

    x <-stringr::str_replace_all(x, n)
  }
  x
}

fix_range <- function(x) {
  stringr::str_replace(x, p_range(), "mean(c(\\2,\\7))") %>%
    purrr::map_vec(\(x) eval(parse(text = x)))
}



aq_dt <- function(data, pageLength = 18, buttons = TRUE) {
  if(!buttons) {
    data %>%
      DT::datatable(options = list(pageLength = pageLength, scrollX = TRUE))
  } else {
    data %>%
      DT::datatable(extensions = c("Buttons"),
                    options = list(pageLength = pageLength,
                                   scrollX = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('csv', 'excel', I('colvis'))))
  }
}
