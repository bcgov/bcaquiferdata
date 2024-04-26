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

fix_leading_zero <- function(x) {
  stringr::str_replace_all(x, "(?<!\\d)(\\.\\d+)", "0\\1")
}

# File names - https://stackoverflow.com/a/56276939
aq_dt <- function(data, filename = NULL, minimal = FALSE) {
  if(minimal) {
    opts <- list(dom = "t")
    ext <- list()
  } else {
    filename <- paste0(filename, "-", Sys.Date())
    opts <- list(dom = 'Bfrtip',
                 buttons = list(
                   I('colvis'),
                   list(extend = 'csv', title = filename),
                   list(extend = 'excel', title = filename)
                 ))
    ext <- "Buttons"
  }

  data %>%
    DT::datatable(
      rownames = FALSE,
      fillContainer = TRUE,
      options = append(
        list(pageLength = 14, scrollX = TRUE),
        opts),
      extensions = ext)
}
