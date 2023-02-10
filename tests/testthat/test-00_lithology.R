test_that("lith_categorize()", {

  for(i in seq_len(length(lith_expect()))) {
    w <- lith_expect()[[i]]
    expect_equal(lith_categorize(w[[1]], w[[2]], w[[3]]), w[[4]], label = i)
  }

})


test_that("lith_fix()", {

  t <- system.file("extdata", "test_lithology_cleaning.csv",
                   package = "bcaquiferdata") %>%
    readr::read_csv(col_types = "cc") %>%
    suppressWarnings()

  for(i in seq(nrow(t))) {
    expect_equal(lith_fix(desc = !!t$desc[i])[["lith_category"]], !!t$cat[i])
  }

})



test_that("lith_yield()", {
  t <- dplyr::tribble(
    ~lithology_raw_data,   ~yield_units, ~digits_extra, ~yield, ~depth,
    "2 to 3 gpm",     "gpm",  NA_character_, 2.5,    NA_character_,
    "50 gpm. and it takes 14 hrs. to recover", "gpm", "14", 50, NA_character_,
    "365' - 2 1/2 gpm  s", "gpm", NA_character_, 2.5,   "365'",
    "45feet - 1/2-3 gpm","gpm", NA_character_, 1.75, "45feet",
    "5 gpm at 50', 10 gpm at 75'", "gpm", NA_character_, c(5, 10), c("50'", "75'"),
    "60 and 80; 5gpm", "gpm", "60;80", 5, NA_character_)

  l <- lith_yield(dplyr::select(t, "lithology_raw_data", "yield_units"), flatten = TRUE)

  expect_equal(l, t)

})
