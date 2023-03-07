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
    expect_equal(lith_fix(desc = !!t$desc[i])[["lithology_category"]], !!t$cat[i])
  }

})



test_that("lith_yield()", {
  t <- dplyr::tribble(
    ~lithology_raw_data,   ~yield_units, ~flag_extra_digits, ~yield, ~depth, ~depth_units,
    "2 to 3 gpm",     "gpm",  "", 2.5,    as.double(), "",
    "50 gpm. and it takes 14 hrs. to recover", "gpm", "14", 50, as.double(), "",
    "365' - 2 1/2 gpm  s", "gpm", "", 2.5,   365, "ft",
    "45feet - 1/2-3 gpm","gpm", "", 1.75, 45, "ft",
    "5 gpm at 50', 10 gpm at 75'", "gpm", "", c(5, 10), c(50, 75), "ft",
    "60 and 80; 5gpm", "gpm", "60;80", 5, as.double(), "",
    "425 feet 1 3/4 gpm", "gpm", "", 1.75, 425, "ft",
    "fine t0 medium brown sand and gravel wet 3 gpm", "gpm", "0", 3, as.double(), "") %>%
    dplyr::select("lithology_raw_data", "flag_extra_digits", "depth", "depth_units", "yield", "yield_units")

  l <- lith_yield(dplyr::select(t, "lithology_raw_data", "yield_units"))

  tidyr::unnest(l, c("yield", "depth"), keep_empty = TRUE)

  expect_equal(l, t)

})
