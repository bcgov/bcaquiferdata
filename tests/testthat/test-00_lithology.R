test_that("lith_categorize()", {

  for(i in seq_len(length(lith_expect))) {
    w <- lith_expect[[i]]
    expect_equal(lith_categorize(!!w[[1]], !!w[[2]], !!w[[3]]), !!w[[4]])
  }



})


test_that("lith_fix()", {

  t <- system.file("extdata", "test_lithology_cleaning.csv",
                   package = "bcaquiferdata") %>%
    readr::read_csv(show_col_types = FALSE)

  for(i in seq(nrow(t))) {
    expect_equal(lith_fix(desc = !!t$desc[i])[["lith_category"]], !!t$cat[i])
  }

})
