testthat::test_that("imputation missing-value plots exclude protein identifier columns", {
  df <- data.frame(
    ID = c("P1", "P2"),
    protein_id = c("P1", "P2"),
    S1 = c(1, NA),
    S2 = c(2, 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  plot_df <- ProtVis:::.protvis_imputation_plot_data(df)

  testthat::expect_false("ID" %in% names(plot_df))
  testthat::expect_false("protein_id" %in% names(plot_df))
  testthat::expect_identical(names(plot_df), c("S1", "S2"))
  testthat::expect_equal(nrow(plot_df), 2L)
})

testthat::test_that("imputation plot helper leaves matrix row names as metadata only", {
  df <- data.frame(
    S1 = c(1, NA),
    S2 = c(2, 3),
    row.names = c("P1", "P2"),
    check.names = FALSE
  )

  plot_df <- ProtVis:::.protvis_imputation_plot_data(df)

  testthat::expect_identical(names(plot_df), c("S1", "S2"))
  testthat::expect_identical(rownames(plot_df), c("P1", "P2"))
})
