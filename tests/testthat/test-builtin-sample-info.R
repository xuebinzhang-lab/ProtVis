test_that("built-in fixtures provide real B73 and Y12 metadata", {
  for (source in c("DIA-NN", "FragPipe", "Skyline", "OpenMS")) {
    info <- .protvis_builtin_sample_info(source)
    expect_equal(nrow(info), 2L)
    expect_equal(info$maxquant_id, c("B73_TMT1_1", "Y12_TMT1_1"))
    expect_equal(info$group, c("B73", "Y12"))
    expect_equal(info$condition, info$group)
    expect_true(all(info$organism == "Zea mays"))
    expect_true(all(grepl("ProtVis/blob/dev/inst/extdata/Maxquant_Export.xlsx",
                          info$source_url, fixed = TRUE)))
  }
})
