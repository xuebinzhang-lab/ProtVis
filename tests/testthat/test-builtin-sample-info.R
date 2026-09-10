test_that("built-in fixtures provide real B73 and Y12 metadata", {
  for (source in c("DIA-NN", "FragPipe", "Skyline", "OpenMS")) {
    info <- .protvis_builtin_sample_info(source)
    expect_equal(nrow(info), 30L)
    expect_equal(info$maxquant_id,
                 c(paste0("B73_TMT", rep(1:3, each = 5), "_", rep(1:5, 3)),
                   paste0("Y12_TMT", rep(1:3, each = 5), "_", rep(1:5, 3))))
    expect_equal(info$group, c(rep("B73", 15), rep("Y12", 15)))
    expect_equal(info$condition, info$group)
    expect_true(all(info$organism == "Zea mays"))
    expect_true(all(grepl("ProtVis/blob/dev/inst/extdata/Maxquant_Export.xlsx",
                          info$source_url, fixed = TRUE)))
  }
})
