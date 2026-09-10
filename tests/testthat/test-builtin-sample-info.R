test_that("built-in fixtures provide real WT and oxidative-stress metadata", {
  for (source in c("DIA-NN", "FragPipe", "Skyline", "OpenMS")) {
    info <- .protvis_builtin_sample_info(source)
    expect_equal(nrow(info), 8L)
    expect_equal(info$maxquant_id,
                 c("WT1", "WT2", "WT3", "WT4", "WT_H2O2_1", "WT_H2O2_2",
                   "WT_H2O2_3", "WT_H2O2_4"))
    expect_equal(info$group, c(rep("WT", 4), rep("WT_H2O2", 4)))
    expect_equal(info$condition, info$group)
    expect_true(all(info$organism == "Candida albicans"))
    expect_true(all(grepl("TraianProt/blob/main/inst/extdata/proteinGroups.txt",
                          info$source_url, fixed = TRUE)))
  }
})
