test_that("compact built-in fixtures provide Control and Treatment metadata", {
  for (source in c("DIA-NN", "FragPipe", "Skyline", "OpenMS")) {
    info <- .protvis_builtin_sample_info(source)
    expect_equal(nrow(info), 4L)
    expect_equal(info$maxquant_id,
                 c("S1_Control", "S2_Control", "S3_Treatment", "S4_Treatment"))
    expect_equal(info$group, c("Control", "Control", "Treatment", "Treatment"))
    expect_equal(info$condition, info$group)
  }
})
