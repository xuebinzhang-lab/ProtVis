testthat::test_that("statistical-engine volcano and DEP count plots are retained", {
  result <- data.frame(
    ID = paste0("P", 1:6),
    logFC = c(-2, -1, -0.1, 0.2, 1.1, 2.2),
    P.Value = c(0.001, 0.01, 0.8, 0.7, 0.02, 0.0005),
    adj.P.Val = c(0.006, 0.03, 0.9, 0.8, 0.04, 0.003),
    method = "limma",
    stringsAsFactors = FALSE
  )

  classified <- ProtVis:::.protvis_dep_engine_classify(
    result, fdr = 0.05, logfc = 0.27
  )
  testthat::expect_equal(
    sum(as.character(classified$data$.pv_regulation) == "Upregulated"), 2L
  )
  testthat::expect_equal(
    sum(as.character(classified$data$.pv_regulation) == "Downregulated"), 2L
  )

  volcano <- ProtVis:::.protvis_dep_engine_volcano_plot(
    result, "limma", "A vs B", fdr = 0.05, logfc = 0.27
  )
  count_plot <- ProtVis:::.protvis_dep_engine_bar_plot(
    result, "limma", "A vs B", fdr = 0.05, logfc = 0.27
  )
  testthat::expect_s3_class(volcano, "ggplot")
  testthat::expect_s3_class(count_plot, "ggplot")
})

testthat::test_that("statistical-engine heatmap uses the selected comparison matrix", {
  expression <- data.frame(
    A1 = c(1, 2, 4, 7),
    A2 = c(1.2, 2.1, 4.2, 7.1),
    B1 = c(4, 2.2, 1, 6),
    B2 = c(4.1, 2.3, 1.1, 6.2),
    row.names = paste0("P", 1:4),
    check.names = FALSE
  )
  sample_info <- data.frame(
    sample_id = c("A1", "A2", "B1", "B2"),
    group = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE,
    row.names = c("A1", "A2", "B1", "B2")
  )
  dataset <- create_protvis_dataset(
    expression_data = expression,
    sample_info = sample_info
  )
  result <- data.frame(
    ID = paste0("P", 1:4),
    logFC = c(-2, -0.1, 2, 0.2),
    P.Value = c(0.001, 0.8, 0.001, 0.7),
    adj.P.Val = c(0.004, 0.9, 0.004, 0.8),
    method = "limma",
    stringsAsFactors = FALSE
  )
  plot <- ProtVis:::.protvis_dep_engine_heatmap_plot(
    result,
    dataset = dataset,
    sample_sets = list(samples = c("A1", "A2", "B1", "B2")),
    method = "limma",
    comparison = "A vs B",
    fdr = 0.05,
    logfc = 0.27,
    top_n = 50
  )
  testthat::expect_s3_class(plot, "ggplot")
})
