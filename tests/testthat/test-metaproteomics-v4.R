testthat::test_that("metaproteomics demo produces all analysis layers", {
  dat <- metaproteomics_demo_data()

  result <- ProtVis:::.mp_prepare_analysis(
    dat,
    tax_level = "Genus",
    function_level = "Pathway",
    relative = TRUE,
    top_n = 8L
  )

  testthat::expect_true(nrow(result$tables$merged) > 0L)
  testthat::expect_true(nrow(result$tables$taxonomy_composition) > 0L)
  testthat::expect_true(nrow(result$tables$function_composition) > 0L)
  testthat::expect_true(nrow(result$tables$taxonomy_differential) > 0L)
  testthat::expect_true(nrow(result$tables$function_differential) > 0L)
  testthat::expect_true(all(c("log2FC", "P.Value", "adj.P.Val") %in% names(result$tables$taxonomy_differential)))
  testthat::expect_true(nrow(result$tables$taxon_function) > 0L)
  testthat::expect_true(nrow(result$tables$peptide_function_scores) > 0L)
  testthat::expect_true(nrow(result$tables$peptide_function_differential) > 0L)

  testthat::expect_setequal(
    unique(result$tables$merged$Sample),
    dat$sample_info$sample_id
  )
})

testthat::test_that("schema v4 metaproteomics runs append without replacing the active matrix", {
  dat <- metaproteomics_demo_data()
  expression <- dat$abundance[, -1L, drop = FALSE]
  rownames(expression) <- dat$abundance$ProteinID

  object <- create_protvis_dataset(
    expression_data = expression,
    sample_info = dat$sample_info,
    metadata = list(source = "test")
  )
  original <- object$expression_data

  result <- ProtVis:::.mp_prepare_analysis(
    dat,
    tax_level = "Genus",
    function_level = "Pathway",
    relative = TRUE,
    top_n = 10L
  )

  run1 <- ProtVis:::.mp_result_run(result, source = "test")
  run1$run_id <- "metaproteomics_test_1"
  object <- ProtVis:::.mp_append_run(object, run1)

  run2 <- ProtVis:::.mp_result_run(result, source = "test")
  run2$run_id <- "metaproteomics_test_2"
  object <- ProtVis:::.mp_append_run(object, run2)

  testthat::expect_identical(protvis_schema_version(), "4.0.0")
  testthat::expect_equal(object$expression_data, original)
  testthat::expect_equal(
    length(object$analysis_results$metaproteomics$runs),
    2L
  )
  testthat::expect_true(all(
    c("metaproteomics_test_1", "metaproteomics_test_2") %in%
      names(object$analysis_results$metaproteomics$runs)
  ))
  testthat::expect_identical(
    object$analysis_results$metaproteomics$latest_run_id,
    "metaproteomics_test_2"
  )
})

testthat::test_that("active ProtVis_dataset can seed metaproteomics inputs", {
  dat <- metaproteomics_demo_data()
  expression <- dat$abundance[, -1L, drop = FALSE]
  rownames(expression) <- dat$abundance$ProteinID

  variable_info <- merge(
    dat$taxonomy,
    dat[["function"]],
    by = "ProteinID",
    all = TRUE
  )
  variable_info$variable_id <- variable_info$ProteinID
  variable_info$protein_id <- variable_info$ProteinID
  variable_info <- variable_info[
    match(rownames(expression), variable_info$ProteinID),
    ,
    drop = FALSE
  ]

  object <- create_protvis_dataset(
    expression_data = expression,
    sample_info = dat$sample_info,
    variable_info = variable_info,
    metadata = list(source = "test")
  )
  object <- register_protvis_assay(
    object,
    level = "peptide",
    data = dat$peptide,
    source = "test"
  )

  extracted <- ProtVis:::.mp_data_from_dataset(object)

  testthat::expect_equal(nrow(extracted$abundance), nrow(dat$abundance))
  testthat::expect_true("Genus" %in% names(extracted$taxonomy))
  testthat::expect_true("Pathway" %in% names(extracted[["function"]]))
  testthat::expect_equal(nrow(extracted$peptide), nrow(dat$peptide))
})

testthat::test_that("peptide sample columns are not offered as function levels", {
  dat <- metaproteomics_demo_data()
  levels <- ProtVis:::.mp_function_levels(dat[["function"]], dat$peptide)

  testthat::expect_true(all(c("Pathway", "KO", "COG", "CAZy") %in% levels))
  testthat::expect_false(any(grepl("Control_|Treatment_", levels)))
})


testthat::test_that("stored metaproteomics inputs are reusable after project resume", {
  dat <- metaproteomics_demo_data()
  expression <- dat$abundance[, -1L, drop = FALSE]
  rownames(expression) <- dat$abundance$ProteinID

  object <- create_protvis_dataset(
    expression_data = expression,
    sample_info = dat$sample_info,
    metadata = list(source = "resume_test")
  )
  result <- ProtVis:::.mp_prepare_analysis(
    dat,
    tax_level = "Genus",
    function_level = "Pathway",
    relative = TRUE,
    top_n = 10L
  )
  run <- ProtVis:::.mp_result_run(result, source = "resume_test")
  run$run_id <- "metaproteomics_resume_test"
  object <- ProtVis:::.mp_append_run(object, run)

  restored <- ProtVis:::.mp_data_from_dataset(object)
  testthat::expect_true("Genus" %in% names(restored$taxonomy))
  testthat::expect_true("Pathway" %in% names(restored[["function"]]))
  testthat::expect_equal(nrow(restored$peptide), nrow(dat$peptide))
})
