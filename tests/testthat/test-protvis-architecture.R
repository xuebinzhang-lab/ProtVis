test_that("ProtVis schema v4 standardizes assays, provenance, and append-only analysis", {
  object <- create_protvis_dataset(
    data.frame(ID = c("P1", "P2"), S1 = c(1, 2), S2 = c(2, 4),
               check.names = FALSE)
  )
  object <- protvis_standardize_dataset(object)
  expect_identical(object$version, "4.0.0")
  expect_identical(object$metadata$schema_version, "4.0.0")
  expect_identical(protvis_schema()$primary_assay, "protein")
  expect_identical(protvis_schema()$analysis_contract$mode, "append_only")
  expect_true(isTRUE(protvis_schema()$analysis_contract$preserve_core_matrix))
  expect_identical(protvis_assay(object, "protein"), object$expression_data)
  provenance <- protvis_provenance(object)
  expect_true(is.list(provenance$environment))
  expect_true("ProtVis" %in% names(provenance$software))
})

test_that("PSM and peptide assays can be registered without replacing protein data", {
  object <- create_protvis_dataset(
    data.frame(ID = c("P1", "P2"), S1 = c(1, 2), S2 = c(2, 4),
               check.names = FALSE)
  )
  psm <- data.frame(peptide = c("AAA", "BBB"), proteins = c("P1", "P2"))
  object <- register_protvis_assay(object, "psm", psm, source = "test")
  expect_identical(protvis_assay(object, "psm"), psm)
  expect_equal(nrow(protvis_assay(object, "protein")), 2)
})

test_that("workflow invalidation is explicit and cleared as nodes rerun", {
  object <- create_protvis_dataset(
    data.frame(
      ID = paste0("P", 1:6),
      S1 = 2:7, S2 = 3:8, S3 = 4:9,
      check.names = FALSE
    )
  )
  invalid <- protvis_invalidate_downstream(object, "transformation")
  state <- protvis_workflow_status(invalid)
  expect_true(all(
    state$status[state$stage %in% c(
      "transformation", "imputation", "normalization",
      "dimensionality_reduction", "differential_analysis",
      "enrichment", "network"
    )] == "invalidated"
  ))
  completed <- run_protvis_step(
    invalid, "transformation", params = list(method = "none")
  )
  state2 <- protvis_workflow_status(completed)
  expect_identical(
    state2$status[state2$stage == "transformation"],
    "complete"
  )
  expect_identical(
    state2$status[state2$stage == "normalization"],
    "invalidated"
  )
})

test_that("project QC and Sage QC return stable tables", {
  object <- create_protvis_dataset(
    data.frame(
      ID = c("P1", "P2", "P3"),
      S1 = c(1, NA, 3), S2 = c(2, 4, 6),
      check.names = FALSE
    )
  )
  qc <- protvis_qc_summary(object)
  expect_equal(qc$proteins, 3)
  expect_equal(qc$samples, 2)
  expect_equal(nrow(protvis_sample_qc(object)), 2)

  psms <- data.frame(
    filename = c("A.mzML", "A.mzML", "B.mzML"),
    peptide = c("AAA", "BBB", "AAA"),
    proteins = c("P1", "P2", "P1"),
    charge = c(2, 3, 2),
    spectrum_q = c(0.001, 0.02, 0.005),
    precursor_ppm = c(1.2, -2.4, 0.4),
    stringsAsFactors = FALSE
  )
  sqc <- protvis_sage_qc(psms)
  expect_equal(sqc$summary$Value[sqc$summary$Metric == "PSMs"], 3)
  expect_equal(nrow(sqc$per_run), 2)
  expect_equal(sum(sqc$charge$psms), 3)
})

test_that("headless and dashboard entry points are exported", {
  expect_true(is.function(ProtVis::run_protvis_cli))
  expect_true(is.function(ProtVis::protvis_dashboard_ui))
  expect_true(is.function(ProtVis::psm_explorer_ui))
})
