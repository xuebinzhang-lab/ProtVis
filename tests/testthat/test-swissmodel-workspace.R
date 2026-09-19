test_that("SWISS-MODEL sequence parser supports heteromeric FASTA input", {
  fasta <- paste(
    ">chain_A",
    "ACDEFGHIK",
    ">chain_B",
    "LMNPQRSTV",
    sep = "\n"
  )

  parsed <- ProtVis:::.protvis_swiss_parse_sequences(fasta)

  expect_equal(parsed, c("ACDEFGHIK", "LMNPQRSTV"))
  expect_equal(
    ProtVis:::.protvis_swiss_target_payload(parsed),
    as.list(c("ACDEFGHIK", "LMNPQRSTV"))
  )
})

test_that("SWISS-MODEL model table normalizes model identifiers", {
  summary <- list(
    status = "COMPLETED",
    models = data.frame(
      model_id = c("01", "02"),
      gmqe = c(0.91, 0.84),
      stringsAsFactors = FALSE
    )
  )

  table <- ProtVis:::.protvis_swiss_models_table(summary)

  expect_equal(table$model_id, c("01", "02"))
  expect_equal(ProtVis:::.protvis_swiss_model_ids(summary), c("01", "02"))
})

test_that("SWISS-MODEL workspace exposes official modelling modes and project tools", {
  html <- as.character(ProtVis::swissmodel_ui("swissmodel_workspace_test"))

  expect_match(html, "Automated mode", fixed = TRUE)
  expect_match(html, "Alignment mode", fixed = TRUE)
  expect_match(html, "User template", fixed = TRUE)
  expect_match(html, "Load existing project", fixed = TRUE)
  expect_match(html, "Template &amp; alignment")
  expect_match(html, "Complex &amp; ligands")
  expect_match(html, "Project history", fixed = TRUE)
  expect_match(html, "Atlas search", fixed = TRUE)
  expect_match(html, "Selected model · mmCIF", fixed = TRUE)
})

test_that("SWISS-MODEL field filters surface quality and complex metadata", {
  details <- list(
    template = list(pdb_id = "1abc", identity = 51.2),
    qmean = list(global_score = 0.81),
    oligomeric_state = "homo-dimer",
    ligands = c("ZN")
  )

  quality <- ProtVis:::.protvis_swiss_quality_fields(list(), details)
  complex <- ProtVis:::.protvis_swiss_complex_fields(details)
  template <- ProtVis:::.protvis_swiss_template_fields(details)

  expect_true(any(grepl("qmean", quality$path, ignore.case = TRUE)))
  expect_true(any(grepl("oligomer|ligand", complex$path, ignore.case = TRUE)))
  expect_true(any(grepl("template", template$path, ignore.case = TRUE)))
})
