test_that("Protein Workbench builds STRING identifier candidates", {
  entry <- list(
    primaryAccession = "A0ATEST01",
    uniProtkbId = "A0ATEST01_MAIZE",
    genes = list(list(
      geneName = list(value = "ZmTEST1"),
      synonyms = list(list(value = "TEST_ALIAS"))
    ))
  )
  candidates <- ProtVis:::.protvis_pw_string_candidates(entry)
  expect_true(all(c("A0ATEST01", "A0ATEST01_MAIZE", "ZmTEST1", "TEST_ALIAS") %in% candidates))
})

test_that("Protein Workbench prefers an existing UniProt STRING cross-reference", {
  entry <- list(
    primaryAccession = "A0ATEST01",
    uniProtkbId = "A0ATEST01_MAIZE",
    genes = list(list(geneName = list(value = "ZmTEST1"))),
    organism = list(scientificName = "Zea mays", taxonId = 4577),
    uniProtKBCrossReferences = list(
      list(database = "STRING", id = "4577.A0ATEST01", properties = list())
    )
  )
  resolved <- ProtVis:::.protvis_pw_string_resolve(entry)
  expect_equal(nrow(resolved$mapping), 1)
  expect_equal(resolved$mapping$string_id, "4577.A0ATEST01")
  expect_match(resolved$status, "cross-reference")
})

test_that("Protein Workbench Interaction UI exposes explicit STRING diagnostics", {
  html <- as.character(ProtVis::protein_workbench_ui("pw_string_diag"))
  expect_match(html, "STRING mapping diagnostics", fixed = TRUE)
  expect_match(html, "Minimum STRING confidence (0-1000)", fixed = TRUE)
  expect_match(html, "Interaction database references", fixed = TRUE)
  expect_match(html, "STRING interaction network", fixed = TRUE)
})
