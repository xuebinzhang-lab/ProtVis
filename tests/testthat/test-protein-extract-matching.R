test_that("protein extraction supports normalized and conservative fuzzy IDs", {
  headers <- c(
    "Zm00001eb096110_P001 pep gene:Zm00001eb096110 transcript:Zm00001eb096110_T001",
    "Zm00001eb321680_P001 pep gene:Zm00001eb321680 transcript:Zm00001eb321680_T001"
  )

  normalized <- ProtVis:::.protvis_match_protein_ids(
    headers, "Zm00001eb096110_T001"
  )
  expect_equal(normalized$sequence_index, 1L)
  expect_equal(normalized$matches$Match_type, "Exact")

  fuzzy <- ProtVis:::.protvis_match_protein_ids(
    headers, "Zm00001eb096111"
  )
  expect_equal(fuzzy$sequence_index, 1L)
  expect_equal(fuzzy$matches$Match_type, "Fuzzy (1 character)")

  unmatched <- ProtVis:::.protvis_match_protein_ids(
    headers, "Zm00001eb314140"
  )
  expect_length(unmatched$sequence_index, 0L)
  expect_equal(unmatched$unmatched_ids, "Zm00001eb314140")
})
