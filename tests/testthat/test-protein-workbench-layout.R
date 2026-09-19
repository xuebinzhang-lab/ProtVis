test_that("Protein Workbench keeps the base workspace at usable height", {
  html <- as.character(ProtVis::protein_workbench_ui("pw_layout_test"))

  expect_match(html, "pw-base-shell", fixed = TRUE)
  expect_match(html, "height: calc(100vh - 82px)", fixed = TRUE)
  expect_match(html, "min-height: 760px", fixed = TRUE)
  expect_match(html, "Localization and interaction context", fixed = TRUE)

  # Existing primary Workbench content must remain present.
  expect_match(html, "Protein-centric analysis workspace", fixed = TRUE)
  expect_match(html, "Overview", fixed = TRUE)
  expect_match(html, "Sequence", fixed = TRUE)
  expect_match(html, "PTM &amp; sites")
  expect_match(html, "Structure", fixed = TRUE)

  # Context views are still available after a protein is resolved.
  expect_match(html, "Localization", fixed = TRUE)
  expect_match(html, "Interaction", fixed = TRUE)
  expect_match(html, "pw-context-empty", fixed = TRUE)
})
