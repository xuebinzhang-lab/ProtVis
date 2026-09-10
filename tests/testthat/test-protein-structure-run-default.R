test_that("RUN falls back to the bundled PDB when no file is selected", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_true(grepl("RUN is also a valid entry point", source_text, fixed = TRUE))
  expect_true(grepl("rv$active_file_path <- demo_pdb_path", source_text, fixed = TRUE))
})
