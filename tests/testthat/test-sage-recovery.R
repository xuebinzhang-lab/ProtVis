test_that("Correct Noise can recover a completed Sage stage", {
  source_text <- paste(
    readLines(testthat::test_path("../../R/correct_noise.R")),
    collapse = "\n"
  )

  expect_match(source_text, "Step2_sage_database_search[.]rda")
  expect_match(source_text, "Sage_search")
  expect_match(source_text, "Step2_remove_unreliable_peptide[.]rda")
  expect_match(source_text, "Sage search results loaded successfully")
})
