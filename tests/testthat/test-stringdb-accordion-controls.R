test_that("STRINGdb controls are grouped into collapsible scientific panels", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "stringdb_ppi.R")),
    collapse = "\n"
  )

  expect_true(grepl("pv-sidebar-details", source_text, fixed = TRUE))
  expect_true(grepl("Data Input", source_text, fixed = TRUE))
  expect_true(grepl("STRING Settings", source_text, fixed = TRUE))
  expect_true(grepl("Network Options", source_text, fixed = TRUE))
  expect_true(grepl("Color & Export", source_text, fixed = TRUE))
})
