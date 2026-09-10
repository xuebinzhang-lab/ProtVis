test_that("enrichment loading does not require a legacy Step7 RDA", {
  ui_text <- paste(readLines(testthat::test_path("..", "..", "R", "enrichment_analysis.R")),
                   collapse = "\n")
  expect_true(grepl("shared_state\\$dep_results", ui_text))
  expect_true(grepl("ProtVis_dataset", ui_text, fixed = TRUE))
  expect_true(grepl("stored\\$comparisons", ui_text))
  expect_true(grepl("No DEP results are available", ui_text, fixed = TRUE))
  server_text <- paste(readLines(testthat::test_path("..", "..", "R", "app_server.R")),
                       collapse = "\n")
  expect_true(grepl("dep_results = list()", server_text, fixed = TRUE))
  expect_false(grepl("shiny.maxRequestSize = 100", ui_text, fixed = TRUE))
  run_text <- paste(readLines(testthat::test_path("..", "..", "R", "run_app.R")),
                    collapse = "\n")
  expect_true(grepl("shiny.maxRequestSize = 2 * 1024^3", run_text, fixed = TRUE))
})
