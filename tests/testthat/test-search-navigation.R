test_that("Search navigation is FASTA-gated", {
  ui_text <- paste(readLines(testthat::test_path("../../R/app_ui.R")),
                   collapse = "\n")
  server_text <- paste(readLines(testthat::test_path("../../R/app_server.R")),
                       collapse = "\n")

  expect_match(ui_text, '"Search"')
  expect_match(ui_text, 'value = "sage_search"')
  expect_match(ui_text, "protvis-sage-nav")
  expect_match(ui_text, "protvis-data-input-nav")
  expect_match(server_text, "file.exists")
  expect_match(server_text, "sendCustomMessage")
  expect_match(server_text, 'data_source = "Raw"')
})
