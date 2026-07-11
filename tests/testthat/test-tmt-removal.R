test_that("standalone TMT module stays removed", {
  repo_root <- testthat::test_path("..", "..")

  expect_false(file.exists(file.path(repo_root, "R", "TMT.R")))
  expect_false(file.exists(file.path(repo_root, "man", "TMT_ui.Rd")))
  expect_false(file.exists(file.path(repo_root, "man", "TMT_server.Rd")))

  namespace_text <- readLines(file.path(repo_root, "NAMESPACE"), warn = FALSE)
  expect_false(any(grepl("export\\(TMT_ui\\)", namespace_text)))
  expect_false(any(grepl("export\\(TMT_server\\)", namespace_text)))

  app_ui_text <- readLines(file.path(repo_root, "R", "app_ui.R"), warn = FALSE)
  app_server_text <- readLines(file.path(repo_root, "R", "app_server.R"), warn = FALSE)
  expect_false(any(grepl("TMT_ui\\(", app_ui_text)))
  expect_false(any(grepl("TMT_server\\(", app_server_text)))
})
