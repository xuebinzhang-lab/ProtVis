test_that("STRINGdb CSS uses parse-safe quoting", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "stringdb_ppi.R")),
    collapse = "\n"
  )

  expect_true(grepl("content: '▸';", source_text, fixed = TRUE))
  expect_false(grepl('content: "▸";', source_text, fixed = TRUE))
})
