test_that("the Kmeans built-in example is bundled and numeric", {
  candidates <- c(
    system.file("extdata", "kmeans.csv", package = "ProtVis"),
    testthat::test_path("..", "..", "inst", "extdata", "kmeans.csv")
  )
  path <- candidates[nzchar(candidates) & file.exists(candidates)][[1L]]
  expect_true(file.exists(path))

  data <- utils::read.csv(path, row.names = 1, check.names = FALSE,
                          stringsAsFactors = FALSE)
  expect_gte(nrow(data), 100L)
  expect_gte(ncol(data), 4L)
  expect_true(all(vapply(data, is.numeric, logical(1))))
  expect_true(all(nzchar(rownames(data))))
})
