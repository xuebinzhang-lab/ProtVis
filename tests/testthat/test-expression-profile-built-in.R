test_that("the Kmeans built-in example is bundled and numeric", {
  path <- ProtVis:::.protvis_kmeans_builtin_path()
  expect_true(file.exists(path))

  data <- ProtVis:::.protvis_read_expression_profile(path)
  expect_equal(nrow(data), 582L)
  expect_equal(ncol(data), 8L)
  expect_equal(colnames(data), c("TA", "TB", "TC", "TD", "A", "B", "C", "D"))
  expect_true(all(vapply(data, is.numeric, logical(1))))
  expect_true(all(nzchar(rownames(data))))
})

test_that("Kmeans expression input rejects non-numeric sample columns", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("id,A,B", "m1,1,bad", "m2,2,3"), path)
  expect_error(
    ProtVis:::.protvis_read_expression_profile(path),
    "sample columns must be numeric"
  )
})

test_that("Kmeans UI exposes explicit built-in load and download controls", {
  html <- htmltools::renderTags(ProtVis::Expression_profile_ui("profile"))$html
  expect_match(html, "profile-load_builtin", fixed = TRUE)
  expect_match(html, "profile-download_builtin", fixed = TRUE)
  expect_match(html, "Upload expression matrix", fixed = TRUE)
})
