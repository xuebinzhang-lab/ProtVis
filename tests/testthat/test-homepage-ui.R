test_that("homepage UI builds without non-exported Shiny tag helpers", {
  homepage <- ProtVis:::protvis_homepage()
  expect_s3_class(homepage, "shiny.tag")
  html <- as.character(homepage)
  expect_match(html, "ProtVis_dataset", fixed = TRUE)
  expect_match(html, "<small>", fixed = TRUE)
})
