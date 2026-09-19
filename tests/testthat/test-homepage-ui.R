test_that("homepage UI builds without non-exported Shiny tag helpers", {
  homepage <- ProtVis:::protvis_homepage()
  expect_s3_class(homepage, "shiny.tag")
  html <- as.character(homepage)
  expect_match(html, "Project dataset", fixed = TRUE)
  expect_match(html, "Tabular proteomics results", fixed = TRUE)
  expect_match(html, "MaxQuant Output Preparation", fixed = TRUE)
  expect_false(grepl("Other tabular sources", html, fixed = TRUE))
  expect_false(grepl("S4", html, fixed = TRUE))
  expect_match(html, "<small>", fixed = TRUE)
})

test_that("application UI does not render the legacy site footer", {
  application <- app_ui(NULL)
  html <- as.character(application)
  expect_false(grepl("protvis-site-footer", html, fixed = TRUE))
  expect_false(grepl("Fei Liang | Henan University", html, fixed = TRUE))
})
