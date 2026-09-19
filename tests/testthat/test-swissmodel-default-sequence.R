test_that("Swiss-Model starts with the bundled demo sequence", {
  sequence <- ProtVis:::.protvis_swissmodel_demo_sequence
  expect_equal(nchar(sequence), 141L)
  expect_match(sequence, "^VLSPADKTNVKAAWAKVGNHA")
  expect_match(sequence, "LDKFLASVSTVLTSKYR$")

  html <- as.character(ProtVis::swissmodel_ui("swissmodel_test"))
  expect_match(html, sequence, fixed = TRUE)
})
