test_that("interaction helper resolves from base namespace", {
  expect_identical(ProtVis:::interaction, base::interaction)
})
