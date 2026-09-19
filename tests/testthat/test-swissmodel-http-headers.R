test_that("SWISS-MODEL raw downloads do not force application/octet-stream", {
  headers <- ProtVis:::.protvis_swiss_request_headers(
    token = "example-token",
    raw = TRUE
  )

  expect_false("Accept" %in% names(headers))
  expect_equal(headers$Authorization, "Token example-token")
})

test_that("SWISS-MODEL JSON calls still request JSON", {
  headers <- ProtVis:::.protvis_swiss_request_headers(
    token = "example-token",
    raw = FALSE
  )

  expect_equal(headers$Accept, "application/json")
  expect_equal(headers$Authorization, "Token example-token")
})
