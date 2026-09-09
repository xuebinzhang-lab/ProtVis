test_that("installed package has a complete lazy-load database", {
  package_path <- system.file(package = "ProtVis")
  expect_true(nzchar(package_path))
  lazy_databases <- file.path(
    package_path, "R", c("ProtVis.rdb", "ProtVis.rdx")
  )
  expect_true(all(file.exists(lazy_databases)))
  expect_true(is.function(getExportedValue("ProtVis", "run_ProtVis")))
  expect_true(is.call(body(getExportedValue("ProtVis", "run_ProtVis"))))
})

test_that("installation metadata requires staged installation", {
  description_path <- system.file("DESCRIPTION", package = "ProtVis")
  description <- read.dcf(description_path)
  expect_identical(unname(description[1L, "LazyData"]), "false")
  expect_identical(unname(description[1L, "StagedInstall"]), "true")
})
