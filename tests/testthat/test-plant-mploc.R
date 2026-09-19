test_that("Plant-mPLoc sequence validation and FASTA formatting work", {
  expect_equal(.plant_mploc_clean_sequence("ACD EFG\nhik"), "ACDEFGHIK")
  expect_error(.plant_mploc_clean_sequence("ACDX"), "standard amino acids")
  fasta <- .plant_mploc_fasta(paste(rep("A", 145), collapse = ""), "ZmProtein")
  lines <- strsplit(fasta, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[[1]], ">ZmProtein")
  expect_equal(nchar(lines[-1]), c(70L, 70L, 5L))
})

test_that("Plant-mPLoc bundles the requested maize demo sequence", {
  expect_equal(nchar(.plant_mploc_demo_sequence), 442L)
  expect_match(.plant_mploc_demo_sequence, "^MDKAHLGGGLLALDASPRPL")
  expect_match(.plant_mploc_demo_sequence, "GFVQVLRPAHVIVIDIG$")
  expect_silent(.plant_mploc_clean_sequence(.plant_mploc_demo_sequence))
  demo_fasta <- .plant_mploc_fasta(.plant_mploc_demo_sequence, "ZmProtein_demo")
  demo_lines <- strsplit(demo_fasta, "\n", fixed = TRUE)[[1]]
  expect_equal(demo_lines[[1]], ">ZmProtein_demo")
  expect_true(all(nchar(demo_lines[-1]) <= 70L))
})

test_that("Plant-mPLoc uses the working HTTP endpoint and can fall back from HTTPS", {
  expect_match(.plant_mploc_url, "^http://", perl = TRUE)
  expect_equal(
    .plant_mploc_candidate_urls(
      "https://www.csbio.sjtu.edu.cn/bioinf/plant-multi/"
    ),
    c(
      "https://www.csbio.sjtu.edu.cn/bioinf/plant-multi/",
      "http://www.csbio.sjtu.edu.cn/bioinf/plant-multi/"
    )
  )
  expect_equal(
    .plant_mploc_candidate_urls(.plant_mploc_url),
    .plant_mploc_url
  )
})

test_that("Plant-mPLoc parser ignores class descriptions", {
  explanatory <- paste(
    "This predictor identifies proteins among the following location sites:",
    "cell membrane, cell wall, chloroplast, cytoplasm, nucleus and vacuole."
  )
  expect_true(all(is.na(.plant_mploc_extract_prediction(explanatory))))
})

test_that("Plant-mPLoc parser supports single and multiple locations", {
  expect_equal(
    .plant_mploc_extract_prediction("Predicted result: Cytoplasm. Nucleus"),
    c("Cytoplasm", "Nucleus")
  )
  expect_equal(
    .plant_mploc_extract_prediction("Chloroplast"),
    "Chloroplast"
  )
})

test_that("Plant-mPLoc prefers the prediction submit control", {
  form <- list(fields = list(
    reset = list(type = "submit", value = "Clear"),
    action = list(type = "submit", value = "Submit prediction")
  ))
  expect_equal(.plant_mploc_submit_name(form), "action")
})

test_that("Plant-mPLoc is wired into the application", {
  ui <- paste(readLines(testthat::test_path("..", "..", "R", "app_ui.R")), collapse = "\n")
  server <- paste(readLines(testthat::test_path("..", "..", "R", "app_server.R")), collapse = "\n")
  expect_match(ui, 'plant_mploc_ui\\("plant_mploc"\\)')
  expect_match(server, 'plant_mploc_server\\("plant_mploc"\\)')
  module <- paste(readLines(testthat::test_path("..", "..", "R", "plant_mploc.R")), collapse = "\n")
  expect_match(module, 'ns\\("load_demo"\\)')
  expect_match(module, "updateTextAreaInput")
  expect_match(module, "\\.plant_mploc_demo_sequence")
})
