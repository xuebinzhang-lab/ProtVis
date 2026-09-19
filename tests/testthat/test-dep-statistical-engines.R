test_that("limma engine returns standardized differential-proteomics columns", {
  set.seed(11)
  mat <- matrix(rnorm(300, mean = 10, sd = 1), nrow = 50)
  rownames(mat) <- paste0("P", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  mat[1:5, 1:3] <- mat[1:5, 1:3] + 2

  result <- ProtVis:::.protvis_dep_run_limma(
    mat,
    group1_samples = c("S1", "S2", "S3"),
    group2_samples = c("S4", "S5", "S6"),
    group1 = "A", group2 = "B"
  )
  expect_equal(nrow(result), nrow(mat))
  expect_true(all(c("ID", "logFC", "P.Value", "adj.P.Val", "method") %in% names(result)))
  expect_true(all(result$method == "limma"))
})

test_that("DEqMS count metadata are detected and aligned", {
  table <- data.frame(
    protein_id = c("P2", "P1", "P3"),
    Peptides = c(4, 8, 2),
    `MS/MS count` = c(10, 20, 5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_true(all(c("Peptides", "MS/MS count") %in% ProtVis:::.protvis_dep_count_columns(table)))
  counts <- ProtVis:::.protvis_dep_count_table(table, c("P1", "P2", "P3"))
  expect_equal(counts$ID, c("P1", "P2", "P3"))
  expect_equal(counts$count, c(8, 4, 2))
})

test_that("import metadata retains peptide and PSM evidence columns", {
  input <- data.frame(
    `Protein IDs` = c("P1", "P2"),
    `Gene names` = c("G1", "G2"),
    Peptides = c(5, 3),
    `MS/MS count` = c(12, 7),
    `LFQ intensity S1` = c(100, 200),
    `LFQ intensity S2` = c(110, 190),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  info <- ProtVis:::.protvis_variable_info_from_table(input, "Protein IDs", c("P1", "P2"))
  expect_true("Peptides" %in% names(info))
  expect_true("MS/MS count" %in% names(info))
  expect_equal(info$Peptides, c(5, 3))
})

test_that("MSstats input validation requires native feature-level columns", {
  incomplete <- data.frame(ProteinName = "P1", Condition = "A", Intensity = 100)
  expect_error(
    ProtVis:::.protvis_dep_msstats_canonical(incomplete),
    "MSstats input is missing required column"
  )

  complete <- data.frame(
    ProteinName = "P1",
    PeptideSequence = "PEPTIDE",
    PrecursorCharge = 2,
    FragmentIon = "NA",
    ProductCharge = 0,
    IsotopeLabelType = "L",
    Condition = "A",
    BioReplicate = "1",
    Run = "run1",
    Intensity = 1000,
    stringsAsFactors = FALSE
  )
  canonical <- ProtVis:::.protvis_dep_msstats_canonical(complete)
  expect_true(all(ProtVis:::.protvis_dep_msstats_required %in% names(canonical)))
  expect_type(canonical$Intensity, "double")
})

test_that("proDA engine accepts missing protein intensities", {
  skip_if_not_installed("proDA")
  set.seed(12)
  mat <- matrix(rnorm(360, mean = 12, sd = 1), nrow = 60)
  rownames(mat) <- paste0("P", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  mat[sample(length(mat), 30)] <- NA_real_
  result <- ProtVis:::.protvis_dep_run_proda(
    mat,
    group1_samples = c("S1", "S2", "S3"),
    group2_samples = c("S4", "S5", "S6"),
    group1 = "A", group2 = "B"
  )
  expect_true(all(c("ID", "logFC", "P.Value", "adj.P.Val", "method") %in% names(result)))
  expect_true(all(result$method == "proDA"))
})

test_that("DEP UI keeps legacy workflow and exposes parallel engines", {
  html <- as.character(ProtVis::DEP_analysis_ui("dep_engines_test"))
  expect_match(html, "DEP workflow", fixed = TRUE)
  expect_match(html, "Statistical engines", fixed = TRUE)
  expect_match(html, "DEqMS", fixed = TRUE)
  expect_match(html, "proDA", fixed = TRUE)
  expect_match(html, "MSstats", fixed = TRUE)
  expect_match(html, "RUN DEP", fixed = TRUE)
})
