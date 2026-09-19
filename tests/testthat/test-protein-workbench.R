test_that("Protein Workbench sequence helpers return stable summaries", {
  seq <- "VLSPADKTNVKAAWAKVGNHAADFGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHGKKVADALTNAVAHVDDMPNALSALSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPAEFTPAVHASLDKFLASVSTVLTSKYR"

  stats <- ProtVis:::.protvis_pw_sequence_stats(seq)
  expect_equal(stats$value[stats$metric == "Length"], "141 aa")
  expect_true(grepl("Da$", stats$value[stats$metric == "Molecular weight"]))

  composition <- ProtVis:::.protvis_pw_composition(seq)
  expect_equal(sum(composition$Count), nchar(seq))
  expect_equal(round(sum(composition$Fraction), 8), 1)

  hydropathy <- ProtVis:::.protvis_pw_hydropathy(seq, 9)
  expect_equal(nrow(hydropathy), nchar(seq))
})

test_that("Protein Workbench parses UniProt-style feature records", {
  entry <- list(
    primaryAccession = "PTEST1",
    uniProtkbId = "PTEST1_TEST",
    proteinDescription = list(
      recommendedName = list(fullName = list(value = "Test protein"))
    ),
    genes = list(list(geneName = list(value = "TEST"))),
    organism = list(scientificName = "Test species", taxonId = 1),
    sequence = list(value = "ACDEFGHIKLMNPQRSTVWY", length = 20, molWeight = 2200),
    features = list(
      list(
        type = "Modified residue",
        description = "Phosphoserine",
        location = list(start = list(value = 5), end = list(value = 5))
      ),
      list(
        type = "Domain",
        description = "Example domain",
        location = list(start = list(value = 2), end = list(value = 18))
      )
    ),
    uniProtKBCrossReferences = list(
      list(database = "PDB", id = "1ABC", properties = list()),
      list(database = "InterPro", id = "IPR000001", properties = list())
    )
  )

  features <- ProtVis:::.protvis_pw_features_table(entry)
  expect_equal(nrow(features), 2)
  expect_equal(features$start, c(5L, 2L))

  ptm <- ProtVis:::.protvis_pw_ptm_table(entry)
  expect_equal(nrow(ptm), 1)
  expect_match(ptm$description, "Phosphoserine")

  xrefs <- ProtVis:::.protvis_pw_xrefs_table(entry)
  expect_true(all(c("PDB", "InterPro") %in% xrefs$database))
})

test_that("Protein Workbench is exposed as a separate Toolkits module", {
  html <- as.character(ProtVis::protein_workbench_ui("pw_test"))
  expect_match(html, "Protein Workbench", fixed = TRUE)
  expect_match(html, "PTM &amp; sites")
  expect_match(html, "AlphaFold structure", fixed = TRUE)
  expect_match(html, "Cross-references", fixed = TRUE)

  app_html <- as.character(ProtVis::app_ui(NULL))
  expect_match(app_html, "Protein Workbench", fixed = TRUE)
  expect_match(app_html, "Protein Extract", fixed = TRUE)
  expect_match(app_html, "Plant-mPLoc", fixed = TRUE)
  expect_match(app_html, "swissmodel", fixed = TRUE)
})
