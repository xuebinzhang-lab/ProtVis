test_that("Protein Workbench parses localization context", {
  entry <- list(
    primaryAccession = "PTEST1",
    organism = list(
      scientificName = "Zea mays",
      taxonId = 4577,
      lineage = c("Eukaryota", "Viridiplantae", "Streptophyta", "Poaceae")
    ),
    comments = list(
      list(
        commentType = "SUBCELLULAR LOCATION",
        subcellularLocations = list(
          list(
            location = list(
              value = "Nucleus",
              evidences = list(list(evidenceCode = "ECO:0000269"))
            ),
            topology = list(value = "Peripheral membrane protein")
          )
        )
      )
    ),
    features = list(
      list(
        type = "Signal peptide",
        description = "Signal peptide",
        location = list(start = list(value = 1), end = list(value = 24))
      ),
      list(
        type = "Domain",
        description = "Unrelated domain",
        location = list(start = list(value = 30), end = list(value = 90))
      ),
      list(
        type = "Transmembrane",
        description = "Helical",
        location = list(start = list(value = 110), end = list(value = 132))
      )
    )
  )

  loc <- ProtVis:::.protvis_pw_localization_table(entry)
  expect_equal(nrow(loc), 1)
  expect_equal(loc$location, "Nucleus")
  expect_match(loc$evidence, "ECO:0000269", fixed = TRUE)

  features <- ProtVis:::.protvis_pw_localization_features(entry)
  expect_equal(nrow(features), 2)
  expect_true(all(features$type %in% c("Signal peptide", "Transmembrane")))
  expect_true(ProtVis:::.protvis_pw_is_plant(entry))
})

test_that("Protein Workbench filters interaction database references", {
  entry <- list(
    uniProtKBCrossReferences = list(
      list(database = "STRING", id = "4577.X", properties = list()),
      list(database = "IntAct", id = "EBI-1", properties = list()),
      list(database = "PDB", id = "1ABC", properties = list()),
      list(database = "ComplexPortal", id = "CPX-1", properties = list())
    )
  )

  refs <- ProtVis:::.protvis_pw_interaction_xrefs(entry)
  expect_equal(sort(refs$database), sort(c("STRING", "IntAct", "ComplexPortal")))
})

test_that("Protein Workbench builds a single-protein interaction plot", {
  partners <- data.frame(
    query = c("QUERY", "QUERY", "QUERY"),
    partner = c("A", "B", "C"),
    score = c(0.9, 0.75, 0.6),
    stringsAsFactors = FALSE
  )
  plot <- ProtVis:::.protvis_pw_interaction_plot(partners)
  expect_s3_class(plot, "ggplot")
})

test_that("Protein Workbench UI keeps existing content and adds context tabs", {
  html <- as.character(ProtVis::protein_workbench_ui("pw_context_test"))
  expect_match(html, "Protein Workbench", fixed = TRUE)
  expect_match(html, "Overview", fixed = TRUE)
  expect_match(html, "Sequence", fixed = TRUE)
  expect_match(html, "PTM &amp; sites")
  expect_match(html, "Domains", fixed = TRUE)
  expect_match(html, "Structure", fixed = TRUE)
  expect_match(html, "Localization", fixed = TRUE)
  expect_match(html, "Interaction", fixed = TRUE)
  expect_match(html, "Plant-mPLoc prediction", fixed = TRUE)
  expect_match(html, "STRING interaction network", fixed = TRUE)
})
