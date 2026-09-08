test_that("ProtVis_dataset has the standard schema", {
  expression <- data.frame(
    ID = c("P1", "P2", "P3"),
    S1 = c(1, NA, 3),
    S2 = c(2, 4, 6),
    check.names = FALSE
  )
  sample_info <- data.frame(
    sample_id = c("S1", "S2"),
    group = c("Control", "Treatment"),
    batch = c("B1", "B1"),
    stringsAsFactors = FALSE
  )
  object <- create_protvis_dataset(expression, sample_info = sample_info)
  expect_s3_class(object, "ProtVis_dataset")
  expect_true(validate_protvis_dataset(object))
  expect_equal(dim(object$expression_data), c(3, 2))
  expect_true(all(c("sample_id", "group") %in% names(object$sample_info)))
  expect_true(all(c("protein_id", "accession", "gene", "description") %in%
                    names(object$variable_info)))
  expect_true(all(c("analysis_results", "process_info", "metadata",
                    "other_files", "checkpoint_info") %in% names(object)))
})

test_that("all tabular source adapters produce a common object", {
  pd <- data.frame(id = c("P1", "P2"), s1 = c(1, 2), s2 = c(3, 4))
  names(pd) <- c("Master Protein Accessions", "Abundance S1", "Abundance S2")
  dia <- data.frame(id = c("P1", "P1", "P2", "P2"),
                    run = c("S1", "S2", "S1", "S2"),
                    value = c(1, 2, 3, 4))
  names(dia) <- c("Protein.Group", "Run", "Precursor.Normalised")
  sn <- data.frame(id = c("P1", "P2"), s1 = c(1, 2), s2 = c(3, 4))
  names(sn) <- c("PG.ProteinGroups", "PG.Quantity S1", "PG.Quantity S2")
  fp <- data.frame(id = c("P1", "P2"), s1 = c(1, 2), s2 = c(3, 4))
  names(fp) <- c("Protein", "Intensity S1", "Intensity S2")
  skyline <- data.frame(id = c("P1", "P1", "P2", "P2"),
                        replicate = c("S1", "S2", "S1", "S2"),
                        area = c(1, 2, 3, 4))
  names(skyline) <- c("ProteinName", "Replicate", "Area")
  openms <- data.frame(id = c("P1", "P2"), s1 = c(1, 2), s2 = c(3, 4))
  names(openms) <- c("ProteinName", "map_S1", "map_S2")
  user <- data.frame(ID = c("P1", "P2"), S1 = c(1, 2), S2 = c(3, 4))
  sources <- list(pd, dia, sn, fp, skyline, openms, user)
  source_names <- c(
    "Proteome Discoverer", "DIA-NN", "Spectronaut", "FragPipe",
    "Skyline", "OpenMS", "User-defined matrix"
  )
  for (i in seq_along(sources)) {
    object <- import_protvis(sources[[i]], source = source_names[[i]])
    expect_s3_class(object, "ProtVis_dataset")
    expect_equal(ncol(object$expression_data), 2)
    expect_true(validate_protvis_dataset(object))
  }
})

test_that("MaxQuant built-in workbook is complete and filtered", {
  path <- protvis_builtin_data_path()
  skip_if_not(file.exists(path))
  object <- load_protvis_builtin_data()
  expect_equal(nrow(object$expression_data), 12300)
  expect_equal(ncol(object$expression_data), 30)
  expect_equal(object$metadata$raw_rows, 12689)
  expect_equal(object$metadata$retained_rows, 12300)
  expect_equal(object$metadata$removed_rows, 389)
  expect_true(file.exists(path))
})

test_that("MaxQuant missing sentinel is kept as NA", {
  expression <- data.frame(
    ID = c("P1", "P2"),
    S1 = c(10, -8),
    S2 = c(-8, 20),
    check.names = FALSE
  )
  validated <- validate_protvis_data(expression, source = "MaxQuant")
  expect_true(is.na(validated$expression_matrix$S1[[2]]))
  expect_true(is.na(validated$expression_matrix$S2[[1]]))
  expect_equal(validated$expression_matrix$S1[[1]], 10)
})

test_that("bundled software fixtures import and enter the pipeline", {
  manifest <- protvis_builtin_datasets()
  expect_gte(nrow(manifest), 8)
  fixture_paths <- vapply(manifest$file, function(name) {
    system.file("extdata", name, package = "ProtVis")
  }, character(1))
  expect_true(all(nzchar(fixture_paths) & file.exists(fixture_paths)))
  for (source in unique(manifest$source)) {
    object <- load_protvis_builtin_data(source = source)
    expect_s3_class(object, "ProtVis_dataset")
    expect_true(validate_protvis_dataset(object))
    expect_gte(nrow(object$expression_data), 4)
    expect_gte(ncol(object$expression_data), 2)
    processed <- run_protvis_pipeline(
      object,
      stages = c("noise_correction", "transformation", "imputation",
                 "normalization", "dimensionality_reduction"),
      params = list(
        noise_correction = list(max_missing = 0.99),
        transformation = list(method = "log2"),
        imputation = list(method = "median"),
        normalization = list(method = "median")
      )
    )
    expect_identical(processed$process_info$last_status, "success")
    expect_true("dimensionality_reduction" %in%
                  names(processed$analysis_results))
  }
  mztab <- manifest[manifest$format == "mzTab", , drop = FALSE]
  mz_object <- load_protvis_builtin_data(
    source = mztab$source[[1L]], file = mztab$file[[1L]]
  )
  expect_equal(nrow(mz_object$expression_data), 4)
  expect_equal(ncol(mz_object$expression_data), 2)
  expect_true(validate_protvis_dataset(mz_object))
})

test_that("node failures are recorded without invalidating the object", {
  expression <- data.frame(
    ID = paste0("P", 1:10),
    S1 = 1:10, S2 = 2:11, S3 = 3:12,
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)
  failed <- run_protvis_step(
    object, "normalization", params = list(method = "not-a-method")
  )
  expect_s3_class(failed, "ProtVis_dataset")
  expect_true(nrow(protvis_error_log(failed)) >= 1)
  retried <- run_protvis_step(
    failed, "normalization", params = list(method = "none")
  )
  expect_identical(retried$process_info$last_status, "success")
})

test_that("each analysis returns a named dataset and auto-exports it", {
  object <- create_protvis_dataset(data.frame(
    ID = paste0("P", 1:6), S1 = 1:6, S2 = 2:7, S3 = 3:8,
    check.names = FALSE
  ))
  output <- tempfile("protvis_auto_output_")
  dir.create(output)
  result <- run_protvis_step(
    object, "transformation", params = list(method = "log2"),
    checkpoint_dir = output
  )
  expect_s3_class(result, "ProtVis_dataset")
  expect_match(protvis_dataset_name(result),
               "ProtVis_dataset__transformation__log2__v2")
  expect_identical(result$metadata$parent_object_name,
                   "ProtVis_dataset__creation__v1")
  expect_true(dir.exists(result$metadata$auto_export_directory))
  saved <- readRDS(file.path(result$metadata$auto_export_directory,
                             "ProtVis_dataset.rds"))
  expect_identical(protvis_dataset_name(saved), protvis_dataset_name(result))
})

test_that("checkpoint save, list, restore, and export are recoverable", {
  expression <- data.frame(
    ID = paste0("P", 1:8), S1 = 1:8, S2 = 2:9, S3 = 3:10,
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)
  directory <- tempfile("protvis_checkpoints_")
  dir.create(directory)
  path <- save_protvis_checkpoint(object, directory, stage = "creation")
  expect_true(file.exists(path))
  expect_true(nrow(list_protvis_checkpoints(directory)) >= 1)
  restored <- restore_protvis_checkpoint(directory)
  expect_s3_class(restored, "ProtVis_dataset")
  export_dir <- export_protvis_dataset(restored, tempfile("protvis_export_"))
  expect_true(file.exists(file.path(export_dir, "ProtVis_dataset.rds")))
  expect_true(file.exists(file.path(export_dir, "process_history.csv")))
})
