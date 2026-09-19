testthat::test_that("schema v4 initializes project-wide run storage", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  info <- data.frame(
    sample_id = c("S1", "S2"),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  )

  object <- create_protvis_dataset(expression, sample_info = info)

  testthat::expect_identical(protvis_schema_version(), "4.0.0")
  testthat::expect_identical(object$version, "4.0.0")
  testthat::expect_true(is.data.frame(object$result_registry))
  testthat::expect_true(is.list(object$workflow))
  testthat::expect_true(is.list(object$artifacts))
  testthat::expect_true(is.list(object$run_store$runs))
  testthat::expect_true(nrow(object$result_registry) >= 1L)
  testthat::expect_true(nzchar(object$workflow$active_matrix_run_id))
})

testthat::test_that("repeated ordinary analyses append instead of overwrite", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)

  object <- record_protvis_output(
    object,
    module = "custom_analysis",
    method = "first",
    tables = list(result = data.frame(ID = "P1", score = 1)),
    plot_data = list(volcano = data.frame(x = 1, y = 2)),
    parameters = list(cutoff = 0.05)
  )
  object <- record_protvis_output(
    object,
    module = "custom_analysis",
    method = "second",
    tables = list(result = data.frame(ID = "P2", score = 2)),
    plot_data = list(volcano = data.frame(x = 2, y = 4)),
    parameters = list(cutoff = 0.01)
  )

  history <- protvis_result_history(object, "custom_analysis")
  testthat::expect_length(history, 2L)
  testthat::expect_identical(history[[1L]]$method, "first")
  testthat::expect_identical(history[[2L]]$method, "second")
  testthat::expect_true(all(vapply(
    history, function(x) isTRUE(x$sealed), logical(1)
  )))
  testthat::expect_true(is.data.frame(history[[2L]]$results$tables$result))
  testthat::expect_true(is.data.frame(history[[2L]]$results$plot_data$volcano))
  testthat::expect_equal(history[[2L]]$parameters$cutoff, 0.01)

  registry <- protvis_result_registry(object, module = "custom_analysis")
  testthat::expect_equal(nrow(registry), 2L)
  testthat::expect_false(anyDuplicated(registry$run_id))
})

testthat::test_that("core matrices are complete snapshots and can be reactivated", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)

  first_matrix <- expression + 10
  second_matrix <- expression + 20

  object <- add_protvis_run(
    object,
    module = "normalization",
    method = "median",
    expression_data = first_matrix,
    parameters = list(scale = "log2")
  )
  first <- protvis_latest_result(object, "normalization")

  object <- add_protvis_run(
    object,
    module = "normalization",
    method = "vsn",
    expression_data = second_matrix,
    parameters = list(scale = "log2")
  )
  second <- protvis_latest_result(object, "normalization")

  testthat::expect_false(identical(first$run_id, second$run_id))
  testthat::expect_equal(
    as.matrix(first$state$expression_data),
    as.matrix(first_matrix),
    ignore_attr = TRUE
  )
  testthat::expect_equal(
    as.matrix(second$state$expression_data),
    as.matrix(second_matrix),
    ignore_attr = TRUE
  )
  testthat::expect_equal(
    as.matrix(object$expression_data),
    as.matrix(second_matrix),
    ignore_attr = TRUE
  )

  object <- protvis_activate_result(object, first$run_id)
  testthat::expect_equal(
    as.matrix(object$expression_data),
    as.matrix(first_matrix),
    ignore_attr = TRUE
  )
  testthat::expect_identical(
    object$workflow$active_matrix_run_id,
    first$run_id
  )
  testthat::expect_length(
    protvis_result_history(object, "normalization"),
    2L
  )
})

testthat::test_that("legacy latest-result assignments are captured as runs", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)

  object$analysis_results$legacy_demo <- list(
    status = "success",
    method = "demo",
    result_table = data.frame(ID = c("P1", "P2"), score = c(3, 4))
  )
  object <- ProtVis:::.protvis_append_process(
    object,
    "legacy_demo",
    status = "success",
    parameters = list(method = "demo")
  )

  latest <- protvis_latest_result(object, "legacy_demo")
  testthat::expect_identical(latest$method, "demo")
  testthat::expect_true(
    is.data.frame(latest$results$tables$result_table)
  )
  testthat::expect_true(isTRUE(latest$sealed))
  testthat::expect_true(is.list(object$analysis_results$legacy_demo))
})

testthat::test_that("module-local append-only stores mirror into global registry", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)

  local_run <- list(
    method = "local_engine",
    status = "success",
    parameters = list(alpha = 0.5),
    tables = list(
      summary = data.frame(metric = "n", value = 3)
    )
  )
  object <- ProtVis:::.protvis_append_analysis_run(
    object,
    module = "local_module",
    run = local_run,
    run_id = "local_run_1"
  )

  testthat::expect_identical(
    object$analysis_results$local_module$latest_run_id,
    "local_run_1"
  )
  mirrored <- protvis_latest_result(object, "local_module")
  testthat::expect_identical(mirrored$method, "local_engine")
  testthat::expect_true(
    is.data.frame(mirrored$results$tables$summary)
  )
  testthat::expect_equal(mirrored$parameters$alpha, 0.5)
})

testthat::test_that("workflow graph records dependencies on active matrix", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  object <- create_protvis_dataset(expression)
  root <- object$workflow$active_matrix_run_id

  object <- record_protvis_output(
    object,
    module = "pca",
    method = "prcomp",
    tables = list(
      scores = data.frame(sample_id = c("S1", "S2"), PC1 = c(-1, 1))
    )
  )

  graph <- protvis_workflow_graph(object)
  latest <- protvis_latest_result(object, "pca")
  testthat::expect_true(latest$run_id %in% graph$nodes$run_id)
  testthat::expect_true(any(
    graph$edges$from == root &
      graph$edges$to == latest$run_id
  ))
})
