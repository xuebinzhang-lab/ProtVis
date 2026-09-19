# ProtVis_dataset schema v4 run store -----------------------------------------
#
# Schema v4 keeps the physical S4 slot layout backward compatible while adding
# three logical project fields:
#   result_registry -> metadata$result_registry
#   artifacts       -> metadata$artifacts
#   workflow        -> metadata$workflow
#
# Canonical analysis history is append-only under analysis_results$v4$runs.
# Existing top-level analysis_results entries remain as a compatibility/latest
# view for older modules. The $<- compatibility hook mirrors those writes into
# this immutable run store.

.protvis_v4_reserved_results <- c(
  "v4", "assays", "assay_registry", "errors"
)

.protvis_empty_result_registry <- function() {
  data.frame(
    run_id = character(),
    category = character(),
    module = character(),
    method = character(),
    status = character(),
    parent_run_id = character(),
    dependencies = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    active = logical(),
    matrix_active = logical(),
    has_matrix = logical(),
    has_tables = logical(),
    has_plot_data = logical(),
    n_rows = integer(),
    n_cols = integer(),
    source = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.protvis_empty_artifacts <- function() {
  list(
    tables = list(),
    matrices = list(),
    plot_data = list(),
    plot_config = list(),
    files = list(),
    reports = list()
  )
}

.protvis_empty_workflow <- function() {
  list(
    active_run_id = NA_character_,
    active_matrix_run_id = NA_character_,
    nodes = data.frame(
      run_id = character(),
      category = character(),
      module = character(),
      method = character(),
      status = character(),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = character(),
      to = character(),
      relation = character(),
      stringsAsFactors = FALSE
    )
  )
}

.protvis_empty_run_store <- function() {
  list(
    schema_version = "4.0.0",
    runs = list(),
    by_module = list(),
    created_at = as.character(Sys.time())
  )
}

.protvis_v4_scalar <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(default)
  value <- trimws(as.character(x[[1L]]))
  if (nzchar(value)) value else default
}

.protvis_module_key <- function(x) {
  x <- tolower(.protvis_v4_scalar(x, "analysis"))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nzchar(x)) x else "analysis"
}

.protvis_module_category <- function(module) {
  key <- .protvis_module_key(module)
  if (grepl("sage|fragpipe|search|database_search|import|raw", key)) {
    return("search")
  }
  if (grepl(
    "noise|transform|imput|normaliz|normalis|missing_value|preprocess|correct",
    key
  )) return("preprocessing")
  if (grepl("qc|quality|summary|overview", key)) return("qc")
  if (grepl("pca|umap|tsne|dimensional|dr_analysis", key)) {
    return("dimensionality_reduction")
  }
  if (grepl(
    "differential|dep|deg|limma|deqms|proda|msstats", key
  )) return("differential_analysis")
  if (grepl("gsea|enrich|go_|kegg|pathview", key)) return("enrichment")
  if (grepl("wgcna|ppi|network|stringdb|correlation|chord", key)) {
    return("network")
  }
  if (grepl("metaproteomics|metaproteome", key)) {
    return("metaproteomics")
  }
  if (grepl("multi_omics|multiomics|co_enrichment|nine_quadrant", key)) {
    return("multi_omics")
  }
  if (grepl("psm|ptm|phospho|spectrum|spectra", key)) return("ptm")
  if (grepl("structure|swiss|nma|pdb", key)) return("protein_structure")
  if (grepl("localization|localisation|mploc", key)) {
    return("subcellular_localization")
  }
  if (grepl("venn|toolkit|blast|protein_fun|protein_link", key)) {
    return("toolkits")
  }
  if (grepl("project_init|creation|dataset_state|subset|activate", key)) {
    return("project")
  }
  "analysis"
}

.protvis_core_matrix_module <- function(module) {
  .protvis_module_key(module) %in% c(
    "noise_correction", "transformation", "imputation", "normalization",
    "normalisation", "data_clean", "data_transformed",
    "missing_value_imputation"
  )
}

.protvis_safe_method <- function(method, module) {
  value <- .protvis_v4_scalar(method, .protvis_module_key(module))
  if (nzchar(value)) value else .protvis_module_key(module)
}

.protvis_run_dimensions <- function(tables, matrices, raw = NULL) {
  candidates <- c(
    if (length(matrices)) matrices else list(),
    if (length(tables)) tables else list(),
    if (!is.null(raw)) list(raw) else list()
  )
  for (value in candidates) {
    if (is.data.frame(value) || is.matrix(value)) {
      return(c(as.integer(NROW(value)), as.integer(NCOL(value))))
    }
  }
  c(NA_integer_, NA_integer_)
}

.protvis_classify_value <- function(value, name = "result") {
  out <- list(
    tables = list(),
    matrices = list(),
    statistics = list(),
    plot_data = list(),
    plot_config = list(),
    objects = list()
  )
  if (is.null(value)) return(out)

  if (is.data.frame(value)) {
    out$tables[[name]] <- as.data.frame(
      value, stringsAsFactors = FALSE, check.names = FALSE
    )
    return(out)
  }
  if (is.matrix(value)) {
    out$matrices[[name]] <- value
    return(out)
  }
  if (inherits(value, "ggplot")) {
    if (is.data.frame(value$data)) out$plot_data[[name]] <- value$data
    out$plot_config[[name]] <- list(
      class = class(value),
      labels = value$labels %||% list(),
      mapping = tryCatch(as.list(value$mapping), error = function(e) list())
    )
    return(out)
  }
  if (is.atomic(value) && length(value) <= 1000L) {
    out$statistics[[name]] <- value
    return(out)
  }
  if (!is.list(value)) {
    out$objects[[name]] <- value
    return(out)
  }

  known_plot <- intersect(
    names(value),
    c("plot_data", "plot_df", "plot_table", "heatmap_data", "volcano_data")
  )
  for (field in known_plot) {
    candidate <- value[[field]]
    if (is.data.frame(candidate) || is.matrix(candidate)) {
      out$plot_data[[field]] <- candidate
    }
  }
  if (!is.null(value$plot_config)) {
    out$plot_config$plot_config <- value$plot_config
  }

  # Preserve every list component. Tabular/matrix components are promoted into
  # their typed containers; scalar/list statistics remain addressable without
  # duplicating very large table-like objects.
  for (field in names(value) %||% character()) {
    candidate <- value[[field]]
    field_name <- if (nzchar(field)) field else paste0("item_", which(names(value) == field)[1L])
    if (is.data.frame(candidate)) {
      out$tables[[field_name]] <- candidate
    } else if (is.matrix(candidate)) {
      out$matrices[[field_name]] <- candidate
    } else if (inherits(candidate, "ggplot")) {
      if (is.data.frame(candidate$data)) {
        out$plot_data[[field_name]] <- candidate$data
      }
      out$plot_config[[field_name]] <- list(
        class = class(candidate),
        labels = candidate$labels %||% list()
      )
    } else {
      out$statistics[[field_name]] <- candidate
    }
  }
  out
}

.protvis_registry_row <- function(
    run, active = FALSE, matrix_active = FALSE) {
  dims <- .protvis_run_dimensions(
    run$results$tables %||% list(),
    run$results$matrices %||% list(),
    run$results$raw %||% NULL
  )
  data.frame(
    run_id = .protvis_v4_scalar(run$run_id),
    category = .protvis_v4_scalar(run$category),
    module = .protvis_v4_scalar(run$module),
    method = .protvis_v4_scalar(run$method),
    status = .protvis_v4_scalar(run$status, "success"),
    parent_run_id = .protvis_v4_scalar(run$parent_run_id),
    dependencies = paste(
      unique(as.character(run$dependencies %||% character())),
      collapse = ";"
    ),
    started_at = .protvis_v4_scalar(run$started_at),
    finished_at = .protvis_v4_scalar(run$finished_at),
    duration_seconds = suppressWarnings(
      as.numeric(run$duration_seconds %||% NA_real_)
    ),
    active = isTRUE(active),
    matrix_active = isTRUE(matrix_active),
    has_matrix = length(run$results$matrices %||% list()) > 0L ||
      !is.null(run$state$expression_data),
    has_tables = length(run$results$tables %||% list()) > 0L,
    has_plot_data = length(run$results$plot_data %||% list()) > 0L,
    n_rows = dims[[1L]],
    n_cols = dims[[2L]],
    source = .protvis_v4_scalar(run$source, "ProtVis"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.protvis_v4_storage <- function(object) {
  results <- methods::slot(object, "analysis_results")
  store <- results$v4
  if (!is.list(store)) store <- .protvis_empty_run_store()
  store$schema_version <- "4.0.0"
  store$runs <- store$runs %||% list()
  store$by_module <- store$by_module %||% list()
  store
}

.protvis_v4_metadata <- function(object) {
  metadata <- methods::slot(object, "metadata")
  metadata$result_registry <- if (
    is.data.frame(metadata$result_registry)
  ) metadata$result_registry else .protvis_empty_result_registry()
  metadata$artifacts <- if (
    is.list(metadata$artifacts)
  ) metadata$artifacts else .protvis_empty_artifacts()
  metadata$workflow <- if (
    is.list(metadata$workflow)
  ) metadata$workflow else .protvis_empty_workflow()
  metadata$run_counter <- suppressWarnings(
    as.integer(metadata$run_counter %||% 0L)
  )
  if (!is.finite(metadata$run_counter)) metadata$run_counter <- 0L
  metadata
}

.protvis_enable_v4_direct <- function(object) {
  metadata <- .protvis_v4_metadata(object)
  metadata$schema_version <- "4.0.0"
  if (exists("protvis_schema", mode = "function")) {
    metadata$schema <- protvis_schema()
  }
  if (is.list(metadata$provenance)) {
    metadata$provenance$schema_version <- "4.0.0"
  }
  methods::slot(object, "metadata") <- metadata
  methods::slot(object, "version") <- "4.0.0"

  results <- methods::slot(object, "analysis_results")
  if (!is.list(results$v4)) results$v4 <- .protvis_empty_run_store()
  methods::slot(object, "analysis_results") <- results
  object
}

.protvis_run_id <- function(object, module, method) {
  metadata <- .protvis_v4_metadata(object)
  counter <- metadata$run_counter + 1L
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  paste0(
    .protvis_module_key(module), "__",
    .protvis_module_key(method), "__",
    stamp, "__", sprintf("%05d", counter)
  )
}

.protvis_add_workflow_node <- function(metadata, run) {
  workflow <- metadata$workflow %||% .protvis_empty_workflow()
  node <- data.frame(
    run_id = run$run_id,
    category = run$category,
    module = run$module,
    method = run$method,
    status = run$status,
    stringsAsFactors = FALSE
  )
  old <- workflow$nodes %||% .protvis_empty_workflow()$nodes
  old <- old[old$run_id != run$run_id, , drop = FALSE]
  workflow$nodes <- rbind(old, node)

  dependencies <- unique(as.character(run$dependencies %||% character()))
  dependencies <- dependencies[nzchar(dependencies)]
  if (length(dependencies)) {
    edges <- data.frame(
      from = dependencies,
      to = rep(run$run_id, length(dependencies)),
      relation = rep("depends_on", length(dependencies)),
      stringsAsFactors = FALSE
    )
    old_edges <- workflow$edges %||% .protvis_empty_workflow()$edges
    key_old <- paste(old_edges$from, old_edges$to, old_edges$relation, sep = "|")
    key_new <- paste(edges$from, edges$to, edges$relation, sep = "|")
    edges <- edges[!key_new %in% key_old, , drop = FALSE]
    if (nrow(edges)) workflow$edges <- rbind(old_edges, edges)
  }
  workflow$active_run_id <- run$run_id
  metadata$workflow <- workflow
  metadata
}

.protvis_add_artifact_index <- function(metadata, run) {
  artifacts <- metadata$artifacts %||% .protvis_empty_artifacts()
  add_names <- function(bucket, values) {
    if (!length(values)) return(bucket)
    bucket[[run$run_id]] <- unique(c(
      bucket[[run$run_id]] %||% character(),
      names(values) %||% paste0("item_", seq_along(values))
    ))
    bucket
  }
  artifacts$tables <- add_names(
    artifacts$tables %||% list(), run$results$tables %||% list()
  )
  artifacts$matrices <- add_names(
    artifacts$matrices %||% list(), run$results$matrices %||% list()
  )
  artifacts$plot_data <- add_names(
    artifacts$plot_data %||% list(), run$results$plot_data %||% list()
  )
  artifacts$plot_config <- add_names(
    artifacts$plot_config %||% list(), run$results$plot_config %||% list()
  )
  artifacts$files <- add_names(
    artifacts$files %||% list(), run$files %||% list()
  )
  metadata$artifacts <- artifacts
  metadata
}

.protvis_append_run_direct <- function(
    object, module, method = NULL, category = NULL, status = "success",
    parameters = list(), tables = list(), matrices = list(),
    statistics = list(), plot_data = list(), plot_config = list(),
    files = list(), dependencies = NULL, parent_run_id = NULL,
    source = "ProtVis", started_at = NULL, finished_at = NULL,
    state = NULL, raw = NULL, activate_matrix = FALSE) {
  metadata <- .protvis_v4_metadata(object)
  results <- methods::slot(object, "analysis_results")
  store <- .protvis_v4_storage(object)

  module <- .protvis_module_key(module)
  method <- .protvis_safe_method(method, module)
  category <- .protvis_v4_scalar(category, .protvis_module_category(module))
  run_id <- .protvis_run_id(object, module, method)

  workflow <- metadata$workflow %||% .protvis_empty_workflow()
  if (is.null(parent_run_id)) {
    parent_run_id <- if (.protvis_core_matrix_module(module) ||
                         isTRUE(activate_matrix)) {
      .protvis_v4_scalar(
        workflow$active_matrix_run_id,
        .protvis_v4_scalar(workflow$active_run_id)
      )
    } else {
      .protvis_v4_scalar(
        workflow$active_run_id,
        .protvis_v4_scalar(workflow$active_matrix_run_id)
      )
    }
  }
  if (is.null(dependencies)) {
    dependencies <- unique(c(
      .protvis_v4_scalar(workflow$active_matrix_run_id),
      .protvis_v4_scalar(parent_run_id)
    ))
  }
  dependencies <- unique(as.character(dependencies %||% character()))
  dependencies <- dependencies[nzchar(dependencies) & dependencies != run_id]

  if (is.null(started_at)) started_at <- Sys.time()
  if (is.null(finished_at)) finished_at <- Sys.time()
  duration <- suppressWarnings(as.numeric(
    difftime(finished_at, started_at, units = "secs")
  ))

  tables <- tables %||% list()
  matrices <- matrices %||% list()
  statistics <- statistics %||% list()
  plot_data <- plot_data %||% list()
  plot_config <- plot_config %||% list()
  files <- files %||% list()

  run <- list(
    run_id = run_id,
    schema_version = "4.0.0",
    category = category,
    module = module,
    method = method,
    status = .protvis_v4_scalar(status, "success"),
    source = .protvis_v4_scalar(source, "ProtVis"),
    parent_run_id = .protvis_v4_scalar(parent_run_id),
    dependencies = dependencies,
    input = list(
      active_matrix_run_id = .protvis_v4_scalar(workflow$active_matrix_run_id),
      n_features = nrow(methods::slot(object, "expression_data")),
      n_samples = ncol(methods::slot(object, "expression_data"))
    ),
    parameters = parameters %||% list(),
    results = list(
      tables = tables,
      matrices = matrices,
      statistics = statistics,
      plot_data = plot_data,
      plot_config = plot_config,
      raw = raw
    ),
    files = files,
    state = state,
    started_at = as.character(started_at),
    finished_at = as.character(finished_at),
    duration_seconds = duration,
    warnings = character(),
    error = NULL,
    sealed = FALSE
  )

  store$runs[[run_id]] <- run
  module_entry <- store$by_module[[category]][[module]] %||%
    list(latest = NA_character_, runs = character())
  module_entry$runs <- c(module_entry$runs %||% character(), run_id)
  module_entry$latest <- run_id
  if (is.null(store$by_module[[category]])) store$by_module[[category]] <- list()
  store$by_module[[category]][[module]] <- module_entry
  results$v4 <- store
  methods::slot(object, "analysis_results") <- results

  metadata$run_counter <- metadata$run_counter + 1L
  registry <- metadata$result_registry
  if (nrow(registry)) {
    registry$active <- FALSE
    if (!"matrix_active" %in% names(registry)) registry$matrix_active <- FALSE
    if (isTRUE(activate_matrix)) registry$matrix_active <- FALSE
  }
  registry <- rbind(
    registry,
    .protvis_registry_row(
      run,
      active = TRUE,
      matrix_active = isTRUE(activate_matrix)
    )
  )
  metadata$result_registry <- registry
  metadata <- .protvis_add_artifact_index(metadata, run)
  metadata <- .protvis_add_workflow_node(metadata, run)

  if (isTRUE(activate_matrix) || !is.null(state$expression_data)) {
    metadata$workflow$active_matrix_run_id <- run_id
  }
  methods::slot(object, "metadata") <- metadata
  object
}

#' Append one immutable analysis run to a ProtVis_dataset.
#'
#' This is the canonical schema-v4 result writer. Existing runs are never
#' replaced. The top-level analysis_results list remains a compatibility/latest
#' view for legacy modules.
#' @export
add_protvis_run <- function(
    object, module, method = NULL, category = NULL, status = "success",
    parameters = list(), tables = list(), matrices = list(),
    statistics = list(), plot_data = list(), plot_config = list(),
    files = list(), dependencies = NULL, parent_run_id = NULL,
    source = "ProtVis", activate_matrix = FALSE, expression_data = NULL,
    sample_info = NULL, variable_info = NULL, legacy_value = NULL,
    record_process = TRUE) {
  object <- protvis_standardize_dataset(object)
  module <- .protvis_module_key(module)
  core <- .protvis_core_matrix_module(module)

  # When callers supply a new core matrix, make it the live dataset state
  # before snapshotting the run. This preserves the v4 invariant that
  # expression_data always represents the currently activated matrix.
  if (!is.null(expression_data)) {
    object <- .protvis_update_expression(object, expression_data)
    if (!is.null(sample_info)) {
      info <- .protvis_normalise_sample_info(
        sample_info, colnames(methods::slot(object, "expression_data"))
      )
      info$class <- as.character(info$group)
      methods::slot(object, "sample_info") <- info
    }
    if (!is.null(variable_info)) {
      methods::slot(object, "variable_info") <-
        .protvis_normalise_variable_info(
          variable_info, rownames(methods::slot(object, "expression_data"))
        )
    }
    validate_protvis_dataset(object)
  }

  state <- NULL
  if (isTRUE(core) || isTRUE(activate_matrix) || !is.null(expression_data)) {
    expression <- expression_data %||% methods::slot(object, "expression_data")
    expression <- as.data.frame(
      expression, stringsAsFactors = FALSE, check.names = FALSE
    )
    sample_snapshot <- sample_info %||% methods::slot(object, "sample_info")
    variable_snapshot <- variable_info %||% methods::slot(object, "variable_info")
    state <- list(
      expression_data = expression,
      sample_info = sample_snapshot,
      variable_info = variable_snapshot
    )
    matrices$expression_data <- matrices$expression_data %||% expression
    activate_matrix <- TRUE
  }

  started <- Sys.time()
  object <- .protvis_append_run_direct(
    object = object,
    module = module,
    method = method,
    category = category,
    status = status,
    parameters = parameters,
    tables = tables,
    matrices = matrices,
    statistics = statistics,
    plot_data = plot_data,
    plot_config = plot_config,
    files = files,
    dependencies = dependencies,
    parent_run_id = parent_run_id,
    source = source,
    started_at = started,
    finished_at = Sys.time(),
    state = state,
    raw = legacy_value,
    activate_matrix = activate_matrix
  )

  store <- .protvis_v4_storage(object)
  category_key <- .protvis_v4_scalar(
    category, .protvis_module_category(module)
  )
  run_id <- store$by_module[[category_key]][[module]]$latest

  if (!is.null(legacy_value)) {
    results <- methods::slot(object, "analysis_results")
    results[[module]] <- legacy_value
    methods::slot(object, "analysis_results") <- results
  }

  if (isTRUE(record_process)) {
    object <- .protvis_append_process(
      object, module, status = status, parameters = parameters,
      message = paste0("Schema-v4 run recorded: ", run_id)
    )
  }
  object
}


.protvis_persist_latest_dataset <- function(object, directory = NULL) {
  if (is.null(directory) || !length(directory) ||
      is.na(directory[[1L]]) || !nzchar(as.character(directory[[1L]]))) {
    return(invisible(NULL))
  }
  directory <- path.expand(as.character(directory[[1L]]))
  if (!dir.exists(directory)) return(invisible(NULL))
  target <- file.path(directory, "ProtVis_dataset_latest.rds")
  temporary <- tempfile(
    pattern = ".ProtVis_dataset_latest_",
    tmpdir = directory,
    fileext = ".rds"
  )
  ok <- tryCatch({
    saveRDS(object, temporary, compress = TRUE)
    if (!file.rename(temporary, target)) {
      copied <- file.copy(temporary, target, overwrite = TRUE)
      unlink(temporary, force = TRUE)
      isTRUE(copied)
    } else {
      TRUE
    }
  }, error = function(e) {
    unlink(temporary, force = TRUE)
    FALSE
  })
  if (isTRUE(ok)) normalizePath(target, winslash = "/", mustWork = FALSE)
  else invisible(NULL)
}

# Record an output from any Shiny module that participates in a ProtVis project.
# Stand-alone use remains valid because a missing shared_state/dataset is a no-op.
.protvis_record_shared_run <- function(
    shared_state, module, method = NULL, category = NULL,
    status = "success", parameters = list(), tables = list(),
    matrices = list(), statistics = list(), plot_data = list(),
    plot_config = list(), files = list(), dependencies = NULL,
    activate_matrix = FALSE) {
  if (is.null(shared_state) ||
      !inherits(shared_state$dataset, "ProtVis_dataset")) {
    return(invisible(NULL))
  }
  dataset <- add_protvis_run(
    shared_state$dataset,
    module = module,
    method = method,
    category = category,
    status = status,
    parameters = parameters,
    tables = tables,
    matrices = matrices,
    statistics = statistics,
    plot_data = plot_data,
    plot_config = plot_config,
    files = files,
    dependencies = dependencies,
    activate_matrix = activate_matrix,
    record_process = TRUE
  )
  if (exists(".protvis_ui_sync_state", mode = "function")) {
    .protvis_ui_sync_state(dataset, shared_state)
  } else {
    shared_state$dataset <- dataset
  }
  .protvis_persist_latest_dataset(
    dataset,
    shared_state$workdir %||% dataset$metadata$output_directory %||% NULL
  )
  invisible(dataset)
}

#' Record a general analysis output in schema-v4 form.
#'
#' This is a public convenience wrapper for custom extensions and scripted
#' analyses. Every call appends a new run; previous runs remain unchanged.
#' @export
record_protvis_output <- function(
    object, module, method = NULL, category = NULL,
    parameters = list(), tables = list(), matrices = list(),
    statistics = list(), plot_data = list(), plot_config = list(),
    files = list(), dependencies = NULL, status = "success") {
  add_protvis_run(
    object,
    module = module,
    method = method,
    category = category,
    status = status,
    parameters = parameters,
    tables = tables,
    matrices = matrices,
    statistics = statistics,
    plot_data = plot_data,
    plot_config = plot_config,
    files = files,
    dependencies = dependencies,
    record_process = TRUE
  )
}

.protvis_legacy_to_run <- function(object, module, value) {
  module <- .protvis_module_key(module)
  category <- .protvis_module_category(module)

  # Existing schema-v4-aware modules (currently metaproteomics and future
  # extensions) may already keep their own module-local run store. Mirror only
  # the newest run into the project-wide registry instead of serializing the
  # entire historical module root again.
  payload <- value
  if (is.list(value) && is.list(value$runs) && length(value$runs)) {
    local_id <- .protvis_v4_scalar(
      value$latest_run_id,
      tail(names(value$runs) %||% character(), 1L)
    )
    if (nzchar(local_id) && !is.null(value$runs[[local_id]])) {
      payload <- value$runs[[local_id]]
    }
  }

  classified <- .protvis_classify_value(payload)
  method <- if (is.list(payload)) payload$method %||% module else module
  status <- if (is.list(payload)) payload$status %||% "success" else "success"
  parameters <- if (is.list(payload) && is.list(payload$parameters)) {
    payload$parameters
  } else {
    methods::slot(object, "process_info")$parameters[[module]] %||% list()
  }

  # Promote typed containers used by v4-aware modules.
  if (is.list(payload)) {
    if (is.list(payload$tables)) {
      typed <- payload$tables[
        vapply(payload$tables, is.data.frame, logical(1))
      ]
      classified$tables <- utils::modifyList(classified$tables, typed)
    }
    if (is.list(payload$matrices)) {
      typed <- payload$matrices[
        vapply(payload$matrices, function(x) is.matrix(x) || is.data.frame(x),
               logical(1))
      ]
      classified$matrices <- utils::modifyList(classified$matrices, typed)
    }
    if (is.list(payload$plot_data)) {
      classified$plot_data <- utils::modifyList(
        classified$plot_data, payload$plot_data
      )
    }
    if (is.list(payload$plot_config)) {
      classified$plot_config <- utils::modifyList(
        classified$plot_config, payload$plot_config
      )
    }
    if (is.list(payload$statistics)) {
      classified$statistics <- utils::modifyList(
        classified$statistics, payload$statistics
      )
    }
  }

  state <- NULL
  matrices <- classified$matrices
  if (.protvis_core_matrix_module(module)) {
    expression <- methods::slot(object, "expression_data")
    state <- list(
      expression_data = expression,
      sample_info = methods::slot(object, "sample_info"),
      variable_info = methods::slot(object, "variable_info")
    )
    matrices$expression_data <- expression
  }

  # Multiple internal assignments made during one execution are merged into
  # the current unsealed draft. Once .protvis_append_process() seals the run,
  # the next execution necessarily appends a new immutable run.
  store <- .protvis_v4_storage(object)
  latest_id <- .protvis_latest_run_id(object, module, category)
  latest <- if (nzchar(latest_id)) store$runs[[latest_id]] else NULL
  recent_draft <- FALSE
  if (!is.null(latest) && !isTRUE(latest$sealed)) {
    elapsed <- suppressWarnings(as.numeric(difftime(
      Sys.time(), as.POSIXct(latest$finished_at), units = "secs"
    )))
    recent_draft <- is.finite(elapsed) && abs(elapsed) <= 5
  }

  if (isTRUE(recent_draft)) {
    latest$method <- .protvis_safe_method(method, module)
    latest$status <- .protvis_v4_scalar(status, "success")
    latest$parameters <- utils::modifyList(
      latest$parameters %||% list(), parameters %||% list()
    )
    latest$results <- list(
      tables = classified$tables,
      matrices = matrices,
      statistics = classified$statistics,
      plot_data = classified$plot_data,
      plot_config = classified$plot_config,
      raw = payload
    )
    latest$state <- state
    latest$finished_at <- as.character(Sys.time())
    latest$source <- "legacy_compat"
    store$runs[[latest_id]] <- latest

    results <- methods::slot(object, "analysis_results")
    results$v4 <- store
    methods::slot(object, "analysis_results") <- results

    metadata <- .protvis_v4_metadata(object)
    registry <- metadata$result_registry
    hit <- which(registry$run_id == latest_id)
    if (length(hit)) {
      is_matrix <- identical(
        .protvis_v4_scalar(metadata$workflow$active_matrix_run_id),
        latest_id
      ) || .protvis_core_matrix_module(module)
      registry[hit[[length(hit)]], ] <- .protvis_registry_row(
        latest,
        active = identical(
          .protvis_v4_scalar(metadata$workflow$active_run_id), latest_id
        ),
        matrix_active = is_matrix
      )
    }
    metadata$result_registry <- registry
    metadata <- .protvis_add_artifact_index(metadata, latest)
    if (.protvis_core_matrix_module(module)) {
      metadata$workflow$active_matrix_run_id <- latest_id
    }
    methods::slot(object, "metadata") <- metadata
    return(object)
  }

  .protvis_append_run_direct(
    object,
    module = module,
    method = method,
    status = status,
    parameters = parameters,
    tables = classified$tables,
    matrices = matrices,
    statistics = classified$statistics,
    plot_data = classified$plot_data,
    plot_config = classified$plot_config,
    raw = payload,
    state = state,
    activate_matrix = .protvis_core_matrix_module(module),
    source = "legacy_compat"
  )
}

.protvis_capture_analysis_assignment <- function(object, old, value) {
  metadata <- methods::slot(object, "metadata")
  if (!identical(
    as.character(metadata$schema_version %||% ""),
    "4.0.0"
  )) {
    object <- .protvis_enable_v4_direct(object)
    object <- .protvis_migrate_legacy_results(object)
    old <- methods::slot(object, "analysis_results")
  }
  if (!is.list(value)) {
    methods::slot(object, "analysis_results") <- value
    return(object)
  }

  # Canonical history is protected from accidental replacement by old modules.
  old_v4 <- old$v4
  if (is.list(old_v4)) value$v4 <- old_v4

  changed <- setdiff(
    union(names(old) %||% character(), names(value) %||% character()),
    .protvis_v4_reserved_results
  )
  changed <- changed[vapply(changed, function(name) {
    !identical(old[[name]], value[[name]])
  }, logical(1))]

  methods::slot(object, "analysis_results") <- value
  if (!length(changed)) return(object)

  for (name in changed) {
    current <- methods::slot(object, "analysis_results")[[name]]
    if (is.null(current)) next
    object <- .protvis_legacy_to_run(object, name, current)
  }
  object
}

.protvis_latest_run_id <- function(object, module, category = NULL) {
  store <- .protvis_v4_storage(object)
  module <- .protvis_module_key(module)
  category <- .protvis_v4_scalar(category, .protvis_module_category(module))
  .protvis_v4_scalar(store$by_module[[category]][[module]]$latest)
}

.protvis_sync_process_to_run <- function(
    object, stage, status, parameters = list(), error = NULL, message = NULL,
    started_at = NULL, finished_at = NULL) {
  metadata <- .protvis_v4_metadata(object)
  if (!identical(
    as.character(metadata$schema_version %||% ""),
    "4.0.0"
  )) {
    object <- .protvis_enable_v4_direct(object)
    object <- .protvis_migrate_legacy_results(object)
    metadata <- .protvis_v4_metadata(object)
  }

  module <- .protvis_module_key(stage)
  category <- .protvis_module_category(module)
  run_id <- .protvis_latest_run_id(object, module, category)

  # A result writer and its immediately-following process event belong to the
  # same run. A later execution of the same module must append a NEW run rather
  # than mutate historical state.
  results <- methods::slot(object, "analysis_results")
  store <- results$v4
  run <- if (nzchar(run_id)) store$runs[[run_id]] else NULL
  event_time <- finished_at %||% Sys.time()
  recent_same_run <- FALSE
  if (!is.null(run) && !isTRUE(run$sealed)) {
    run_time <- suppressWarnings(as.POSIXct(run$finished_at))
    now_time <- suppressWarnings(as.POSIXct(event_time))
    recent_same_run <- is.finite(as.numeric(run_time)) &&
      is.finite(as.numeric(now_time)) &&
      abs(as.numeric(difftime(now_time, run_time, units = "secs"))) <= 5
  }

  # A process-only operation still becomes a run, so project history remains
  # complete even when a module does not produce a table or matrix.
  if (!nzchar(run_id) || !isTRUE(recent_same_run)) {
    state <- NULL
    matrices <- list()
    activate_matrix <- FALSE
    if (module %in% c("subset", "dataset_state")) {
      expression <- methods::slot(object, "expression_data")
      state <- list(
        expression_data = expression,
        sample_info = methods::slot(object, "sample_info"),
        variable_info = methods::slot(object, "variable_info")
      )
      matrices$expression_data <- expression
      activate_matrix <- TRUE
    }
    object <- .protvis_append_run_direct(
      object,
      module = module,
      method = parameters$method %||% module,
      category = category,
      status = status,
      parameters = parameters,
      matrices = matrices,
      state = state,
      activate_matrix = activate_matrix,
      started_at = started_at %||% Sys.time(),
      finished_at = event_time,
      source = "process_event"
    )
    run_id <- .protvis_latest_run_id(object, module, category)
    results <- methods::slot(object, "analysis_results")
    store <- results$v4
    run <- store$runs[[run_id]]
  }

  if (is.null(run)) return(object)

  run$status <- .protvis_v4_scalar(status, run$status %||% "success")
  run$sealed <- TRUE
  run$parameters <- utils::modifyList(
    run$parameters %||% list(), parameters %||% list()
  )
  if (!is.null(error)) run$error <- as.character(error)
  if (!is.null(message)) run$message <- as.character(message)
  if (!is.null(started_at)) run$started_at <- as.character(started_at)
  if (!is.null(finished_at)) run$finished_at <- as.character(finished_at)
  run$duration_seconds <- suppressWarnings(as.numeric(difftime(
    as.POSIXct(run$finished_at),
    as.POSIXct(run$started_at),
    units = "secs"
  )))
  store$runs[[run_id]] <- run
  results$v4 <- store
  methods::slot(object, "analysis_results") <- results

  registry <- metadata$result_registry
  hit <- which(registry$run_id == run_id)
  if (length(hit)) {
    registry[hit[[length(hit)]], ] <- .protvis_registry_row(
      run,
      active = identical(
        .protvis_v4_scalar(metadata$workflow$active_run_id), run_id
      ),
      matrix_active = identical(
        .protvis_v4_scalar(metadata$workflow$active_matrix_run_id), run_id
      )
    )
  }
  metadata$result_registry <- registry
  metadata$workflow$nodes$status[
    metadata$workflow$nodes$run_id == run_id
  ] <- run$status
  methods::slot(object, "metadata") <- metadata
  object
}

.protvis_migrate_legacy_results <- function(object) {
  results <- methods::slot(object, "analysis_results")
  store <- .protvis_v4_storage(object)
  if (length(store$runs)) {
    results$v4 <- store
    methods::slot(object, "analysis_results") <- results
    metadata <- .protvis_v4_metadata(object)
    active_matrix <- .protvis_v4_scalar(
      metadata$workflow$active_matrix_run_id
    )
    if (!nzchar(active_matrix)) {
      object <- .protvis_append_run_direct(
        object,
        module = "dataset_state",
        method = .protvis_v4_scalar(
          methods::slot(object, "metadata")$source, "current"
        ),
        category = "project",
        status = "success",
        matrices = list(
          expression_data = methods::slot(object, "expression_data")
        ),
        state = list(
          expression_data = methods::slot(object, "expression_data"),
          sample_info = methods::slot(object, "sample_info"),
          variable_info = methods::slot(object, "variable_info")
        ),
        source = "schema_v4_state_anchor",
        activate_matrix = TRUE
      )
    }
    return(object)
  }

  legacy_names <- setdiff(
    names(results) %||% character(),
    .protvis_v4_reserved_results
  )
  results$v4 <- store
  methods::slot(object, "analysis_results") <- results

  # Anchor the current matrix as an explicit project state before migrated
  # analyses are appended. This gives old projects a valid DAG root.
  object <- .protvis_append_run_direct(
    object,
    module = "dataset_state",
    method = .protvis_v4_scalar(
      methods::slot(object, "metadata")$source, "imported"
    ),
    category = "project",
    status = "success",
    matrices = list(
      expression_data = methods::slot(object, "expression_data")
    ),
    state = list(
      expression_data = methods::slot(object, "expression_data"),
      sample_info = methods::slot(object, "sample_info"),
      variable_info = methods::slot(object, "variable_info")
    ),
    source = "schema_v4_migration",
    activate_matrix = TRUE
  )

  for (name in legacy_names) {
    value <- methods::slot(object, "analysis_results")[[name]]
    if (!is.null(value)) object <- .protvis_legacy_to_run(object, name, value)
  }
  object
}

#' Return the schema-v4 result registry.
#' @export
protvis_result_registry <- function(object, module = NULL, category = NULL) {
  object <- protvis_standardize_dataset(object)
  registry <- methods::slot(object, "metadata")$result_registry
  if (!is.null(module)) {
    registry <- registry[
      registry$module == .protvis_module_key(module), , drop = FALSE
    ]
  }
  if (!is.null(category)) {
    registry <- registry[
      registry$category == .protvis_module_key(category), , drop = FALSE
    ]
  }
  rownames(registry) <- NULL
  registry
}

#' Return one schema-v4 run by run_id.
#' @export
protvis_result <- function(object, run_id) {
  object <- protvis_standardize_dataset(object)
  run_id <- .protvis_v4_scalar(run_id)
  run <- .protvis_v4_storage(object)$runs[[run_id]]
  if (is.null(run)) stop("ProtVis run not found: ", run_id, call. = FALSE)
  run
}

#' Return all runs for one module in execution order.
#' @export
protvis_result_history <- function(object, module, category = NULL) {
  object <- protvis_standardize_dataset(object)
  module <- .protvis_module_key(module)
  category <- .protvis_v4_scalar(category, .protvis_module_category(module))
  store <- .protvis_v4_storage(object)
  ids <- store$by_module[[category]][[module]]$runs %||% character()
  lapply(ids, function(id) store$runs[[id]])
}

#' Return the latest run for one module.
#' @export
protvis_latest_result <- function(object, module, category = NULL) {
  object <- protvis_standardize_dataset(object)
  run_id <- .protvis_latest_run_id(object, module, category)
  if (!nzchar(run_id)) return(NULL)
  protvis_result(object, run_id)
}

#' Activate a stored matrix-producing run as expression_data.
#' @export
protvis_activate_result <- function(object, run_id) {
  object <- protvis_standardize_dataset(object)
  run <- protvis_result(object, run_id)
  state <- run$state
  if (!is.list(state) || is.null(state$expression_data)) {
    stop("The selected run does not contain an activatable matrix state.",
         call. = FALSE)
  }

  expression <- as.data.frame(
    state$expression_data, stringsAsFactors = FALSE, check.names = FALSE
  )
  methods::slot(object, "expression_data") <- expression
  if (is.data.frame(state$sample_info)) {
    methods::slot(object, "sample_info") <- state$sample_info
  }
  if (is.data.frame(state$variable_info)) {
    methods::slot(object, "variable_info") <- state$variable_info
  }

  metadata <- .protvis_v4_metadata(object)
  if (nrow(metadata$result_registry)) {
    metadata$result_registry$active <- FALSE
    if (!"matrix_active" %in% names(metadata$result_registry)) {
      metadata$result_registry$matrix_active <- FALSE
    }
    metadata$result_registry$matrix_active <-
      metadata$result_registry$run_id == run_id
  }
  metadata$workflow$active_matrix_run_id <- run_id
  metadata$workflow$active_run_id <- run_id
  methods::slot(object, "metadata") <- metadata

  object <- .protvis_append_run_direct(
    object,
    module = "activate_result",
    method = run$module,
    category = "project",
    status = "success",
    parameters = list(activated_run_id = run_id),
    dependencies = run_id,
    source = "ProtVis",
    activate_matrix = FALSE
  )
  object <- .protvis_append_process(
    object, "activate_result", status = "success",
    parameters = list(activated_run_id = run_id),
    message = paste0("Activated schema-v4 run ", run_id, ".")
  )
  validate_protvis_dataset(object)
  object
}

#' Return the schema-v4 workflow graph.
#' @export
protvis_workflow_graph <- function(object) {
  object <- protvis_standardize_dataset(object)
  methods::slot(object, "metadata")$workflow
}

#' Return the schema-v4 artifact index.
#' @export
protvis_artifacts <- function(object) {
  object <- protvis_standardize_dataset(object)
  methods::slot(object, "metadata")$artifacts
}
