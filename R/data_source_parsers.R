utils::globalVariables(c("Sample", "Intensity", "Group", "PC1", "PC2", "sample_id", "maxquant_id", "group"))

read_proteomics_table <- function(path, filename = NULL) {
  if (exists("protvis_read_table", mode = "function")) {
    return(protvis_read_table(path, filename = filename %||% basename(path)))
  }
  ext <- base::tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path), check.names = FALSE))
  }
  sep <- if (ext %in% c("tsv", "txt")) "\t" else ","
  as.data.frame(data.table::fread(path, sep = sep, data.table = FALSE, check.names = FALSE))
}

first_matching_column <- function(columns, patterns) {
  for (pattern in patterns) {
    hit <- columns[base::grepl(pattern, columns, ignore.case = TRUE)]
    if (base::length(hit) > 0) return(hit[1])
  }
  NULL
}

safe_numeric <- function(x) {
  if (base::is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
}

# MaxQuant exports encode not-observed reporter intensities as zero in the
# bundled workbook and some exports use -8 as a sentinel.  Neither value is a
# measured raw abundance.  Convert both to NA at the import boundary.  Keep
# this source-specific so valid zeros/negative values from other sources are
# not changed.
normalise_proteomics_missing_values <- function(expression_matrix, source = NULL) {
  if (is.null(source) || !identical(tolower(as.character(source)), "maxquant")) {
    return(expression_matrix)
  }
  sample_cols <- base::setdiff(base::names(expression_matrix), "ID")
  for (column in sample_cols) {
    values <- safe_numeric(expression_matrix[[column]])
    expression_matrix[[column]][!is.na(values) & values %in% c(0, -8)] <- NA_real_
  }
  expression_matrix
}

numeric_column_names <- function(df, exclude = character()) {
  candidates <- base::setdiff(base::names(df), exclude)
  candidates[vapply(df[candidates], function(x) {
    values <- safe_numeric(x)
    sum(!is.na(values)) > 0
  }, logical(1))]
}

clean_sample_names <- function(x) {
  x <- base::gsub("^Abundances?[: ]+", "", x, ignore.case = TRUE)
  x <- base::gsub("^Abundances? [(](Normalized|Grouped|by Bio[.] Rep[.]|Scaled)[)][: ]*", "", x, ignore.case = TRUE)
  x <- base::gsub("^Area[: ]+", "", x, ignore.case = TRUE)
  x <- base::gsub("^(Intensity|Abundance|LFQ intensity|MSstats Area)[_: ]+", "", x, ignore.case = TRUE)
  base::make.names(x, unique = TRUE)
}

guess_sample_info <- function(samples) {
  groups <- rep("Unassigned", length(samples))
  data.frame(
    sample_id = samples,
    maxquant_id = samples,
    group = groups,
    Sample = samples,
    Group = groups,
    stringsAsFactors = FALSE
  )
}

normalise_sample_info <- function(sample_info, samples) {
  if (base::is.null(sample_info)) return(guess_sample_info(samples))
  names(sample_info) <- base::make.names(names(sample_info), unique = TRUE)
  cols <- names(sample_info)
  maxquant_col <- first_matching_column(cols, c("^maxquant_id$", "maxquant", "raw.file", "file", "run", "^sample$", "^Sample$"))
  sample_col <- first_matching_column(cols, c("^sample_id$", "^Sample$", "sample.name", "sample_id", "run", "file"))
  group_col <- first_matching_column(cols, c("^group$", "^Group$", "group", "condition", "treatment", "class"))
  if (base::is.null(maxquant_col)) maxquant_col <- if (!base::is.null(sample_col)) sample_col else names(sample_info)[1]
  if (base::is.null(sample_col)) sample_col <- maxquant_col
  if (base::is.null(group_col)) group_col <- sample_col
  if (identical(group_col, sample_col) && base::length(cols) > 1) {
    fallback_group <- first_matching_column(base::setdiff(cols, sample_col), c("^group$", "^Group$", "group", "condition", "treatment", "class"))
    if (!base::is.null(fallback_group)) group_col <- fallback_group
  }
  out <- data.frame(
    sample_id = base::make.names(as.character(sample_info[[sample_col]]), unique = TRUE),
    maxquant_id = base::make.names(as.character(sample_info[[maxquant_col]]), unique = TRUE),
    group = as.character(sample_info[[group_col]]),
    stringsAsFactors = FALSE
  )
  matched <- out[out$maxquant_id %in% samples, , drop = FALSE]
  if (nrow(matched) == 0 && nrow(out) == length(samples)) {
    matched <- out
    matched$maxquant_id <- samples
  }
  missing_samples <- base::setdiff(samples, matched$maxquant_id)
  if (base::length(missing_samples) > 0) matched <- rbind(matched, guess_sample_info(missing_samples)[, c("sample_id", "maxquant_id", "group")])
  matched$Sample <- matched$sample_id
  matched$Group <- matched$group
  matched
}

summarise_numeric_mean <- function(x) {
  if (base::all(base::is.na(x))) return(NA_real_)
  base::mean(x, na.rm = TRUE)
}

build_expression_matrix <- function(df, id_col, abundance_cols, sample_names = NULL) {
  if (base::is.null(sample_names)) sample_names <- clean_sample_names(abundance_cols)
  expr <- data.frame(ID = as.character(df[[id_col]]), stringsAsFactors = FALSE)
  for (i in seq_along(abundance_cols)) expr[[sample_names[i]]] <- safe_numeric(df[[abundance_cols[i]]])
  expr <- expr[!is.na(expr$ID) & expr$ID != "", , drop = FALSE]
  stats::aggregate(. ~ ID, data = expr, FUN = summarise_numeric_mean, na.action = na.pass)
}

validate_protvis_data <- function(expression_matrix, sample_info = NULL,
                                  source = NULL) {
  if (base::is.null(expression_matrix) || !base::is.data.frame(expression_matrix)) {
    stop("Expression matrix must be a data frame.", call. = FALSE)
  }
  if (!"ID" %in% base::names(expression_matrix)) {
    base::names(expression_matrix)[1] <- "ID"
  }
  if (base::ncol(expression_matrix) < 2) {
    stop("Expression matrix must contain ID and at least one sample column.", call. = FALSE)
  }
  sample_cols <- base::setdiff(base::names(expression_matrix), "ID")
  for (sample_col in sample_cols) {
    expression_matrix[[sample_col]] <- safe_numeric(expression_matrix[[sample_col]])
  }
  expression_matrix <- normalise_proteomics_missing_values(
    expression_matrix, source = source
  )
  expression_matrix <- expression_matrix[!base::is.na(expression_matrix$ID) & expression_matrix$ID != "", , drop = FALSE]
  if (base::nrow(expression_matrix) == 0) {
    stop("Expression matrix contains no valid protein IDs.", call. = FALSE)
  }
  expression_matrix[sample_cols] <- base::lapply(expression_matrix[sample_cols], function(x) {
    x[base::is.nan(x)] <- NA_real_
    x
  })
  if (base::is.null(sample_info)) {
    sample_info <- guess_sample_info(sample_cols)
  } else {
    sample_info <- normalise_sample_info(sample_info, sample_cols)
  }
  missing_samples <- base::setdiff(sample_cols, sample_info$maxquant_id)
  if (base::length(missing_samples) > 0) {
    sample_info <- base::rbind(sample_info, guess_sample_info(missing_samples)[, c("sample_id", "maxquant_id", "group", "Sample", "Group")])
  }
  list(expression_matrix = expression_matrix, sample_info = sample_info)
}

parse_proteome_discoverer_output <- function(df) {
  id_col <- first_matching_column(names(df), c("Master Protein Accessions", "Accession", "Protein Accessions", "Protein Group ID", "Protein ID"))
  abundance_cols <- names(df)[grepl("Abundances?|Area", names(df), ignore.case = TRUE)]
  abundance_cols <- numeric_column_names(df, exclude = id_col)[numeric_column_names(df, exclude = id_col) %in% abundance_cols]
  if (base::is.null(id_col) || base::length(abundance_cols) == 0) stop("Proteome Discoverer tables require an accession/protein ID column and abundance or area columns.", call. = FALSE)
  list(
    expression_matrix = build_expression_matrix(df, id_col, abundance_cols),
    note = "Parsed Proteome Discoverer protein/peptide group export using accession plus Abundance/Area columns."
  )
}

parse_skyline_output <- function(df) {
  cols <- names(df)
  protein_col <- first_matching_column(cols, c("^ProteinName$", "Protein.Name", "Protein", "Accession"))
  file_col <- first_matching_column(cols, c("^FileName$", "Replicate", "Run", "Sample"))
  area_col <- first_matching_column(cols, c("^Area$", "TotalArea", "PeakArea", "Intensity"))
  if (!is.null(protein_col) && !is.null(file_col) && !is.null(area_col)) {
    long <- data.frame(
      ID = as.character(df[[protein_col]]),
      Sample = clean_sample_names(as.character(df[[file_col]])),
      Intensity = safe_numeric(df[[area_col]]),
      stringsAsFactors = FALSE
    )
    long <- long[!is.na(long$ID) & !is.na(long$Intensity), , drop = FALSE]
    summary_long <- stats::aggregate(Intensity ~ ID + Sample, data = long, FUN = summarise_numeric_mean, na.action = na.pass)
    expr <- tidyr::pivot_wider(summary_long, names_from = Sample, values_from = Intensity)
    expr <- as.data.frame(expr, check.names = FALSE)
    return(list(expression_matrix = expr, note = "Parsed Skyline MSstats-style long report using ProteinName/FileName/Area; duplicate protein/sample rows were averaged."))
  }
  id_col <- first_matching_column(cols, c("^ProteinName$", "Protein.Name", "Protein", "Accession"))
  abundance_cols <- numeric_column_names(df, exclude = id_col)
  if (is.null(id_col) || length(abundance_cols) == 0) stop("Skyline reports require ProteinName plus FileName/Area columns or a wide protein abundance table.", call. = FALSE)
  list(expression_matrix = build_expression_matrix(df, id_col, abundance_cols), note = "Parsed Skyline wide protein abundance report.")
}

parse_mascot_output <- function(df) {
  id_col <- first_matching_column(names(df), c("^prot_acc$", "prot_acc", "accession", "Protein", "prot_hit_num"))
  score_cols <- names(df)[grepl("score|mass|m/z|query|rank|expect|identity|homology", names(df), ignore.case = TRUE)]
  abundance_cols <- numeric_column_names(df, exclude = c(id_col, score_cols))
  if (length(abundance_cols) == 0) abundance_cols <- numeric_column_names(df, exclude = id_col)
  abundance_cols <- abundance_cols[!is.na(abundance_cols)]
  if (is.null(id_col) || length(abundance_cols) == 0) stop("Mascot CSV exports require a protein accession column and at least one numeric quantitation column such as emPAI or intensity.", call. = FALSE)
  list(expression_matrix = build_expression_matrix(df, id_col, abundance_cols), note = "Parsed Mascot CSV/export table using protein accession and numeric quantitation columns.")
}

parse_openms_output <- function(df) {
  id_col <- first_matching_column(names(df), c("ProteinName", "protein_accession", "protein accession", "accession", "Protein", "id"))
  abundance_cols <- names(df)[grepl("intensity|abundance|area|map_", names(df), ignore.case = TRUE)]
  abundance_cols <- numeric_column_names(df, exclude = id_col)[numeric_column_names(df, exclude = id_col) %in% abundance_cols]
  if (is.null(id_col) || length(abundance_cols) == 0) stop("OpenMS consensus/ProteinQuantifier tables require a protein accession column and intensity/abundance columns.", call. = FALSE)
  list(expression_matrix = build_expression_matrix(df, id_col, abundance_cols), note = "Parsed OpenMS consensus/ProteinQuantifier tabular output using accession and intensity/abundance columns.")
}

render_source_boxplot <- function(expression_matrix) {
  long <- tidyr::pivot_longer(expression_matrix, cols = -ID, names_to = "Sample", values_to = "Intensity")
  ggplot2::ggplot(long, ggplot2::aes(x = Sample, y = log2(Intensity + 1))) +
    ggplot2::geom_boxplot(fill = "#60a5fa", alpha = 0.8, outlier.size = 0.7) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = "log2 intensity + 1", title = "Sample intensity distribution")
}

render_source_pca <- function(expression_matrix, sample_info) {
  mat <- as.matrix(expression_matrix[, -1, drop = FALSE])
  mat[is.na(mat)] <- 0
  if (ncol(mat) < 2 || nrow(mat) < 2) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "Need at least two samples and proteins for PCA."); return(invisible(NULL))
  }
  pca_input <- t(log2(mat + 1))
  variable_features <- apply(pca_input, 2, stats::sd, na.rm = TRUE) > 0
  pca_input <- pca_input[, variable_features, drop = FALSE]
  if (ncol(pca_input) < 2) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "Need at least two variable proteins for PCA."); return(invisible(NULL))
  }
  pca <- stats::prcomp(pca_input, scale. = TRUE)
  plot_df <- data.frame(maxquant_id = rownames(pca$x), PC1 = pca$x[, 1], PC2 = pca$x[, 2], stringsAsFactors = FALSE)
  plot_df <- merge(plot_df, sample_info, by = "maxquant_id", all.x = TRUE)
  plot_df$Sample <- ifelse(is.na(plot_df$sample_id), plot_df$maxquant_id, plot_df$sample_id)
  plot_df$Group <- ifelse(is.na(plot_df$group), "Unknown", plot_df$group)
  print(ggplot2::ggplot(plot_df, ggplot2::aes(PC1, PC2, color = Group, label = Sample)) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_text(vjust = -0.7, size = 3) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::labs(title = "PCA overview of imported source data"))
}

register_tabular_data_source_server <- function(id, source_name, parser, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(raw = NULL, sample_info = NULL, expression_matrix = NULL, note = NULL)

    load_source <- function() {
      tryCatch({
        shiny::req(input$protein_file)
        raw <- read_proteomics_table(
          input$protein_file$datapath, filename = input$protein_file$name
        )
        parsed <- parser(raw)
        sample_info <- NULL
        if (!is.null(input$sample_info)) {
          sample_info <- read_proteomics_table(
            input$sample_info$datapath, filename = input$sample_info$name
          )
        }
        validated <- validate_protvis_data(
          parsed$expression_matrix, sample_info, source = source_name
        )
        parsed$expression_matrix <- validated$expression_matrix
        sample_info <- validated$sample_info
        rv$raw <- raw
        rv$expression_matrix <- parsed$expression_matrix
        rv$sample_info <- sample_info
        rv$note <- parsed$note
        if (!is.null(shared_state)) {
          shared_state$expression_matrix <- parsed$expression_matrix
          shared_state$expression_matrix_filtered <- parsed$expression_matrix
          shared_state$sample_info <- sample_info
          shared_state$data_source <- source_name
          if (!is.null(shared_state$workdir)) {
            expression_matrix <- parsed$expression_matrix
            expression_matrix_filtered <- parsed$expression_matrix
            data_source <- source_name
            step1_path <- file.path(shared_state$workdir, "Step1_project_init.rda")
            step2_path <- file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")
            source_path <- file.path(shared_state$workdir, paste0("Step1_", gsub("[^A-Za-z0-9]+", "_", source_name), "_import.rda"))
            base::save(sample_info, expression_matrix, data_source, file = step1_path)
            base::save(sample_info, expression_matrix, expression_matrix_filtered, file = step2_path)
            base::save(sample_info, expression_matrix, expression_matrix_filtered, data_source, file = source_path)
          }
        }
        shiny::showNotification(paste(source_name, "file parsed successfully."), type = "message")
        invisible(TRUE)
      }, error = function(e) {
        shiny::showNotification(
          paste0(source_name, " import failed: ", conditionMessage(e)),
          type = "error"
        )
        invisible(FALSE)
      })
    }

    shiny::observeEvent(input$load_data, load_source())
    shiny::observeEvent(input$protein_file, load_source(), ignoreInit = TRUE)
    shiny::observeEvent(input$sample_info, {
      shiny::req(rv$expression_matrix)
      sample_info <- read_proteomics_table(
        input$sample_info$datapath, filename = input$sample_info$name
      )
      rv$sample_info <- normalise_sample_info(sample_info, names(rv$expression_matrix)[-1])
      if (!is.null(shared_state)) {
        shared_state$sample_info <- rv$sample_info
        if (!is.null(shared_state$workdir)) {
          sample_info <- rv$sample_info
          expression_matrix <- rv$expression_matrix
          expression_matrix_filtered <- rv$expression_matrix
          data_source <- source_name
          base::save(sample_info, expression_matrix, data_source, file = file.path(shared_state$workdir, "Step1_project_init.rda"))
          base::save(sample_info, expression_matrix, expression_matrix_filtered, file = file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda"))
        }
      }
    }, ignoreInit = TRUE)

    output$group_select <- shiny::renderUI({
      if (is.null(rv$sample_info)) return(shiny::tags$span("Upload a source file to detect samples."))
      shiny::tags$div(
        shiny::strong("Detected groups"),
        DT::DTOutput(session$ns("sample_info_preview"))
      )
    })

    output$sample_info_preview <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(dom = "t", paging = FALSE))
    })

    output$de_table <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(pageLength = 10, scrollX = TRUE), caption = rv$note)
    })

    output$boxplot <- shiny::renderPlot({
      shiny::req(rv$expression_matrix)
      print(render_source_boxplot(rv$expression_matrix))
    })

    output$umap_plot <- shiny::renderPlot({
      shiny::req(rv$expression_matrix, rv$sample_info)
      render_source_pca(rv$expression_matrix, rv$sample_info)
    })

    output$heatmap_plot <- shiny::renderPlot({
      shiny::req(rv$expression_matrix)
      mat <- as.matrix(rv$expression_matrix[, -1, drop = FALSE])
      rownames(mat) <- rv$expression_matrix$ID
      mat[is.na(mat)] <- 0
      if (nrow(mat) < 2 || ncol(mat) < 2) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Need at least two proteins and samples for heatmap.")
        return(invisible(NULL))
      }
      top_rows <- order(rowSums(mat), decreasing = TRUE)[seq_len(min(50, nrow(mat)))]
      stats::heatmap(log2(mat[top_rows, , drop = FALSE] + 1), Colv = NA, scale = "row", main = paste(source_name, "top proteins"))
    })

    output$download_cleaned <- shiny::downloadHandler(
      filename = function() paste0(tolower(gsub("[^A-Za-z0-9]+", "_", source_name)), "_expression_matrix.csv"),
      content = function(file) utils::write.csv(rv$expression_matrix, file, row.names = FALSE)
    )

    output$download_diff <- shiny::downloadHandler(
      filename = function() paste0(tolower(gsub("[^A-Za-z0-9]+", "_", source_name)), "_sample_info.csv"),
      content = function(file) utils::write.csv(rv$sample_info, file, row.names = FALSE)
    )
  })
}
