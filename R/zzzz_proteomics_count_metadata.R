# Preserve count-level proteomics metadata ---------------------------------
# The canonical importer historically retained gene/description only. DEqMS
# needs peptide/PSM evidence counts, so retain count-like protein annotations
# in variable_info without changing expression_data.

.protvis_count_metadata_columns <- function(data) {
  if (base::is.null(data) || !base::ncol(data)) return(base::character())
  pattern <- paste(
    c(
      "(^|[._ -])psms?($|[._ -])",
      "peptide[._ -]*count",
      "^peptides?$",
      "unique[._ -]*peptides?",
      "razor.*unique",
      "ms[./_-]*ms[._ -]*count",
      "spectra[._ -]*count",
      "spectral[._ -]*count",
      "sequence[._ -]*count"
    ),
    collapse = "|"
  )
  base::names(data)[base::grepl(pattern, base::names(data), ignore.case = TRUE, perl = TRUE)]
}

.protvis_variable_info_from_table <- function(data, id_col, protein_ids) {
  result <- base::data.frame(
    protein_id = protein_ids,
    accession = protein_ids,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (base::is.null(id_col)) return(result)

  gene_col <- .protvis_find_column(
    base::names(data), c("^gene$", "gene[ ._-]*name", "symbol")
  )
  description_col <- .protvis_find_column(
    base::names(data), c("^description$", "protein[ ._-]*name", "fasta[ ._-]*headers?")
  )
  raw_ids <- sub(";.*$", "", base::trimws(base::as.character(data[[id_col]])))
  index <- base::match(protein_ids, raw_ids)

  if (!base::is.null(gene_col)) result$gene <- base::as.character(data[[gene_col]])[index]
  if (!base::is.null(description_col)) {
    result$description <- base::as.character(data[[description_col]])[index]
  }

  count_cols <- .protvis_count_metadata_columns(data)
  for (column in count_cols) {
    values <- suppressWarnings(base::as.numeric(base::as.character(data[[column]])))
    result[[column]] <- values[index]
  }
  result
}
