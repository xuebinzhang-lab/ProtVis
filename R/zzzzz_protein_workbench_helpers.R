# Defensive overrides for Protein Workbench records with sparse UniProt names.

.protvis_pw_first_name_value <- function(x) {
  if (base::is.null(x) || !base::length(x)) return(NULL)
  first <- x[[1]]
  value <- first$fullName$value %||% first$name$value %||% NULL
  if (base::is.null(value) || !base::length(value) || !base::nzchar(base::as.character(value[[1]]))) return(NULL)
  base::as.character(value[[1]])
}

.protvis_pw_protein_name <- function(entry) {
  if (base::is.null(entry)) return(NA_character_)
  description <- entry$proteinDescription %||% base::list()

  recommended <- description$recommendedName$fullName$value %||% NULL
  if (!base::is.null(recommended) && base::length(recommended) &&
      base::nzchar(base::as.character(recommended[[1]]))) {
    return(base::as.character(recommended[[1]]))
  }

  submitted <- .protvis_pw_first_name_value(description$submissionNames %||% base::list())
  if (!base::is.null(submitted)) return(submitted)

  alternative <- .protvis_pw_first_name_value(description$alternativeNames %||% base::list())
  if (!base::is.null(alternative)) return(alternative)

  base::as.character(entry$uniProtkbId %||% entry$primaryAccession %||% NA_character_)
}
