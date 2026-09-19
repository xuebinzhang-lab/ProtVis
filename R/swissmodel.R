.protvis_swissmodel_demo_sequence <- paste0(
  "VLSPADKTNVKAAWAKVGNHAADFGAEALERMFMSFPSTKTYFSHFDLGHNSTQVKGHGKKV",
  "ADALTKAVGHLDTLPDALSDLSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPGDFTPSVHAS",
  "LDKFLASVSTVLTSKYR"
)

.PROTVIS_SWISS_BASE_URL <- "https://swissmodel.expasy.org"

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (length(x) == 1L && is.na(x))) y else x
}

.protvis_swiss_parse_sequences <- function(x) {
  x <- paste(x, collapse = "\n")
  x <- gsub("\r", "", x, fixed = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) return(character())

  lines <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
  if (any(grepl("^>", lines))) {
    out <- character()
    current <- character()
    for (line in lines) {
      if (!nzchar(line)) next
      if (grepl("^>", line)) {
        if (length(current)) {
          out <- c(out, paste(current, collapse = ""))
          current <- character()
        }
      } else {
        current <- c(current, line)
      }
    }
    if (length(current)) out <- c(out, paste(current, collapse = ""))
  } else {
    out <- strsplit(x, "\\n[[:space:]]*\\n|[;]+", perl = TRUE)[[1]]
  }

  out <- toupper(gsub("[[:space:]]+", "", out))
  out <- out[nzchar(out)]
  invalid <- vapply(
    out,
    function(s) grepl("[^ABCDEFGHIKLMNPQRSTUVWXYZ*.-]", s),
    logical(1)
  )
  if (any(invalid)) stop("Target sequence contains unsupported characters.", call. = FALSE)
  unname(out)
}

.protvis_swiss_target_payload <- function(sequences) {
  if (!length(sequences)) stop("At least one target protein sequence is required.", call. = FALSE)
  if (length(sequences) == 1L) sequences[[1]] else as.list(unname(sequences))
}

.protvis_swiss_json_text <- function(x) {
  if (is.null(x)) return("{}")
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
}

.protvis_swiss_api <- function(
  path,
  token = NULL,
  method = c("GET", "POST"),
  body = NULL,
  raw = FALSE,
  timeout = 120
) {
  method <- match.arg(method)
  url <- if (grepl("^https?://", path)) path else paste0(.PROTVIS_SWISS_BASE_URL, path)
  headers <- c(Accept = if (isTRUE(raw)) "application/octet-stream" else "application/json")
  if (!is.null(token) && nzchar(token)) headers <- c(headers, Authorization = paste("Token", token))
  header_call <- do.call(httr::add_headers, as.list(headers))

  response <- if (identical(method, "POST")) {
    httr::POST(url, header_call, httr::timeout(timeout), body = body, encode = "json")
  } else {
    httr::GET(url, header_call, httr::timeout(timeout))
  }

  status <- httr::status_code(response)
  if (status < 200L || status >= 300L) {
    text <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    if (status == 429L) {
      stop("SWISS-MODEL API rate limit reached. Please try again later.", call. = FALSE)
    }
    stop(
      sprintf("SWISS-MODEL API request failed (%s): %s", status, substr(text, 1, 1200)),
      call. = FALSE
    )
  }

  if (isTRUE(raw)) return(httr::content(response, as = "raw"))
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!nzchar(trimws(text))) return(list())
  jsonlite::fromJSON(text, simplifyVector = TRUE)
}

.protvis_swiss_submit <- function(
  mode,
  token,
  target_sequences,
  project_title = "ProtVis SWISS-MODEL",
  template_sequence = NULL,
  template_seqres_offset = 0,
  pdb_id = NULL,
  auth_asym_id = NULL,
  assembly_id = 1,
  template_coordinates = NULL
) {
  mode <- match.arg(mode, c("automodel", "alignment", "user_template"))
  target <- .protvis_swiss_target_payload(target_sequences)
  body <- switch(
    mode,
    automodel = list(target_sequences = target, project_title = project_title),
    alignment = list(
      target_sequences = target,
      template_sequence = template_sequence,
      template_seqres_offset = as.integer(template_seqres_offset),
      pdb_id = pdb_id,
      auth_asym_id = auth_asym_id,
      assembly_id = as.integer(assembly_id),
      project_title = project_title
    ),
    user_template = list(
      target_sequences = target,
      template_coordinates = template_coordinates,
      project_title = project_title
    )
  )
  result <- .protvis_swiss_api(paste0("/", mode, "/"), token, "POST", body)
  project_id <- result$project_id
  if (is.null(project_id) || !nzchar(as.character(project_id))) {
    stop("SWISS-MODEL accepted the request but no project_id was returned.", call. = FALSE)
  }
  as.character(project_id)
}

.protvis_swiss_project_summary <- function(project_id, token) {
  .protvis_swiss_api(paste0("/project/", project_id, "/models/summary/"), token)
}

.protvis_swiss_project_details <- function(project_id, token, model_id = NULL) {
  path <- if (is.null(model_id)) {
    paste0("/project/", project_id, "/models/full-details/")
  } else {
    paste0(
      "/project/", project_id, "/models/full-details/",
      utils::URLencode(as.character(model_id), reserved = TRUE)
    )
  }
  .protvis_swiss_api(path, token)
}

.protvis_swiss_list_projects <- function(token) {
  .protvis_swiss_api("/projects/", token)
}

.protvis_swiss_model_raw <- function(project_id, model_id, token, format = "pdb") {
  format <- match.arg(format, c("pdb", "cif"))
  .protvis_swiss_api(
    paste0(
      "/project/", project_id, "/models/",
      utils::URLencode(as.character(model_id), reserved = TRUE), ".", format
    ),
    token = token,
    raw = TRUE
  )
}

.protvis_swiss_wait <- function(
  project_id,
  token,
  wait_interval = 10,
  max_wait_time = 1800,
  progress_callback = NULL
) {
  started <- Sys.time()
  repeat {
    info <- .protvis_swiss_project_summary(project_id, token)
    status <- toupper(as.character(info$status %||% "UNKNOWN"))
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (is.function(progress_callback)) progress_callback(status, elapsed)
    if (status %in% c("COMPLETED", "FAILED")) return(info)
    if (elapsed >= max_wait_time) {
      stop(
        sprintf(
          "Timed out after %s seconds. Project %s can be loaded later using Existing project mode.",
          max_wait_time, project_id
        ),
        call. = FALSE
      )
    }
    Sys.sleep(max(1, wait_interval))
  }
}

.protvis_swiss_flatten <- function(x, path = "") {
  out <- list()
  add <- function(value, current_path) {
    if (!nzchar(current_path)) current_path <- "value"
    out[[length(out) + 1L]] <<- data.frame(
      path = current_path,
      value = if (!length(value)) "" else paste(as.character(value), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  walk <- function(obj, current_path) {
    if (is.null(obj)) {
      add(NA_character_, current_path)
    } else if (is.data.frame(obj)) {
      if (!nrow(obj)) {
        add("<empty data frame>", current_path)
      } else {
        for (i in seq_len(nrow(obj))) {
          walk(as.list(obj[i, , drop = FALSE]), paste0(current_path, if (nzchar(current_path)) "." else "", "[", i, "]"))
        }
      }
    } else if (is.list(obj)) {
      nm <- names(obj)
      if (is.null(nm)) nm <- rep("", length(obj))
      for (i in seq_along(obj)) {
        label <- nm[[i]]
        if (!nzchar(label)) label <- paste0("[", i, "]")
        walk(obj[[i]], if (nzchar(current_path)) paste(current_path, label, sep = ".") else label)
      }
    } else if (length(obj) <= 12L) {
      add(obj, current_path)
    } else {
      add(paste0(paste(head(as.character(obj), 6), collapse = ", "), " ... [n=", length(obj), "]"), current_path)
    }
    invisible(NULL)
  }
  walk(x, path)
  if (!length(out)) return(data.frame(path = character(), value = character()))
  do.call(rbind, out)
}

.protvis_swiss_record_table <- function(x) {
  if (is.null(x)) return(data.frame())
  normalize_df <- function(out) {
    for (name in names(out)) {
      if (is.list(out[[name]])) {
        out[[name]] <- vapply(
          out[[name]],
          function(value) {
            if (is.null(value)) NA_character_ else jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
          },
          character(1)
        )
      }
    }
    out
  }
  if (is.data.frame(x)) return(normalize_df(x))

  records <- x
  if (is.list(x) && !is.null(x$models)) records <- x$models
  if (is.list(x) && !is.null(x$projects)) records <- x$projects
  if (is.list(x) && !is.null(x$results)) records <- x$results
  if (is.data.frame(records)) return(normalize_df(records))
  if (!is.list(records) || !length(records)) return(.protvis_swiss_flatten(records))

  scalar_record <- !is.null(names(records)) && all(vapply(
    records,
    function(z) is.null(z) || (!is.list(z) && length(z) <= 1L),
    logical(1)
  ))
  if (scalar_record) records <- list(records)

  rows <- lapply(records, function(record) {
    if (!is.list(record)) return(data.frame(value = as.character(record), stringsAsFactors = FALSE))
    fields <- lapply(record, function(value) {
      if (is.null(value)) NA_character_
      else if (!is.list(value) && length(value) <= 1L) as.character(value)
      else jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
    })
    as.data.frame(fields, stringsAsFactors = FALSE, check.names = FALSE)
  })
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(row) {
    for (name in setdiff(cols, names(row))) row[[name]] <- NA_character_
    row[, cols, drop = FALSE]
  })
  do.call(rbind, rows)
}

.protvis_swiss_models_table <- function(summary) {
  table <- .protvis_swiss_record_table(summary$models)
  if (!nrow(table)) return(table)
  id_col <- intersect(c("model_id", "id", "model", "modelId"), names(table))
  if (!length(id_col)) {
    table$model_id <- sprintf("%02d", seq_len(nrow(table)))
  } else if (id_col[[1]] != "model_id") {
    table$model_id <- table[[id_col[[1]]]]
  }
  table$model_id <- as.character(table$model_id)
  table
}

.protvis_swiss_model_ids <- function(summary) {
  table <- .protvis_swiss_models_table(summary)
  if (!nrow(table)) return(character())
  unique(as.character(table$model_id))
}

.protvis_swiss_filter_fields <- function(x, pattern) {
  flat <- .protvis_swiss_flatten(x)
  if (!nrow(flat)) return(flat)
  flat[grepl(pattern, flat$path, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
}

.protvis_swiss_quality_fields <- function(summary, details) {
  .protvis_swiss_filter_fields(
    list(summary = summary, details = details),
    "gmqe|qsqe|qmean|quality|score|identity|coverage|resolution|method|sequence_similarity|similarity|local_quality|oligo"
  )
}

.protvis_swiss_template_fields <- function(details) {
  .protvis_swiss_filter_fields(
    details,
    "template|alignment|coverage|identity|similarity|pdb|chain|method|resolution|assembly|seqres"
  )
}

.protvis_swiss_complex_fields <- function(details) {
  .protvis_swiss_filter_fields(
    details,
    "oligo|quaternary|ligand|hetero|assembly|complex|interface|stoichiometry|membrane"
  )
}

.protvis_swiss_local_quality <- function(details) {
  candidates <- list()
  walk <- function(x, path = "") {
    if (is.null(x)) return(invisible(NULL))
    if (is.data.frame(x)) {
      for (name in names(x)) walk(x[[name]], if (nzchar(path)) paste(path, name, sep = ".") else name)
      return(invisible(NULL))
    }
    if (is.list(x)) {
      nm <- names(x)
      if (is.null(nm)) nm <- rep("", length(x))
      for (i in seq_along(x)) {
        label <- nm[[i]]
        if (!nzchar(label)) label <- paste0("[", i, "]")
        walk(x[[i]], if (nzchar(path)) paste(path, label, sep = ".") else label)
      }
      return(invisible(NULL))
    }
    values <- suppressWarnings(as.numeric(x))
    if (
      length(values) >= 10L &&
      sum(is.finite(values)) >= 10L &&
      grepl("qmean|quality|local.*score|score.*local", path, ignore.case = TRUE)
    ) {
      candidates[[length(candidates) + 1L]] <<- list(path = path, values = values)
    }
    invisible(NULL)
  }
  walk(details)
  if (!length(candidates)) return(NULL)
  preference <- vapply(candidates, function(z) {
    p <- tolower(z$path)
    if (grepl("qmean.*local|local.*qmean", p)) 3L else if (grepl("quality", p)) 2L else 1L
  }, integer(1))
  candidate <- candidates[[which.max(preference)]]
  data.frame(
    residue = seq_along(candidate$values),
    score = candidate$values,
    source = candidate$path,
    stringsAsFactors = FALSE
  )
}

.protvis_swiss_pdb_info <- function(pdb) {
  atom <- pdb$atom
  chain <- if ("chain" %in% names(atom)) atom$chain else rep("", nrow(atom))
  insert <- if ("insert" %in% names(atom)) atom$insert else rep("", nrow(atom))
  residue_key <- paste(chain, atom$resno, insert, sep = ":")
  chains <- unique(chain)
  chains <- chains[!is.na(chains) & nzchar(chains)]
  dimensions <- c(
    x = diff(range(atom$x, na.rm = TRUE)),
    y = diff(range(atom$y, na.rm = TRUE)),
    z = diff(range(atom$z, na.rm = TRUE))
  )
  list(
    atoms = nrow(atom),
    residues = length(unique(residue_key)),
    chains = paste(chains, collapse = ", "),
    dimension_x_A = unname(dimensions[["x"]]),
    dimension_y_A = unname(dimensions[["y"]]),
    dimension_z_A = unname(dimensions[["z"]])
  )
}

.protvis_swiss_residue_composition <- function(pdb) {
  atom <- pdb$atom
  chain <- if ("chain" %in% names(atom)) atom$chain else rep("", nrow(atom))
  insert <- if ("insert" %in% names(atom)) atom$insert else rep("", nrow(atom))
  key <- paste(chain, atom$resno, insert, sep = ":")
  residues <- atom$resid[!duplicated(key)]
  tab <- sort(table(residues), decreasing = TRUE)
  data.frame(residue = names(tab), count = as.numeric(tab), stringsAsFactors = FALSE)
}

.protvis_swiss_ramachandran <- function(pdb) {
  torsion <- bio3d::torsion.pdb(pdb)
  n <- min(length(torsion$phi), length(torsion$psi))
  if (!n) return(data.frame())
  stats::na.omit(data.frame(phi = torsion$phi[seq_len(n)], psi = torsion$psi[seq_len(n)]))
}

.protvis_swiss_model_viewer <- function(pdb_texts) {
  viewer <- r3dmol::r3dmol()
  for (text in pdb_texts) {
    if (!is.character(text) || !nzchar(text)) next
    viewer <- r3dmol::m_add_model(
      viewer,
      strsplit(text, "\n", fixed = TRUE)[[1]],
      format = "pdb"
    )
  }
  viewer <- r3dmol::m_set_style(
    viewer,
    style = r3dmol::m_style_cartoon(color = "spectrum")
  )
  r3dmol::m_zoom_to(viewer)
}

.protvis_swiss_bulk_download <- function(
  token,
  coordinates_type = "pdb",
  from_datetime = NULL,
  to_datetime = NULL,
  wait_interval = 5,
  max_wait_time = 900
) {
  coordinates_type <- match.arg(coordinates_type, c("pdb", "cif"))
  body <- list(coordinates_type = coordinates_type)
  if (!is.null(from_datetime) && nzchar(trimws(from_datetime))) body$from_datetime <- trimws(from_datetime)
  if (!is.null(to_datetime) && nzchar(trimws(to_datetime))) body$to_datetime <- trimws(to_datetime)
  job <- .protvis_swiss_api("/projects/download/", token, "POST", body)
  download_id <- job$download_id
  if (is.null(download_id)) stop("SWISS-MODEL did not return a bulk download_id.", call. = FALSE)
  started <- Sys.time()
  repeat {
    status <- .protvis_swiss_api(paste0("/projects/download/", download_id, "/"), token)
    state <- toupper(as.character(status$status %||% "UNKNOWN"))
    if (state == "COMPLETED") {
      if (is.null(status$download_url)) {
        stop("Bulk download completed but no download URL was returned.", call. = FALSE)
      }
      return(.protvis_swiss_api(status$download_url, token = token, raw = TRUE))
    }
    if (state == "FAILED") stop("SWISS-MODEL bulk download failed.", call. = FALSE)
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= max_wait_time) {
      stop("Timed out while preparing the SWISS-MODEL bulk download.", call. = FALSE)
    }
    Sys.sleep(wait_interval)
  }
}

.protvis_swiss_atlas_search <- function(query, token = NULL) {
  job <- .protvis_swiss_api(
    "/atlas/search/",
    token = token,
    method = "POST",
    body = list(query = query)
  )
  md5 <- job$md5 %||% job$query_md5 %||% job$id
  if (is.null(md5)) return(job)
  tryCatch(
    .protvis_swiss_api(paste0("/atlas/results/", md5, "/"), token = token),
    error = function(e) list(status = "SUBMITTED", md5 = md5, message = conditionMessage(e))
  )
}

#' SWISS-MODEL UI
#'
#' Interactive SWISS-MODEL workspace for modelling, model inspection,
#' project management, downloads, and Atlas search.
#'
#' @param id Shiny module id.
#' @return A Shiny UI object.
#' @export
swissmodel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      "#", ns("swiss_root"), " .swiss-note{font-size:12px;color:#65717d;line-height:1.45;}",
      "#", ns("swiss_root"), " .swiss-kpi{border:1px solid #e7ebef;border-radius:12px;padding:14px 16px;height:100%;}",
      "#", ns("swiss_root"), " .swiss-kpi-label{font-size:12px;color:#72808e;text-transform:uppercase;letter-spacing:.04em;}",
      "#", ns("swiss_root"), " .swiss-kpi-value{font-size:19px;font-weight:650;margin-top:4px;word-break:break-word;}",
      "#", ns("swiss_root"), " .swiss-section-note{padding:10px 12px;border-radius:10px;background:#f6f8fa;color:#59636e;font-size:12px;margin-bottom:10px;}"
    ))),
    shiny::div(
      id = ns("swiss_root"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 390,
          open = "open",
          gap = "10px",
          shiny::div(
            shiny::h4("SWISS-MODEL", style = "margin-bottom:4px;"),
            shiny::p(
              "Homology modelling, model inspection, project management and structure exploration in ProtVis.",
              class = "swiss-note"
            )
          ),
          bslib::accordion(
            open = c("Modelling", "Model selection"),
            bslib::accordion_panel(
              "Modelling",
              shiny::selectInput(
                ns("mode"),
                "Workflow",
                choices = c(
                  "Automated mode" = "automodel",
                  "Alignment mode" = "alignment",
                  "User template" = "user_template",
                  "Load existing project" = "existing"
                ),
                selected = "automodel"
              ),
              shiny::passwordInput(
                ns("api_token"),
                "SWISS-MODEL API token",
                placeholder = "Token is kept only in this Shiny session"
              ),
              shiny::div(
                class = "swiss-note",
                shiny::tags$a(
                  href = "https://swissmodel.expasy.org/account",
                  target = "_blank",
                  "SWISS-MODEL account / token"
                ),
                " · ",
                shiny::tags$a(
                  href = "https://swissmodel.expasy.org/coreapi/",
                  target = "_blank",
                  "REST API"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.mode != 'existing'",
                ns = ns,
                shiny::textInput(ns("project_title"), "Project title", value = "ProtVis SWISS-MODEL"),
                shiny::textAreaInput(
                  ns("sequence"),
                  "Target sequence(s)",
                  value = .protvis_swissmodel_demo_sequence,
                  rows = 8,
                  width = "100%",
                  placeholder = "Paste one sequence or multiple FASTA records for a heteromeric target."
                ),
                shiny::div(
                  class = "swiss-note",
                  "Multiple FASTA records are sent as a heteromeric target when supported by SWISS-MODEL."
                )
              ),
              shiny::conditionalPanel(
                condition = "input.mode == 'alignment'",
                ns = ns,
                shiny::textAreaInput(ns("template_sequence"), "Template sequence", rows = 5),
                bslib::layout_columns(
                  col_widths = c(4, 4, 4),
                  shiny::textInput(ns("pdb_id"), "PDB ID", placeholder = "e.g. 1abc"),
                  shiny::textInput(ns("chain_id"), "Chain", value = "A"),
                  shiny::numericInput(ns("assembly_id"), "Assembly", value = 1, min = 1)
                ),
                shiny::numericInput(ns("template_offset"), "Template SEQRES offset", value = 0, min = 0)
              ),
              shiny::conditionalPanel(
                condition = "input.mode == 'user_template'",
                ns = ns,
                shiny::fileInput(
                  ns("template_file"),
                  "Template coordinates (PDB)",
                  accept = c(".pdb", "chemical/x-pdb")
                ),
                shiny::div(
                  class = "swiss-note",
                  "The uploaded PDB coordinates are sent to the official user-template endpoint."
                )
              ),
              shiny::conditionalPanel(
                condition = "input.mode == 'existing'",
                ns = ns,
                shiny::textInput(ns("existing_project_id"), "Project ID", placeholder = "e.g. 25f307")
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::numericInput(ns("wait_interval"), "Check every (s)", value = 15, min = 5, max = 120),
                shiny::numericInput(ns("max_wait"), "Max wait (s)", value = 1800, min = 60, max = 7200)
              ),
              shiny::div(
                style = "display:flex;gap:8px;flex-wrap:wrap;",
                shiny::actionButton(
                  ns("run_model"), "Run / Load",
                  icon = bsicons::bs_icon("play-fill"),
                  class = "btn-primary"
                ),
                shiny::actionButton(ns("example_seq"), "Example", icon = bsicons::bs_icon("stars")),
                shiny::actionButton(ns("clear_all"), "Clear", icon = bsicons::bs_icon("x-circle"))
              )
            ),
            bslib::accordion_panel(
              "Model selection",
              shiny::selectInput(ns("model_id"), "Model", choices = character()),
              shiny::checkboxGroupInput(
                ns("compare_models"),
                "Models to overlay in 3D",
                choices = character()
              ),
              shiny::uiOutput(ns("project_link"))
            ),
            bslib::accordion_panel(
              "Project history",
              shiny::actionButton(
                ns("refresh_projects"), "Refresh my projects",
                icon = bsicons::bs_icon("arrow-clockwise")
              ),
              shiny::selectInput(ns("history_project_id"), "Project from history", choices = character()),
              shiny::actionButton(
                ns("load_history"), "Load selected project",
                icon = bsicons::bs_icon("folder2-open")
              )
            ),
            bslib::accordion_panel(
              "Atlas search",
              shiny::radioButtons(
                ns("atlas_type"), "Query type",
                choices = c("Protein sequence" = "sequence", "PDB text" = "pdb"),
                inline = TRUE
              ),
              shiny::textAreaInput(ns("atlas_query"), "Sequence / PDB query", rows = 6),
              shiny::actionButton(
                ns("run_atlas"), "Search SWISS-MODEL Atlas",
                icon = bsicons::bs_icon("search")
              )
            ),
            bslib::accordion_panel(
              "Downloads",
              shiny::div(
                style = "display:flex;flex-direction:column;gap:8px;",
                shiny::downloadButton(ns("download_pdb"), "Selected model · PDB"),
                shiny::downloadButton(ns("download_cif"), "Selected model · mmCIF"),
                shiny::downloadButton(ns("download_summary"), "Project summary · JSON"),
                shiny::downloadButton(ns("download_details"), "Full model details · JSON"),
                shiny::downloadButton(ns("download_project_zip"), "Current project bundle · ZIP")
              ),
              shiny::hr(),
              shiny::selectInput(
                ns("bulk_format"), "Bulk coordinate format",
                choices = c("PDB" = "pdb", "mmCIF" = "cif")
              ),
              shiny::textInput(ns("bulk_from"), "Created after (optional)", placeholder = "2026-09-01T00:00:00"),
              shiny::textInput(ns("bulk_to"), "Created before (optional)", placeholder = "2026-09-30T23:59:59"),
              shiny::downloadButton(ns("download_bulk"), "Bulk download · ZIP"),
              shiny::div(
                class = "swiss-note",
                "Bulk downloads use the official /projects/download/ API."
              )
            )
          )
        ),
        bslib::navset_card_tab(
          id = ns("result_tabs"),
          full_screen = TRUE,
          bslib::nav_panel(
            "Overview",
            shiny::uiOutput(ns("project_kpis")),
            shiny::br(),
            bslib::card(
              bslib::card_header("Models returned by SWISS-MODEL"),
              DT::DTOutput(ns("models_table"))
            ),
            shiny::br(),
            bslib::card(
              bslib::card_header("Website-only / interactive features"),
              shiny::div(
                class = "swiss-section-note",
                "Template-search candidate browsing, interactive template selection, and legacy DeepView Project Mode are not exposed as stable modelling REST endpoints. ProtVis displays all API-returned template/model details and keeps a direct link to the official project page for those web-only interactions."
              )
            )
          ),
          bslib::nav_panel(
            "Template & alignment",
            shiny::div(
              class = "swiss-section-note",
              "Template identifiers, coverage, sequence identity, experimental metadata and alignment fields parsed from full-details."
            ),
            DT::DTOutput(ns("template_table")),
            shiny::br(),
            shiny::verbatimTextOutput(ns("alignment_text"))
          ),
          bslib::nav_panel(
            "Quality",
            shiny::div(
              class = "swiss-section-note",
              "Official GMQE, QSQE and QMEAN/QMEANDisCo fields are shown when returned by SWISS-MODEL. Local bio3d diagnostics are kept separate."
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("SWISS-MODEL quality fields"),
                DT::DTOutput(ns("quality_table"))
              ),
              bslib::card(
                bslib::card_header("Local per-residue quality"),
                shiny::plotOutput(ns("local_quality_plot"), height = "350px")
              )
            )
          ),
          bslib::nav_panel(
            "Complex & ligands",
            shiny::div(
              class = "swiss-section-note",
              "Oligomeric state, assembly, interfaces, ligands/cofactors, heteromers and membrane-related fields are shown when present."
            ),
            DT::DTOutput(ns("complex_table"))
          ),
          bslib::nav_panel(
            "3D structure",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("Selected model"),
                r3dmol::r3dmolOutput(ns("pdb_plot"), height = "520px")
              ),
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("Multiple-model overlay"),
                r3dmol::r3dmolOutput(ns("comparison_plot"), height = "520px")
              )
            )
          ),
          bslib::nav_panel(
            "Structure diagnostics",
            bslib::layout_columns(
              col_widths = c(4, 8),
              bslib::card(
                bslib::card_header("PDB information"),
                DT::DTOutput(ns("pdb_information"))
              ),
              bslib::card(
                bslib::card_header("Ramachandran plot"),
                shiny::plotOutput(ns("ramachandran_plot"), height = "390px")
              )
            ),
            shiny::br(),
            bslib::card(
              bslib::card_header("Residue composition"),
              shiny::plotOutput(ns("residue_composition"), height = "360px")
            )
          ),
          bslib::nav_panel(
            "Project history",
            bslib::card(
              bslib::card_header("Projects available to this API token"),
              DT::DTOutput(ns("projects_table"))
            )
          ),
          bslib::nav_panel(
            "Atlas",
            shiny::div(
              class = "swiss-section-note",
              "SWISS-MODEL Atlas accepts a protein sequence or PDB structure string; ProtVis displays the returned API result without altering it."
            ),
            DT::DTOutput(ns("atlas_table")),
            shiny::br(),
            shiny::verbatimTextOutput(ns("atlas_json"))
          ),
          bslib::nav_panel(
            "Raw API details",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("Project summary"),
                shiny::verbatimTextOutput(ns("summary_json"))
              ),
              bslib::card(
                bslib::card_header("Selected model full-details"),
                shiny::verbatimTextOutput(ns("details_json"))
              )
            )
          )
        )
      )
    )
  )
}

#' SWISS-MODEL server
#'
#' Server logic for the ProtVis SWISS-MODEL workspace.
#'
#' @param id Shiny module id.
#' @return A Shiny module server.
#' @export
swissmodel_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      token = NULL,
      project_id = NULL,
      summary = NULL,
      all_details = NULL,
      model_details = NULL,
      models = data.frame(),
      pdb = NULL,
      pdb_text = NULL,
      pdb_cache = list(),
      projects = data.frame(),
      atlas = NULL
    )

    reset_results <- function() {
      rv$project_id <- NULL
      rv$summary <- NULL
      rv$all_details <- NULL
      rv$model_details <- NULL
      rv$models <- data.frame()
      rv$pdb <- NULL
      rv$pdb_text <- NULL
      rv$pdb_cache <- list()
      rv$atlas <- NULL
      shiny::updateSelectInput(session, "model_id", choices = character())
      shiny::updateCheckboxGroupInput(
        session, "compare_models", choices = character(), selected = character()
      )
    }

    token_required <- function() {
      token <- trimws(input$api_token %||% "")
      if (!nzchar(token)) stop("Please enter a SWISS-MODEL API token.", call. = FALSE)
      token
    }

    get_pdb_text <- function(model_id) {
      key <- as.character(model_id)
      cached <- rv$pdb_cache[[key]]
      if (!is.null(cached)) return(cached)
      if (is.null(rv$project_id) || is.null(rv$token)) stop("No active SWISS-MODEL project.")
      text <- rawToChar(.protvis_swiss_model_raw(rv$project_id, key, rv$token, "pdb"))
      rv$pdb_cache[[key]] <- text
      text
    }

    read_pdb_text <- function(text) {
      path <- tempfile(fileext = ".pdb")
      on.exit(unlink(path), add = TRUE)
      writeLines(text, path, useBytes = TRUE)
      bio3d::read.pdb(path, verbose = FALSE)
    }

    load_selected_model <- function(model_id) {
      if (is.null(model_id) || !nzchar(model_id)) return(invisible(NULL))
      rv$model_details <- tryCatch(
        .protvis_swiss_project_details(rv$project_id, rv$token, model_id),
        error = function(e) list(warning = paste("Could not load model full-details:", conditionMessage(e)))
      )
      rv$pdb_text <- get_pdb_text(model_id)
      rv$pdb <- tryCatch(read_pdb_text(rv$pdb_text), error = function(e) NULL)
      invisible(NULL)
    }

    load_project <- function(project_id, token, supplied_summary = NULL) {
      summary <- supplied_summary %||% .protvis_swiss_project_summary(project_id, token)
      all_details <- tryCatch(
        .protvis_swiss_project_details(project_id, token),
        error = function(e) list(warning = paste("Could not load project full-details:", conditionMessage(e)))
      )
      models <- .protvis_swiss_models_table(summary)
      ids <- .protvis_swiss_model_ids(summary)
      rv$token <- token
      rv$project_id <- project_id
      rv$summary <- summary
      rv$all_details <- all_details
      rv$models <- models
      rv$pdb_cache <- list()
      shiny::updateSelectInput(
        session, "model_id", choices = ids,
        selected = if (length(ids)) ids[[1]] else character()
      )
      shiny::updateCheckboxGroupInput(
        session, "compare_models", choices = ids,
        selected = head(ids, min(3L, length(ids)))
      )
      if (length(ids)) load_selected_model(ids[[1]])
      invisible(summary)
    }

    refresh_projects <- function(token) {
      raw_projects <- .protvis_swiss_list_projects(token)
      projects <- .protvis_swiss_record_table(raw_projects)
      rv$projects <- projects
      id_col <- intersect(c("project_id", "id", "project"), names(projects))
      ids <- if (length(id_col)) unique(as.character(projects[[id_col[[1]]]])) else character()
      ids <- ids[!is.na(ids) & nzchar(ids)]
      shiny::updateSelectInput(
        session, "history_project_id", choices = ids,
        selected = if (length(ids)) ids[[1]] else character()
      )
      invisible(projects)
    }

    shiny::observeEvent(input$example_seq, {
      shiny::updateTextAreaInput(session, "sequence", value = .protvis_swissmodel_demo_sequence)
      shiny::showNotification("Example protein sequence loaded.", type = "message", duration = 2)
    })

    shiny::observeEvent(input$clear_all, {
      shiny::updateTextAreaInput(session, "sequence", value = "")
      shiny::updateTextInput(session, "project_title", value = "ProtVis SWISS-MODEL")
      shiny::updateTextInput(session, "existing_project_id", value = "")
      shiny::updateTextAreaInput(session, "template_sequence", value = "")
      shiny::updateTextInput(session, "pdb_id", value = "")
      shiny::updateTextInput(session, "chain_id", value = "A")
      shiny::updateTextAreaInput(session, "atlas_query", value = "")
      reset_results()
      shiny::showNotification("SWISS-MODEL inputs and results cleared.", type = "message", duration = 2)
    })

    shiny::observeEvent(input$run_model, {
      tryCatch({
        token <- token_required()
        mode <- input$mode
        shiny::withProgress(message = "SWISS-MODEL", value = 0, {
          if (identical(mode, "existing")) {
            project_id <- trimws(input$existing_project_id %||% "")
            if (!nzchar(project_id)) stop("Please enter an existing SWISS-MODEL project ID.")
            shiny::setProgress(0.25, detail = "Loading existing project...")
            load_project(project_id, token)
            shiny::setProgress(1, detail = "Project loaded.")
          } else {
            sequences <- .protvis_swiss_parse_sequences(input$sequence)
            if (!length(sequences)) stop("Please enter at least one target protein sequence.")
            template_sequence <- NULL
            pdb_id <- NULL
            chain_id <- NULL
            template_coordinates <- NULL

            if (identical(mode, "alignment")) {
              template_sequence <- .protvis_swiss_parse_sequences(input$template_sequence)
              if (length(template_sequence) != 1L) stop("Alignment mode requires one template sequence.")
              template_sequence <- template_sequence[[1]]
              pdb_id <- trimws(input$pdb_id %||% "")
              chain_id <- trimws(input$chain_id %||% "")
              if (!nzchar(pdb_id) || !nzchar(chain_id)) {
                stop("Alignment mode requires both PDB ID and chain ID.")
              }
            }

            if (identical(mode, "user_template")) {
              file <- input$template_file
              if (is.null(file) || !nrow(file)) stop("Please upload a PDB template file.")
              if (!grepl("\\.pdb$", file$name, ignore.case = TRUE)) {
                stop("User-template mode currently accepts PDB coordinate files.")
              }
              template_coordinates <- paste(readLines(file$datapath, warn = FALSE), collapse = "\n")
              if (!grepl("(^|\\n)(ATOM|HETATM)", template_coordinates, perl = TRUE)) {
                stop("The uploaded file does not appear to contain PDB coordinates.")
              }
            }

            shiny::setProgress(0.08, detail = "Submitting modelling request...")
            project_id <- .protvis_swiss_submit(
              mode = mode,
              token = token,
              target_sequences = sequences,
              project_title = trimws(input$project_title %||% "ProtVis SWISS-MODEL"),
              template_sequence = template_sequence,
              template_seqres_offset = input$template_offset %||% 0,
              pdb_id = pdb_id,
              auth_asym_id = chain_id,
              assembly_id = input$assembly_id %||% 1,
              template_coordinates = template_coordinates
            )
            rv$project_id <- project_id
            rv$token <- token
            summary <- .protvis_swiss_wait(
              project_id,
              token,
              wait_interval = input$wait_interval %||% 15,
              max_wait_time = input$max_wait %||% 1800,
              progress_callback = function(status, elapsed) {
                fraction <- min(0.88, 0.12 + 0.70 * elapsed / max(60, input$max_wait %||% 1800))
                shiny::setProgress(fraction, detail = paste0("Project ", status, " · ", round(elapsed), " s"))
              }
            )
            shiny::setProgress(0.9, detail = "Loading model details...")
            load_project(project_id, token, supplied_summary = summary)
            shiny::setProgress(1, detail = "Complete.")
          }
        })
        shiny::showNotification(
          paste("SWISS-MODEL project loaded:", rv$project_id),
          type = "message", duration = 4
        )
        model_tables <- list(models = rv$models)
        model_tables <- model_tables[
          vapply(model_tables, is.data.frame, logical(1))
        ]
        .protvis_record_shared_run(
          shared_state,
          module = "swissmodel",
          method = input$mode %||% "auto",
          category = "protein_structure",
          parameters = list(
            mode = input$mode,
            project_title = input$project_title,
            template_offset = input$template_offset,
            pdb_id = input$pdb_id,
            chain_id = input$chain_id,
            assembly_id = input$assembly_id
          ),
          tables = model_tables,
          statistics = list(
            project_id = rv$project_id,
            summary = rv$summary,
            selected_model = input$model_id %||% NA_character_,
            model_details = rv$model_details
          ),
          plot_config = list(
            compare_models = input$compare_models %||% character()
          )
        )
      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "SWISS-MODEL error",
          conditionMessage(e),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    shiny::observeEvent(input$model_id, {
      if (is.null(rv$project_id) || is.null(rv$token) || is.null(input$model_id) || !nzchar(input$model_id)) return()
      tryCatch(
        load_selected_model(input$model_id),
        error = function(e) shiny::showNotification(
          paste("Could not load selected model:", conditionMessage(e)),
          type = "error", duration = 5
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh_projects, {
      tryCatch({
        refresh_projects(token_required())
        shiny::showNotification(
          paste("Project list refreshed:", nrow(rv$projects), "records."),
          type = "message", duration = 3
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 5))
    })

    shiny::observeEvent(input$load_history, {
      tryCatch({
        project_id <- input$history_project_id %||% ""
        if (!nzchar(project_id)) stop("Select a project from history first.")
        load_project(project_id, token_required())
        shiny::showNotification(paste("Project loaded:", project_id), type = "message", duration = 3)
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 5))
    })

    shiny::observeEvent(input$run_atlas, {
      tryCatch({
        query <- trimws(input$atlas_query %||% "")
        if (!nzchar(query)) stop("Enter a protein sequence or PDB structure for Atlas search.")
        token <- trimws(input$api_token %||% "")
        if (!nzchar(token)) token <- NULL
        rv$atlas <- .protvis_swiss_atlas_search(query, token)
        .protvis_record_shared_run(
          shared_state,
          module = "swissmodel_atlas",
          method = "Atlas_search",
          category = "protein_structure",
          parameters = list(query_length = nchar(query)),
          statistics = list(result = rv$atlas)
        )
        shiny::showNotification("SWISS-MODEL Atlas request completed.", type = "message", duration = 3)
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 5))
    })

    output$project_link <- shiny::renderUI({
      if (is.null(rv$project_id)) return(shiny::div(class = "swiss-note", "No project loaded."))
      url <- rv$summary$view_url %||% paste0(.PROTVIS_SWISS_BASE_URL, "/project/", rv$project_id, "/view")
      shiny::tags$a(
        href = url,
        target = "_blank",
        class = "btn btn-outline-primary btn-sm",
        "Open project on SWISS-MODEL"
      )
    })

    output$project_kpis <- shiny::renderUI({
      summary <- rv$summary
      if (is.null(summary)) {
        return(shiny::div(
          class = "swiss-section-note",
          "Run a modelling workflow or load an existing project to inspect SWISS-MODEL results."
        ))
      }
      boxes <- list(
        c("Project ID", rv$project_id),
        c("Status", as.character(summary$status %||% "Unknown")),
        c("Models", nrow(rv$models)),
        c("Created", as.character(summary$date_created %||% summary$created_at %||% "—"))
      )
      nodes <- lapply(boxes, function(item) {
        shiny::div(
          class = "swiss-kpi",
          shiny::div(class = "swiss-kpi-label", item[[1]]),
          shiny::div(class = "swiss-kpi-value", item[[2]])
        )
      })
      layout <- do.call(bslib::layout_columns, c(nodes, list(col_widths = c(3, 3, 3, 3))))
      shiny::tagList(
        layout,
        shiny::div(
          class = "swiss-note", style = "margin-top:10px;",
          shiny::strong("Project title: "),
          as.character(summary$project_title %||% summary$title %||% "—")
        )
      )
    })

    output$models_table <- DT::renderDT({
      table <- rv$models
      if (!nrow(table)) table <- data.frame(Message = "No model table available yet.")
      DT::datatable(
        table, rownames = FALSE,
        filter = if (nrow(table) > 1L) "top" else "none",
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$template_table <- DT::renderDT({
      table <- .protvis_swiss_template_fields(rv$model_details)
      if (!nrow(table)) table <- data.frame(Message = "No template/alignment fields were returned for the selected model.")
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$alignment_text <- shiny::renderText({
      table <- .protvis_swiss_filter_fields(
        rv$model_details,
        "alignment|target.*sequence|template.*sequence"
      )
      if (!nrow(table)) return("No explicit alignment text was returned by the API for this model.")
      paste(paste0(table$path, "\n", table$value), collapse = "\n\n")
    })

    output$quality_table <- DT::renderDT({
      table <- .protvis_swiss_quality_fields(rv$summary, rv$model_details)
      if (!nrow(table)) table <- data.frame(Message = "No quality fields were returned for the selected model.")
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$local_quality_plot <- shiny::renderPlot({
      local <- .protvis_swiss_local_quality(rv$model_details)
      if (is.null(local) || !nrow(local)) {
        plot.new()
        text(0.5, 0.5, "No per-residue quality vector was returned\nfor the selected model.")
        return(invisible(NULL))
      }
      ggplot2::ggplot(local, ggplot2::aes(x = residue, y = score)) +
        ggplot2::geom_line(linewidth = 0.65) +
        ggplot2::geom_hline(yintercept = 0.6, linetype = 2, linewidth = 0.4) +
        ggplot2::labs(x = "Residue", y = "Local quality score", subtitle = unique(local$source)) +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$complex_table <- DT::renderDT({
      table <- .protvis_swiss_complex_fields(rv$model_details)
      if (!nrow(table)) table <- data.frame(Message = "No oligomer/ligand/complex fields were returned for the selected model.")
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$pdb_plot <- r3dmol::renderR3dmol({
      if (is.null(rv$pdb_text)) return(r3dmol::r3dmol())
      .protvis_swiss_model_viewer(list(rv$pdb_text))
    })

    output$comparison_plot <- r3dmol::renderR3dmol({
      ids <- input$compare_models
      if (is.null(ids) || !length(ids) || is.null(rv$project_id)) return(r3dmol::r3dmol())
      texts <- lapply(ids, function(model_id) tryCatch(get_pdb_text(model_id), error = function(e) NULL))
      texts <- Filter(Negate(is.null), texts)
      if (!length(texts)) return(r3dmol::r3dmol())
      .protvis_swiss_model_viewer(texts)
    })

    output$pdb_information <- DT::renderDT({
      if (is.null(rv$pdb)) {
        table <- data.frame(Message = "No PDB structure loaded.")
      } else {
        info <- .protvis_swiss_pdb_info(rv$pdb)
        table <- data.frame(
          Metric = names(info),
          Value = unlist(info, use.names = FALSE),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", paging = FALSE))
    })

    output$ramachandran_plot <- shiny::renderPlot({
      if (is.null(rv$pdb)) {
        plot.new(); text(0.5, 0.5, "No PDB structure loaded."); return(invisible(NULL))
      }
      rama <- .protvis_swiss_ramachandran(rv$pdb)
      if (!nrow(rama)) {
        plot.new(); text(0.5, 0.5, "No backbone torsion angles available."); return(invisible(NULL))
      }
      ggplot2::ggplot(rama, ggplot2::aes(x = phi, y = psi)) +
        ggplot2::geom_point(alpha = 0.55, size = 1.15) +
        ggplot2::geom_density_2d(linewidth = 0.35, alpha = 0.45) +
        ggplot2::coord_cartesian(xlim = c(-180, 180), ylim = c(-180, 180)) +
        ggplot2::labs(x = expression(phi ~ "(degrees)"), y = expression(psi ~ "(degrees)")) +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$residue_composition <- shiny::renderPlot({
      if (is.null(rv$pdb)) {
        plot.new(); text(0.5, 0.5, "No PDB structure loaded."); return(invisible(NULL))
      }
      data <- .protvis_swiss_residue_composition(rv$pdb)
      ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(residue, -count), y = count)) +
        ggplot2::geom_col() +
        ggplot2::labs(x = "Residue", y = "Residue count") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$projects_table <- DT::renderDT({
      table <- rv$projects
      if (!nrow(table)) table <- data.frame(Message = "Use 'Refresh my projects' to query the official /projects/ endpoint.")
      DT::datatable(
        table, rownames = FALSE,
        filter = if (nrow(table) > 1L) "top" else "none",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$atlas_table <- DT::renderDT({
      table <- .protvis_swiss_record_table(rv$atlas)
      if (!nrow(table)) table <- data.frame(Message = "Run an Atlas search to display returned records.")
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$atlas_json <- shiny::renderText({
      if (is.null(rv$atlas)) return("No Atlas result yet.")
      .protvis_swiss_json_text(rv$atlas)
    })
    output$summary_json <- shiny::renderText({
      if (is.null(rv$summary)) return("No project summary yet.")
      .protvis_swiss_json_text(rv$summary)
    })
    output$details_json <- shiny::renderText({
      if (is.null(rv$model_details)) return("No model full-details yet.")
      .protvis_swiss_json_text(rv$model_details)
    })

    output$download_pdb <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_", rv$project_id %||% "project", "_", input$model_id %||% "model", ".pdb"),
      content = function(file) {
        if (is.null(rv$project_id) || is.null(input$model_id)) stop("No model selected.")
        writeBin(.protvis_swiss_model_raw(rv$project_id, input$model_id, rv$token, "pdb"), file)
      }
    )

    output$download_cif <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_", rv$project_id %||% "project", "_", input$model_id %||% "model", ".cif"),
      content = function(file) {
        if (is.null(rv$project_id) || is.null(input$model_id)) stop("No model selected.")
        writeBin(.protvis_swiss_model_raw(rv$project_id, input$model_id, rv$token, "cif"), file)
      }
    )

    output$download_summary <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_", rv$project_id %||% "project", "_summary.json"),
      content = function(file) {
        if (is.null(rv$summary)) stop("No project summary available.")
        jsonlite::write_json(rv$summary, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      }
    )

    output$download_details <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_", rv$project_id %||% "project", "_", input$model_id %||% "model", "_full_details.json"),
      content = function(file) {
        if (is.null(rv$model_details)) stop("No model details available.")
        jsonlite::write_json(rv$model_details, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      }
    )

    output$download_project_zip <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_", rv$project_id %||% "project", "_bundle.zip"),
      content = function(file) {
        if (is.null(rv$project_id) || is.null(rv$token)) stop("No SWISS-MODEL project loaded.")
        ids <- .protvis_swiss_model_ids(rv$summary)
        if (!length(ids)) stop("No model coordinates are available.")
        dir <- tempfile("swissmodel_bundle_")
        dir.create(dir, recursive = TRUE)
        on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
        jsonlite::write_json(
          rv$summary,
          file.path(dir, "project_summary.json"),
          pretty = TRUE, auto_unbox = TRUE, null = "null"
        )
        jsonlite::write_json(
          rv$all_details,
          file.path(dir, "project_full_details.json"),
          pretty = TRUE, auto_unbox = TRUE, null = "null"
        )
        for (model_id in ids) {
          for (format in c("pdb", "cif")) {
            raw_value <- tryCatch(
              .protvis_swiss_model_raw(rv$project_id, model_id, rv$token, format),
              error = function(e) NULL
            )
            if (!is.null(raw_value)) writeBin(raw_value, file.path(dir, paste0("model_", model_id, ".", format)))
          }
        }
        old <- setwd(dir)
        on.exit(setwd(old), add = TRUE)
        utils::zip(zipfile = file, files = list.files(".", all.files = FALSE))
      }
    )

    output$download_bulk <- shiny::downloadHandler(
      filename = function() paste0("swissmodel_bulk_", input$bulk_format %||% "pdb", ".zip"),
      content = function(file) {
        raw_zip <- .protvis_swiss_bulk_download(
          token_required(),
          coordinates_type = input$bulk_format %||% "pdb",
          from_datetime = input$bulk_from,
          to_datetime = input$bulk_to
        )
        writeBin(raw_zip, file)
      }
    )
  })
}
