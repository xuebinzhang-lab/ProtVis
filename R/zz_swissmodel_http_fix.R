# SWISS-MODEL HTTP content-negotiation fix ---------------------------------
#
# SWISS-MODEL's JSON endpoints accept application/json, but coordinate and
# archive endpoints negotiate their own representation from the URL (.pdb,
# .cif, download URL, etc.). Sending Accept: application/octet-stream to those
# endpoints causes HTTP 406 ("Could not satisfy the request Accept header").
#
# This late-loaded definition intentionally replaces the request helper in
# swissmodel.R while keeping the public module API unchanged.

.protvis_swiss_request_headers <- function(token = NULL, raw = FALSE) {
  headers <- list()

  # Only constrain JSON API calls. For PDB/mmCIF/ZIP downloads, omit Accept
  # entirely and let SWISS-MODEL choose the representation from the endpoint.
  if (!isTRUE(raw)) {
    headers$Accept <- "application/json"
  }

  if (!is.null(token) && length(token) && !is.na(token[[1]]) && nzchar(token[[1]])) {
    headers$Authorization <- paste("Token", token[[1]])
  }

  headers
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

  headers <- .protvis_swiss_request_headers(token = token, raw = raw)
  header_call <- do.call(httr::add_headers, headers)

  response <- if (identical(method, "POST")) {
    httr::POST(
      url,
      header_call,
      httr::timeout(timeout),
      body = body,
      encode = "json"
    )
  } else {
    httr::GET(
      url,
      header_call,
      httr::timeout(timeout)
    )
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

    if (status == 406L && isTRUE(raw)) {
      stop(
        paste0(
          "SWISS-MODEL rejected the coordinate/archive representation (HTTP 406). ",
          "ProtVis did not send a restrictive Accept header; the remote endpoint may have changed. ",
          substr(text, 1, 800)
        ),
        call. = FALSE
      )
    }

    stop(
      sprintf("SWISS-MODEL API request failed (%s): %s", status, substr(text, 1, 1200)),
      call. = FALSE
    )
  }

  if (isTRUE(raw)) {
    return(httr::content(response, as = "raw"))
  }

  text <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!nzchar(trimws(text))) {
    return(list())
  }

  jsonlite::fromJSON(text, simplifyVector = TRUE)
}
