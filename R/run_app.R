#' Run the Shiny Application
#'
#' @description Launches the ProtVis Shiny application for proteomics data analysis.
#'
#' @param onStart A function that will be called before the app is actually run.
#' @param options Named list of values passed to `shiny::shinyOptions`.
#' @param enableBookmarking Can be "url", "server", or "disable".
#' @param uiPattern A regular expression used to determine which requests should be handled by the UI.
#' @param ... Arguments to pass to `golem_opts`. See `?golem::get_golem_options` for more details.
#'
#' @return An object that represents the app.
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' \dontrun{
#' # Run the app with default settings
#' run_ProtVis()
#'
#' # Run the app on a specific port
#' run_ProtVis(options = list(port = 3838))
#' }

run_ProtVis <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {

  # Set default shiny options (800MB limit)
  # modifyList ensures that user-defined options are preserved
  default_options <- list(shiny.maxRequestSize = 800 * 1024^2)
  combined_options <- utils::modifyList(default_options, options)

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = combined_options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
