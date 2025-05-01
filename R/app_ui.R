#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib nav_panel nav_menu page_navbar bs_theme
#' @importFrom bsicons bs_icon
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(bootswatch = "lumen"),  # 选择默认主题
      title = "ProtVis",
      bslib::nav_panel(
        "Homepage",
        icon = bs_icon("bank")
      ),
      bslib::nav_menu(
        "Data input",
        icon = bs_icon("usb-drive"),
        TMT_ui("TMT"),
        # label_free_ui("label_free"),
        # PD_ui("PD"),
        # skyline_ui("skyline")
      ),
      bslib::nav_menu(
        "Pre-processing",
        icon = bs_icon("wrench"),
        # h2("Choose your theme from the dropdown above."),
        uiOutput("theme_selector"),# 添加一个占位符用于主题选择
        mv_noise_ui("mv_noise"),
        mv_imputation_ui("mv_imputation"),
        mv_summary_ui("mv_summary"),
        # Align_batch_ui("Align_batch")
      ),
      bslib::nav_menu(
        "Downstream analysis",
        icon = bs_icon("tools"),
        DEP_analysis_ui("DEP_analysis"),
        DEP_visualize_ui("DEP_visualize"),
        veen_ui("veen"),
        protein_structure_ui("protein_structure"),
        GO_and_KEGG_ui("GO_and_KEGG"),
        # PPI_network_ui("PPI_network"),
        Expression_profile_ui("Expression_profile")
      ),
      bslib::nav_menu(
        "Multi-omics data",
        icon = bs_icon("database-gear"),
        DR_analysis_ui("DR_analysis"),
        # correlation_ui("correlation"),
        # pathway_analysis_ui("pathway_analysis")

      ),
      bslib::nav_panel(
        "Help",
        icon = bs_icon("exclamation-circle")
      )
      # flexible tools
      # footer = flexible_tools_ui("flexible_tools")
      # div(
      #   style = "position: fixed; bottom: 0; width: 100%;",
      #   flexible_tools_ui("flexible_tools")  # Include flexible footer UI here
      # )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProtVis"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
