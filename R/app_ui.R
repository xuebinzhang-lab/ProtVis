#' Add External Resources to the Shiny Application
#'
#' This internal function adds external resources such as CSS, JS,
#' and favicon to the Shiny app. It also sets up resource paths
#' for static files within the `app/www` directory.
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
    favicon(),  # You may replace with a custom icon under 'www'
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProtVis"
    )
  )
}

#' Application User Interface
#'
#' Defines the overall UI layout for the Shiny application including
#' navigation bars, menus, and loading UI modules.
#'
#' @param request Internal parameter for {shiny}. DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib nav_panel nav_menu page_navbar bs_theme
#' @importFrom bsicons bs_icon
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    page_navbar(
      title = "ProtVis",
      theme = bs_theme(bootswatch = "lumen"),  # You can switch to other themes like flatly, minty, cosmo, etc.

      #### Homepage ####
      nav_panel(
        "Homepage",
        icon = bs_icon("bank"),
        div(
          style = "margin: 0 auto; width: 60%; padding-top: 30px;",
          h2("Welcome to ProtVis", align = "center"),
          p("ProtVis is a user-friendly and modular platform designed for comprehensive proteomics data processing and visualization.", align = "center"),
          p("It supports multiple data formats including Raw intensity, MaxQuant, Proteome Discoverer, Skyline, Mascot, and OpenMS.", align = "center"),

          br(),

          h4("Features"),
          tags$ul(
            tags$li("Flexible project initialization and metadata management"),
            tags$li("Support for multi-source proteomics data (MaxQuant, PD, Skyline, etc.)"),
            tags$li("Built-in data cleaning: contaminant filtering, missing value imputation, normalization"),
            tags$li("Interactive dimensionality reduction (PCA, UMAP)"),
            tags$li("Comprehensive differential expression analysis using limma"),
            tags$li("Functional enrichment analysis (GO, KEGG, Reactome)"),
            tags$li("Publication-ready visualizations")
          ),

          br(),

          h4("Workflow Overview"),
          tags$ol(
            tags$li("Project Initialization"),
            tags$li("Data Input and Filtering"),
            tags$li("Data Cleaning and Normalization"),
            tags$li("Dimensionality Reduction"),
            tags$li("Differential Expression Analysis"),
            tags$li("Functional Enrichment and Biological Interpretation")
          ),

          br(),

          h4("Get Started"),
          p("Use the tabs above to begin your analysis. You can start by uploading your sample metadata and selecting the data source under 'Data Input'.")
        )
      ),
      #### Project Initialization ####
      nav_panel(
        "Project init",
        icon = bs_icon("arrow-bar-right"),
        nav_panel("Project init", project_init_ui("project_init"))
      ),

      #### Data Input ####
      nav_panel(
        "Data input",
        icon = bs_icon("usb-drive"),
        data_input_ui("data_input")
      ),

      #### Pre-processing ####
      nav_menu(
        "Pre-processing",
        icon = bs_icon("wrench"),
        nav_panel("Correct Noise", correct_noise_ui("correct_noise")),
        nav_panel("Data Transformed", data_transformed_ui("data_transformed")),
        nav_panel("Data Imputation", data_imputation_ui("data_imputation")),
        nav_panel("Data Normalization", data_normalization_ui("data_normalization")),

        nav_panel("Missing Value", missing_value_ui("missing_value")),
        nav_panel("Noise Filter", mv_noise_ui("mv_noise")),
        nav_panel("Imputation", mv_imputation_ui("mv_imputation")),
        nav_panel("Summary", mv_summary_ui("mv_summary")),
        nav_panel("Dimension Reduction", Dimension_reduction_ui("Dimension_reduction"))
      ),


      #### Downstream Analysis ####
      nav_menu(
        "Downstream analysis",
        icon = bs_icon("tools"),
        DEP_analysis_ui("DEP_analysis"),
        DEP_visualize_ui("DEP_visualize"),
        veen_ui("veen"),
        protein_structure_ui("protein_structure"),
        GO_and_KEGG_ui("GO_and_KEGG"),
        Expression_profile_ui("Expression_profile")
      ),

      #### Multi-omics Data ####
      nav_menu(
        "Multi-omics data",
        icon = bs_icon("database-gear"),
        DR_analysis_ui("DR_analysis")
        # correlation_ui("correlation"),
        # pathway_analysis_ui("pathway_analysis")
      ),

      #### Help ####
      nav_panel(
        "Help",
        icon = bs_icon("exclamation-circle")
        # Embed markdown documents or FAQs here
      )

      # Footer or bottom tools not added yet; could add flexible_tools here if needed
    )
  )
}
