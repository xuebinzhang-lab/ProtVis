#' Add External Resources to the Shiny Application
#'
#' This internal function adds external resources such as CSS, JS,
#' and favicon to the Shiny app. It also sets up resource paths
#' for static files within the `app/www` directory.
#'
#' @import shiny
#' @importFrom golem favicon bundle_resources
#' @name golem_add_external_resources
#' @export
#'
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProtVis"
    ),
    shiny::tags$style(shiny::HTML("
      body {
        padding-bottom: 260px;
      }

      .protvis-page {
        max-width: 1450px;
        margin: 0 auto;
        padding-bottom: 80px;
      }

      .protvis-hero {
        background: linear-gradient(135deg, #0b1220 0%, #163d7a 42%, #2563eb 100%);
        color: white;
        border-radius: 28px;
        padding: 54px 48px;
        margin: 24px auto 28px auto;
        box-shadow: 0 14px 38px rgba(0, 0, 0, 0.18);
        position: relative;
        overflow: hidden;
      }

      .protvis-hero::before {
        content: '';
        position: absolute;
        right: -80px;
        top: -80px;
        width: 260px;
        height: 260px;
        background: rgba(255, 255, 255, 0.08);
        border-radius: 50%;
      }

      .protvis-hero::after {
        content: '';
        position: absolute;
        right: 100px;
        bottom: -70px;
        width: 180px;
        height: 180px;
        background: rgba(255, 255, 255, 0.06);
        border-radius: 50%;
      }

      .protvis-kicker {
        font-size: 0.95rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        font-weight: 700;
        opacity: 0.92;
        margin-bottom: 16px;
      }

      .protvis-title {
        font-size: 3.2rem;
        font-weight: 800;
        line-height: 1.05;
        margin-bottom: 14px;
      }

      .protvis-subtitle {
        font-size: 1.12rem;
        line-height: 1.85;
        opacity: 0.97;
        max-width: 920px;
        margin-bottom: 0;
      }

      .protvis-badge-wrap {
        margin-top: 22px;
        margin-bottom: 0;
      }

      .protvis-badge {
        display: inline-block;
        background: rgba(255, 255, 255, 0.12);
        color: white;
        border: 1px solid rgba(255, 255, 255, 0.20);
        border-radius: 999px;
        padding: 7px 15px;
        margin-right: 10px;
        margin-bottom: 10px;
        font-size: 0.94rem;
      }

      .protvis-section-title {
        font-size: 1.55rem;
        font-weight: 800;
        margin-top: 10px;
        margin-bottom: 18px;
        color: #0f172a;
      }

      .protvis-card {
        background: white;
        border-radius: 20px;
        padding: 24px 22px;
        min-height: 210px;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08);
        border: 1px solid #e5e7eb;
        margin-bottom: 18px;
      }

      .protvis-card h4 {
        font-size: 1.12rem;
        font-weight: 800;
        margin-bottom: 12px;
        color: #1d4ed8;
      }

      .protvis-card p,
      .protvis-card li {
        color: #374151;
        line-height: 1.78;
        font-size: 0.98rem;
      }

      .protvis-card ul {
        padding-left: 20px;
        margin-bottom: 0;
      }

      .protvis-flow {
        background: #f8fafc;
        border-radius: 18px;
        padding: 20px 18px;
        border: 1px solid #e5e7eb;
        height: 100%;
        box-shadow: 0 4px 12px rgba(15, 23, 42, 0.04);
      }

      .protvis-flow-step {
        font-weight: 800;
        color: #1d4ed8;
        margin-bottom: 8px;
        font-size: 1rem;
      }

      .protvis-support {
        background: linear-gradient(180deg, #eff6ff 0%, #f8fbff 100%);
        border: 1px solid #bfdbfe;
        border-radius: 18px;
        padding: 18px 22px;
        margin-bottom: 24px;
      }

      .protvis-support ul {
        margin-bottom: 0;
        padding-left: 20px;
      }

      .protvis-support li {
        line-height: 1.8;
        color: #334155;
      }

      .protvis-footer-note {
        color: #6b7280;
        font-size: 0.95rem;
        margin-top: 10px;
        line-height: 1.8;
      }

      .protvis-info-panel {
        background: white;
        border-radius: 20px;
        padding: 24px 24px;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08);
        border: 1px solid #e5e7eb;
        margin-bottom: 24px;
      }

      .protvis-info-panel h4 {
        color: #0f172a;
        font-weight: 800;
        margin-bottom: 14px;
      }

      .protvis-info-panel p,
      .protvis-info-panel li {
        color: #374151;
        line-height: 1.8;
      }

      .protvis-bottom-row {
        margin-bottom: 80px;
      }

      .protvis-site-footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        z-index: 1050;
        background: #0f172a;
        color: rgba(255, 255, 255, 0.92);
        padding: 18px 0 12px 0;
        border-top: 1px solid rgba(255, 255, 255, 0.08);
        box-shadow: 0 -6px 20px rgba(0, 0, 0, 0.18);
      }

      .protvis-site-footer .footer-inner {
        max-width: 1450px;
        margin: 0 auto;
        padding: 0 18px;
      }

      .protvis-site-footer .footer-title {
        font-size: 1rem;
        font-weight: 800;
        margin-bottom: 6px;
      }

      .protvis-site-footer .footer-text {
        font-size: 0.90rem;
        line-height: 1.7;
        color: rgba(255, 255, 255, 0.80);
        margin-bottom: 0;
      }

      .protvis-site-footer .footer-small {
        margin-top: 10px;
        padding-top: 10px;
        border-top: 1px solid rgba(255, 255, 255, 0.10);
        font-size: 0.84rem;
        color: rgba(255, 255, 255, 0.68);
      }

      @media (max-width: 992px) {
        body {
          padding-bottom: 320px;
        }

        .protvis-page {
          padding-bottom: 100px;
        }

        .protvis-bottom-row {
          margin-bottom: 100px;
        }
      }

      @media (max-width: 768px) {
        body {
          padding-bottom: 380px;
        }

        .protvis-title {
          font-size: 2.4rem;
        }

        .protvis-hero {
          padding: 34px 24px;
        }

        .protvis-page {
          padding-bottom: 120px;
        }

        .protvis-bottom-row {
          margin-bottom: 120px;
        }
      }
    "))
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
#' @importFrom bslib nav_panel nav_menu page_navbar bs_theme layout_columns
#' @name app_ui
#' @export
#'
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),

    bslib::page_navbar(
      title = "ProtVis",
      theme = bslib::bs_theme(bootswatch = "lumen"),

      footer = shiny::tags$footer(
        class = "protvis-site-footer",
        shiny::div(
          class = "footer-inner",
          bslib::layout_columns(
            col_widths = c(5, 4, 3),

            shiny::div(
              shiny::div(class = "footer-title", "ProtVis"),
              shiny::p(
                class = "footer-text",
                "ProtVis is an integrated and user-friendly platform for proteomics data processing, statistical analysis, biological interpretation, multi-omics integration, and publication-ready visualization."
              )
            ),

            shiny::div(
              shiny::div(class = "footer-title", "Developer"),
              shiny::p(
                class = "footer-text",
                shiny::strong("Fei Liang"),
                shiny::br(),
                "State Key Laboratory of Crop Stress Adaptation and Improvement",
                shiny::br(),
                "Henan University"
              )
            ),

            shiny::div(
              shiny::div(class = "footer-title", "Contact"),
              shiny::p(
                class = "footer-text",
                "Email: fyliangfei@163.com",
                shiny::br(),
                "Platform: ProtVis",
                shiny::br(),
                "For research and visualization use"
              )
            )
          ),
          shiny::div(
            class = "footer-small",
            "© 2026 ProtVis. All rights reserved."
          )
        )
      ),

      #### Homepage ####
      bslib::nav_panel(
        "Homepage",
        icon = bsicons::bs_icon("house-door-fill"),
        shiny::div(
          class = "container-fluid protvis-page",

          shiny::div(
            class = "protvis-hero",
            shiny::div(
              class = "protvis-kicker",
              "Proteomics Analysis and Visualization Platform"
            ),
            shiny::div(
              class = "protvis-title",
              "ProtVis"
            ),
            shiny::p(
              class = "protvis-subtitle",
              "A publication-ready, modular, and user-friendly platform for comprehensive proteomics data processing, statistical analysis, biological interpretation, multi-omics integration, and high-quality figure generation."
            ),
            shiny::div(
              class = "protvis-badge-wrap",
              shiny::span(class = "protvis-badge", "One-stop workflow"),
              shiny::span(class = "protvis-badge", "Interactive exploration"),
              shiny::span(class = "protvis-badge", "Multi-source compatibility"),
              shiny::span(class = "protvis-badge", "Publication-ready export"),
              shiny::span(class = "protvis-badge", "Modular architecture")
            )
          ),

          bslib::layout_columns(
            col_widths = c(8, 4),

            shiny::div(
              class = "protvis-info-panel",
              shiny::h4("Platform Overview"),
              shiny::p(
                "ProtVis was developed to address major challenges in proteomics analysis, including fragmented workflows, low tool integration, limited interactivity, and insufficient support for reproducible, publication-grade visualization. The platform integrates upstream preprocessing, statistical analysis, functional interpretation, and downstream figure generation into a unified Shiny-based interface."
              ),
              shiny::p(
                "It is designed to support both routine proteomics analysis and advanced data interpretation workflows, providing a streamlined environment for researchers with different levels of computational experience."
              )
            ),

            shiny::div(
              class = "protvis-info-panel",
              shiny::h4("At a Glance"),
              shiny::tags$ul(
                shiny::tags$li("Integrated end-to-end proteomics workflow"),
                shiny::tags$li("Support for multiple input formats"),
                shiny::tags$li("Interactive downstream analysis"),
                shiny::tags$li("Multi-omics extension modules"),
                shiny::tags$li("High-quality export for figures")
              )
            )
          ),

          shiny::h3(class = "protvis-section-title", "Core Highlights"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),

            shiny::div(
              class = "protvis-card",
              shiny::h4("Integrated Full Workflow"),
              shiny::p(
                "ProtVis connects project initialization, metadata management, data import, cleaning, transformation, imputation, normalization, exploratory analysis, DEP analysis, enrichment interpretation, and downstream visualization within one coherent environment."
              )
            ),

            shiny::div(
              class = "protvis-card",
              shiny::h4("Flexible Data Compatibility"),
              shiny::p(
                "The platform is compatible with multiple commonly used proteomics outputs, enabling users to analyze data generated from diverse software pipelines without rebuilding the workflow from scratch."
              )
            ),

            shiny::div(
              class = "protvis-card",
              shiny::h4("SCI-ready Figure Export"),
              shiny::p(
                "ProtVis supports clear and customizable visual outputs for heatmaps, volcano plots, dimensionality reduction, enrichment, pathway display, and multi-omics exploration, with export suitable for scientific publication and further editing."
              )
            )
          ),

          shiny::h3(class = "protvis-section-title", "Workflow Overview"),
          bslib::layout_columns(
            col_widths = c(2, 2, 2, 2, 2, 2),

            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 1"),
              shiny::p("Project setup and sample metadata configuration")
            ),
            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 2"),
              shiny::p("Proteomics data import from multiple supported formats")
            ),
            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 3"),
              shiny::p("Noise correction, transformation, imputation, and normalization")
            ),
            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 4"),
              shiny::p("Overview statistics, DEP analysis, and pattern discovery")
            ),
            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 5"),
              shiny::p("Functional enrichment, GSEA, and pathway-level interpretation")
            ),
            shiny::div(
              class = "protvis-flow",
              shiny::div(class = "protvis-flow-step", "Step 6"),
              shiny::p("Multi-omics integration and toolkit-based downstream applications")
            )
          ),

          bslib::layout_columns(
            class = "protvis-bottom-row",
            col_widths = c(6, 6),

            shiny::div(
              shiny::h3(class = "protvis-section-title", "Supported Data Types"),
              shiny::div(
                class = "protvis-support",
                shiny::tags$ul(
                  shiny::tags$li("Raw intensity-based quantitative proteomics tables"),
                  shiny::tags$li("MaxQuant output tables"),
                  shiny::tags$li("Proteome Discoverer output tables"),
                  shiny::tags$li("Skyline-based quantitative datasets"),
                  shiny::tags$li("Mascot and OpenMS compatible tabular inputs"),
                  shiny::tags$li("User-defined matrices for customized downstream visualization")
                )
              )
            ),

            shiny::div(
              shiny::h3(class = "protvis-section-title", "Get Started"),
              shiny::div(
                class = "protvis-card",
                shiny::p(
                  "Start with the ",
                  shiny::strong("Project init"),
                  " tab to initialize the project and configure metadata. Then proceed to ",
                  shiny::strong("Data input"),
                  " and the pre-processing modules before entering downstream analysis."
                ),
                shiny::p(
                  "The modular design of ProtVis allows users to complete analysis step by step while maintaining flexibility, transparency, and reproducibility."
                ),
                shiny::p(
                  class = "protvis-footer-note",
                  "Recommended navigation path: Project init → Data input → Pre-processing → Downstream analysis → Multi-omics / Toolkits."
                )
              )
            )
          )
        )
      ),

      #### Project Initialization ####
      bslib::nav_panel(
        "Project init",
        icon = bsicons::bs_icon("gear"),
        project_init_ui("project_init")
      ),

      #### Data Input ####
      bslib::nav_panel(
        "Data input",
        icon = bsicons::bs_icon("usb-drive"),
        data_input_ui("data_input")
      ),

      #### Pre-processing ####
      bslib::nav_menu(
        "Pre-processing",
        icon = bsicons::bs_icon("wrench"),
        bslib::nav_panel("Correct Noise", correct_noise_ui("correct_noise")),
        bslib::nav_panel("Data Transformed", data_transformed_ui("data_transformed")),
        bslib::nav_panel("Data Imputation", data_imputation_ui("data_imputation")),
        bslib::nav_panel("Data Normalization", data_normalization_ui("data_normalization"))
      ),

      #### Downstream Analysis ####
      bslib::nav_menu(
        "Downstream analysis",
        icon = bsicons::bs_icon("bar-chart-line"),
        bslib::nav_panel("Overview", overview_ui("overview")),
        bslib::nav_panel("DEP analysis", DEP_analysis_ui("DEP_analysis")),
        bslib::nav_panel(
          "Enrichment analysis",
          enrichment_analysis_ui("enrichment_analysis")
        ),
        bslib::nav_panel("GSEA analysis", gsea_ui("gsea")),
        bslib::nav_panel("Pathview", pathview_ui("pathview")),
        bslib::nav_panel("Protein function", protein_fun_ui("protein_fun"))
      ),

      #### Multi-omics Data ####
      bslib::nav_menu(
        "Multi-omics data",
        icon = bsicons::bs_icon("database-gear"),
        bslib::nav_panel(
          "Expression Profile",
          Expression_profile_ui("Expression_profile")
        ),
        bslib::nav_panel("Nine Quadrant", nine_quadrant_ui("nine")),
        bslib::nav_panel("Venn", venn_ui("venn"))
      ),

      #### PTM ####
      bslib::nav_menu(
        "PTM",
        icon = bsicons::bs_icon("layers"),
        bslib::nav_panel("PTM", PTM_ui("PTM"))
      ),

      #### Release Data ####
      bslib::nav_panel(
        "Release data",
        icon = bsicons::bs_icon("folder2-open"),
        release_data_ui("release_data1")
      ),

      #### Toolkits ####
      bslib::nav_menu(
        "Toolkits",
        icon = bsicons::bs_icon("tools"),
        bslib::nav_panel("Protein Extract", protein_extract_ui("protein_extract")),
        bslib::nav_panel("Background Make", background_make_ui("background_make")),
        bslib::nav_panel("Protein Links", protein_links_ui("prot_links")),
        bslib::nav_panel("Protein Structure", protein_structure_ui("protein_structure")),
        bslib::nav_panel("Boxplot", boxplot_module_ui("box1")),
        bslib::nav_panel("swissmodel", swissmodel_ui("swissmodel")),
        bslib::nav_panel(
          "Stacked Column Diagram",
          stacked_column_chart_ui("stacked_column_chart")
        ),
        bslib::nav_panel("DEG Analyse", DEG_ui("DEG"))
      ),

      #### Help ####
      bslib::nav_panel(
        "Help",
        icon = bsicons::bs_icon("question-circle"),
        shiny::div(
          style = "max-width: 1100px; margin: 30px auto; padding: 10px 20px;",
          shiny::h2("Help & Documentation", align = "center"),
          shiny::p(
            "ProtVis provides a modular framework for proteomics data analysis. Users are encouraged to proceed through the workflow sequentially: Project initialization → Data input → Pre-processing → Downstream analysis."
          ),
          shiny::tags$ul(
            shiny::tags$li("Use 'Project init' to define project metadata."),
            shiny::tags$li("Use 'Data input' to upload experimental and expression data."),
            shiny::tags$li("Use pre-processing modules to prepare high-quality matrices."),
            shiny::tags$li("Use downstream analysis modules for DEP, enrichment, GSEA, and pathway interpretation."),
            shiny::tags$li("Use Toolkits for additional visualization and utility functions.")
          ),
          shiny::br(),
          shiny::p(
            "This page can be further extended to include tutorials, workflow examples, FAQ content, and downloadable user documentation."
          )
        )
      )
    )
  )
}
