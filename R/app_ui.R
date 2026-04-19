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
        background: #eef1f4;
        padding-bottom: 270px;
      }

      .protvis-page {
        max-width: 1450px;
        margin: 0 auto;
        padding: 10px 6px 100px 6px;
      }

      .protvis-hero {
        position: relative;
        overflow: hidden;
        border-radius: 30px;
        min-height: 720px;
        margin: 22px auto 26px auto;
        background:
          radial-gradient(circle at top right, rgba(255,255,255,0.10) 0, rgba(255,255,255,0.04) 20%, transparent 40%),
          radial-gradient(circle at bottom left, rgba(255,255,255,0.06) 0, transparent 28%),
          linear-gradient(135deg, #1f2937 0%, #4b5563 45%, #9ca3af 100%);
        color: white;
        box-shadow: 0 18px 42px rgba(31, 41, 55, 0.18);
      }

      .protvis-hero-inner {
        width: 100%;
        padding: 56px 52px 44px 52px;
      }

      .protvis-hero-top {
        display: flex;
        align-items: stretch;
        justify-content: space-between;
        gap: 36px;
      }

      .protvis-hero-content {
        flex: 1 1 58%;
        max-width: 760px;
        padding-top: 6px;
      }

      .protvis-hero-image-wrap {
        flex: 0 0 40%;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 360px;
      }

      .protvis-hero-image-card {
        width: 100%;
        max-width: 520px;
        min-height: 320px;
        background: transparent;
        border: none;
        box-shadow: none;
        backdrop-filter: none;
        overflow: visible;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0;
      }

      .protvis-hero-figure {
        width: 100%;
        height: auto;
        max-height: 100%;
        object-fit: contain;
        border-radius: 18px;
        display: block;
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.18);
      }

      .protvis-kicker {
        display: inline-block;
        font-size: 0.90rem;
        font-weight: 800;
        letter-spacing: 0.09em;
        text-transform: uppercase;
        color: rgba(255,255,255,0.88);
        margin-bottom: 18px;
      }

      .protvis-title {
        font-size: 3.5rem;
        font-weight: 900;
        line-height: 1.02;
        margin-bottom: 16px;
        letter-spacing: -0.02em;
      }

      .protvis-subtitle {
        font-size: 1.08rem;
        line-height: 1.9;
        max-width: 720px;
        color: rgba(255,255,255,0.94);
        margin-bottom: 24px;
      }

      .protvis-badge-wrap {
        margin-top: 6px;
      }

      .protvis-badge {
        display: inline-block;
        border-radius: 999px;
        padding: 8px 16px;
        margin-right: 10px;
        margin-bottom: 10px;
        background: rgba(255,255,255,0.10);
        border: 1px solid rgba(255,255,255,0.16);
        color: white;
        font-size: 0.94rem;
        font-weight: 600;
        backdrop-filter: blur(4px);
      }

      .protvis-stat-grid {
        margin-top: 34px;
      }

      .protvis-stat-card {
        background: rgba(255,255,255,0.10);
        border: 1px solid rgba(255,255,255,0.15);
        border-radius: 20px;
        padding: 18px 20px;
        min-height: 126px;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.06);
        backdrop-filter: blur(10px);
      }

      .protvis-stat-value {
        font-size: 2rem;
        font-weight: 900;
        line-height: 1.1;
        margin-bottom: 8px;
      }

      .protvis-stat-label {
        font-size: 0.95rem;
        font-weight: 700;
        color: rgba(255,255,255,0.94);
        margin-bottom: 6px;
      }

      .protvis-stat-note {
        font-size: 0.88rem;
        line-height: 1.7;
        color: rgba(255,255,255,0.82);
      }

      .protvis-section-title {
        font-size: 1.55rem;
        font-weight: 900;
        color: #111827;
        margin-top: 12px;
        margin-bottom: 18px;
      }

      .protvis-panel {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 22px;
        padding: 26px 26px;
        box-shadow: 0 10px 28px rgba(31, 41, 55, 0.06);
        margin-bottom: 22px;
      }

      .protvis-panel h4 {
        font-size: 1.18rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 14px;
      }

      .protvis-panel p,
      .protvis-panel li {
        color: #4b5563;
        line-height: 1.82;
        font-size: 0.98rem;
      }

      .protvis-panel ul {
        padding-left: 20px;
        margin-bottom: 0;
      }

      .protvis-feature-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 22px;
        padding: 24px 22px;
        min-height: 220px;
        box-shadow: 0 10px 24px rgba(31, 41, 55, 0.05);
        margin-bottom: 18px;
        transition: transform 0.18s ease, box-shadow 0.18s ease;
      }

      .protvis-feature-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 14px 30px rgba(31, 41, 55, 0.09);
      }

      .protvis-feature-icon {
        width: 48px;
        height: 48px;
        border-radius: 14px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #e5e7eb 0%, #f3f4f6 100%);
        color: #374151;
        font-size: 1.25rem;
        font-weight: 900;
        margin-bottom: 16px;
      }

      .protvis-feature-card h4 {
        font-size: 1.10rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 12px;
      }

      .protvis-feature-card p {
        font-size: 0.97rem;
        line-height: 1.8;
        color: #4b5563;
        margin-bottom: 0;
      }

      .protvis-flow-card {
        background: linear-gradient(180deg, #ffffff 0%, #f9fafb 100%);
        border: 1px solid #e5e7eb;
        border-radius: 20px;
        padding: 20px 18px;
        min-height: 180px;
        box-shadow: 0 8px 22px rgba(31, 41, 55, 0.05);
        position: relative;
      }

      .protvis-step-badge {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 38px;
        height: 38px;
        border-radius: 999px;
        background: #4b5563;
        color: white;
        font-size: 0.95rem;
        font-weight: 900;
        margin-bottom: 14px;
      }

      .protvis-flow-card h5 {
        font-size: 1rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 10px;
      }

      .protvis-flow-card p {
        font-size: 0.95rem;
        line-height: 1.75;
        color: #4b5563;
        margin-bottom: 0;
      }

      .protvis-support-box {
        background: linear-gradient(180deg, #f3f4f6 0%, #f9fafb 100%);
        border: 1px solid #d1d5db;
        border-radius: 20px;
        padding: 20px 22px;
        min-height: 100%;
      }

      .protvis-support-box ul {
        margin-bottom: 0;
        padding-left: 20px;
      }

      .protvis-support-box li {
        font-size: 0.97rem;
        line-height: 1.82;
        color: #374151;
      }

      .protvis-note {
        font-size: 0.95rem;
        color: #6b7280;
        line-height: 1.8;
        margin-top: 10px;
      }

      .protvis-home-bottom {
        margin-bottom: 90px;
      }

      .protvis-site-footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        z-index: 1050;
        background: #111827;
        color: rgba(255,255,255,0.92);
        padding: 18px 0 12px 0;
        border-top: 1px solid rgba(255,255,255,0.08);
        box-shadow: 0 -6px 20px rgba(0,0,0,0.18);
      }

      .protvis-site-footer .footer-inner {
        max-width: 1450px;
        margin: 0 auto;
        padding: 0 18px;
      }

      .protvis-site-footer .footer-title {
        font-size: 1rem;
        font-weight: 850;
        margin-bottom: 6px;
      }

      .protvis-site-footer .footer-text {
        font-size: 0.90rem;
        line-height: 1.7;
        color: rgba(255,255,255,0.80);
        margin-bottom: 0;
      }

      .protvis-site-footer .footer-small {
        margin-top: 10px;
        padding-top: 10px;
        border-top: 1px solid rgba(255,255,255,0.10);
        font-size: 0.84rem;
        color: rgba(255,255,255,0.68);
      }

      @media (max-width: 1200px) {
        .protvis-title {
          font-size: 3rem;
        }

        .protvis-hero-image-wrap {
          flex: 0 0 36%;
        }
      }

      @media (max-width: 992px) {
        body {
          padding-bottom: 320px;
        }

        .protvis-page {
          padding-bottom: 120px;
        }

        .protvis-home-bottom {
          margin-bottom: 120px;
        }

        .protvis-hero {
          min-height: auto;
        }

        .protvis-hero-inner {
          padding: 42px 28px 32px 28px;
        }

        .protvis-hero-top {
          flex-direction: column;
        }

        .protvis-hero-content {
          max-width: 100%;
        }

        .protvis-hero-image-wrap {
          width: 100%;
          min-height: auto;
        }

        .protvis-hero-image-card {
          max-width: 100%;
        }

        .protvis-title {
          font-size: 2.6rem;
        }

        .protvis-subtitle {
          max-width: 100%;
        }
      }

      @media (max-width: 768px) {
        body {
          padding-bottom: 380px;
        }

        .protvis-page {
          padding-bottom: 140px;
        }

        .protvis-home-bottom {
          margin-bottom: 140px;
        }

        .protvis-hero {
          border-radius: 24px;
        }

        .protvis-hero-inner {
          padding: 34px 22px 28px 22px;
        }

        .protvis-title {
          font-size: 2.15rem;
        }

        .protvis-subtitle {
          font-size: 1rem;
          line-height: 1.8;
        }

        .protvis-stat-value {
          font-size: 1.7rem;
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

      bslib::nav_panel(
        "Homepage",
        icon = bsicons::bs_icon("house-door-fill"),
        shiny::div(
          class = "container-fluid protvis-page",

          shiny::div(
            class = "protvis-hero",
            shiny::div(
              class = "protvis-hero-inner",

              shiny::div(
                class = "protvis-hero-top",

                shiny::div(
                  class = "protvis-hero-content",
                  shiny::div(
                    class = "protvis-kicker",
                    "Proteomics Analysis and Visualization Platform"
                  ),
                  shiny::div(class = "protvis-title", "ProtVis"),
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

                shiny::div(
                  class = "protvis-hero-image-wrap",
                  shiny::div(
                    class = "protvis-hero-image-card",
                    shiny::tags$img(
                      src = "https://raw.githubusercontent.com/xuebinzhang-lab/ProtVis/dev/app/www/protvis_homepage_bg_clean.png",
                      class = "protvis-hero-figure",
                      alt = "ProtVis homepage illustration"
                    )
                  )
                )
              ),

              bslib::layout_columns(
                class = "protvis-stat-grid",
                col_widths = c(3, 3, 3, 3),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "End-to-end"),
                  shiny::div(class = "protvis-stat-label", "Integrated workflow"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "From project setup and preprocessing to biological interpretation and final visualization."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "Multi-format"),
                  shiny::div(class = "protvis-stat-label", "Flexible input support"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Compatible with multiple proteomics result tables generated from common upstream pipelines."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "Interactive"),
                  shiny::div(class = "protvis-stat-label", "Exploration and analysis"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Supports dynamic data exploration, customizable figures, and modular downstream analysis."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "SCI-ready"),
                  shiny::div(class = "protvis-stat-label", "Figure export"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Produces publication-quality results for scientific communication and further figure refinement."
                  )
                )
              )
            )
          ),

          bslib::layout_columns(
            col_widths = c(8, 4),

            shiny::div(
              class = "protvis-panel",
              shiny::h4("Platform Overview"),
              shiny::p(
                "ProtVis was developed to address major challenges in proteomics analysis, including fragmented workflows, low tool integration, limited interactivity, and insufficient support for reproducible, publication-grade visualization. The platform integrates preprocessing, statistical analysis, functional interpretation, and downstream figure generation into a unified Shiny-based environment."
              ),
              shiny::p(
                "It supports both routine proteomics analysis and advanced exploratory workflows, enabling users with different levels of computational experience to perform standardized and interpretable data analysis in a more efficient manner."
              )
            ),

            shiny::div(
              class = "protvis-panel",
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
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "01"),
              shiny::h4("Integrated Full Workflow"),
              shiny::p(
                "ProtVis connects project initialization, metadata management, data import, correction, transformation, imputation, normalization, DEP analysis, enrichment interpretation, and downstream visualization in one coherent analytical environment."
              )
            ),

            shiny::div(
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "02"),
              shiny::h4("Flexible Data Compatibility"),
              shiny::p(
                "The platform is compatible with multiple commonly used proteomics outputs, allowing users to analyze data from diverse software pipelines without rebuilding the workflow from scratch."
              )
            ),

            shiny::div(
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "03"),
              shiny::h4("Publication-Oriented Visualization"),
              shiny::p(
                "ProtVis supports high-quality figure generation for heatmaps, volcano plots, dimensionality reduction, enrichment, pathway display, and multi-omics exploration, with outputs suitable for scientific publication and post-editing."
              )
            )
          ),

          shiny::h3(class = "protvis-section-title", "Workflow Overview"),
          bslib::layout_columns(
            col_widths = c(2, 2, 2, 2, 2, 2),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "1"),
              shiny::h5("Project Initialization"),
              shiny::p(
                "Create the project environment and configure sample metadata for downstream analysis."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "2"),
              shiny::h5("Data Input"),
              shiny::p(
                "Import proteomics result tables from supported software outputs and user-defined matrices."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "3"),
              shiny::h5("Pre-processing"),
              shiny::p(
                "Perform correction, transformation, missing-value imputation, and normalization for robust analysis."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "4"),
              shiny::h5("Overview & DEP"),
              shiny::p(
                "Conduct exploratory analysis, summary statistics, differential expression analysis, and pattern discovery."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "5"),
              shiny::h5("Biological Interpretation"),
              shiny::p(
                "Perform enrichment analysis, GSEA, and pathway-level interpretation for functional insights."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "6"),
              shiny::h5("Multi-omics & Toolkits"),
              shiny::p(
                "Extend analysis through multi-omics integration modules and additional visualization utilities."
              )
            )
          ),

          bslib::layout_columns(
            class = "protvis-home-bottom",
            col_widths = c(6, 6),

            shiny::div(
              shiny::h3(class = "protvis-section-title", "Supported Data Types"),
              shiny::div(
                class = "protvis-support-box",
                shiny::tags$ul(
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
                class = "protvis-panel",
                shiny::h4("Recommended Navigation"),
                shiny::p(
                  "Start with the ",
                  shiny::strong("Project init"),
                  " tab to initialize the project and configure metadata. Then proceed to ",
                  shiny::strong("Data input"),
                  ", followed by the pre-processing modules before entering downstream analysis."
                ),
                shiny::p(
                  "The modular architecture of ProtVis allows users to complete analyses step by step while maintaining flexibility, transparency, and reproducibility throughout the workflow."
                ),
                shiny::p(
                  class = "protvis-note",
                  "Suggested path: Project init → Data input → Pre-processing → Downstream analysis → Multi-omics / Toolkits."
                )
              )
            )
          )
        )
      ),

      bslib::nav_panel(
        "Project init",
        icon = bsicons::bs_icon("gear"),
        project_init_ui("project_init")
      ),

      bslib::nav_panel(
        "Data input",
        icon = bsicons::bs_icon("usb-drive"),
        data_input_ui("data_input")
      ),

      bslib::nav_menu(
        "Pre-processing",
        icon = bsicons::bs_icon("wrench"),
        bslib::nav_panel("Correct Noise", correct_noise_ui("correct_noise")),
        bslib::nav_panel("Data Transformed", data_transformed_ui("data_transformed")),
        bslib::nav_panel("Data Imputation", data_imputation_ui("data_imputation")),
        bslib::nav_panel("Data Normalization", data_normalization_ui("data_normalization"))
      ),

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

      bslib::nav_menu(
        "PTM",
        icon = bsicons::bs_icon("layers"),
        bslib::nav_panel("PTM", PTM_ui("PTM"))
      ),

      bslib::nav_panel(
        "Release data",
        icon = bsicons::bs_icon("folder2-open"),
        release_data_ui("release_data1")
      ),

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
