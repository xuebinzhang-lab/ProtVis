#' Add External Resources to the Shiny Application
#'
#' This internal function adds external resources such as CSS, JS,
#' and favicon to the Shiny app. It also sets up resource paths
#' for static files within the `app/www` directory.
#'
#' @import shiny
#' @importFrom golem bundle_resources
#' @name golem_add_external_resources
#' @export
#'
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    shiny::tags$link(
      rel = "icon",
      type = "image/x-icon",
      href = "https://raw.githubusercontent.com/xuebinzhang-lab/ProtVis/dev/app/www/ProtVis_ico.ico"
    ),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProtVis"
    ),
    shiny::tags$style(shiny::HTML("
      body {
        background: #eef1f4;
      }

      .protvis-page {
        max-width: 1450px;
        margin: 0 auto;
        padding: 8px 6px 280px 6px;
      }

      .protvis-hero {
        position: relative;
        overflow: hidden;
        border-radius: 28px;
        min-height: 620px;
        margin: 18px auto 22px auto;
        background:
          radial-gradient(circle at top right, rgba(255,255,255,0.10) 0, rgba(255,255,255,0.04) 20%, transparent 40%),
          radial-gradient(circle at bottom left, rgba(255,255,255,0.06) 0, transparent 28%),
          linear-gradient(135deg, #1f2937 0%, #4b5563 45%, #9ca3af 100%);
        color: white;
        box-shadow: 0 16px 36px rgba(31, 41, 55, 0.16);
      }

      .protvis-hero-inner {
        width: 100%;
        padding: 38px 42px 28px 42px;
      }

      .protvis-hero-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 28px;
      }

      .protvis-hero-content {
        flex: 1 1 58%;
        max-width: 700px;
        padding-top: 0;
      }

      .protvis-hero-image-wrap {
        flex: 0 0 38%;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 260px;
      }

      .protvis-hero-image-card {
        width: 100%;
        max-width: 460px;
        min-height: 250px;
        background: transparent;
        border: none;
        box-shadow: none;
        overflow: visible;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0;
      }

      .protvis-hero-figure {
        width: 100%;
        height: auto;
        max-height: 290px;
        object-fit: contain;
        border-radius: 16px;
        display: block;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.16);
      }

      .protvis-kicker {
        display: inline-block;
        font-size: 0.82rem;
        font-weight: 800;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: rgba(255,255,255,0.88);
        margin-bottom: 12px;
      }

      .protvis-title {
        font-size: 3rem;
        font-weight: 900;
        line-height: 1.02;
        margin-bottom: 12px;
        letter-spacing: -0.02em;
      }

      .protvis-subtitle {
        font-size: 1rem;
        line-height: 1.72;
        max-width: 650px;
        color: rgba(255,255,255,0.94);
        margin-bottom: 16px;
      }

      .protvis-badge-wrap {
        margin-top: 2px;
      }

      .protvis-badge {
        display: inline-block;
        border-radius: 999px;
        padding: 7px 13px;
        margin-right: 8px;
        margin-bottom: 8px;
        background: rgba(255,255,255,0.10);
        border: 1px solid rgba(255,255,255,0.15);
        color: white;
        font-size: 0.88rem;
        font-weight: 600;
        backdrop-filter: blur(4px);
      }

      .protvis-stat-grid {
        margin-top: 20px;
        align-items: stretch;
        gap: 10px 0;
      }

      .protvis-stat-grid > div {
        display: flex;
      }

      .protvis-stat-card {
        background: rgba(255,255,255,0.10);
        border: 1px solid rgba(255,255,255,0.14);
        border-radius: 18px;
        padding: 16px 16px;
        min-height: 145px;
        width: 100%;
        height: 100%;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.05);
        backdrop-filter: blur(8px);
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
      }

      .protvis-stat-value {
        font-size: 1.7rem;
        font-weight: 900;
        line-height: 1.1;
        margin-bottom: 6px;
      }

      .protvis-stat-label {
        font-size: 0.91rem;
        font-weight: 700;
        color: rgba(255,255,255,0.94);
        margin-bottom: 7px;
      }

      .protvis-stat-note {
        font-size: 0.84rem;
        line-height: 1.58;
        color: rgba(255,255,255,0.82);
        margin-bottom: 0;
      }

      .protvis-section-title {
        font-size: 1.45rem;
        font-weight: 900;
        color: #111827;
        margin-top: 8px;
        margin-bottom: 16px;
      }

      .protvis-panel {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 20px;
        padding: 22px 22px;
        box-shadow: 0 8px 24px rgba(31, 41, 55, 0.05);
        margin-bottom: 18px;
        height: 100%;
      }

      .protvis-panel h4 {
        font-size: 1.10rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 12px;
      }

      .protvis-panel p,
      .protvis-panel li {
        color: #4b5563;
        line-height: 1.72;
        font-size: 0.95rem;
      }

      .protvis-panel ul {
        padding-left: 18px;
        margin-bottom: 0;
      }

      .protvis-feature-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 20px;
        padding: 20px 18px;
        min-height: 190px;
        height: 100%;
        box-shadow: 0 8px 20px rgba(31, 41, 55, 0.05);
        margin-bottom: 16px;
        transition: transform 0.18s ease, box-shadow 0.18s ease;
      }

      .protvis-feature-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 12px 24px rgba(31, 41, 55, 0.08);
      }

      .protvis-feature-icon {
        width: 44px;
        height: 44px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #e5e7eb 0%, #f3f4f6 100%);
        color: #374151;
        font-size: 1.15rem;
        font-weight: 900;
        margin-bottom: 14px;
      }

      .protvis-feature-card h4 {
        font-size: 1.03rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 10px;
      }

      .protvis-feature-card p {
        font-size: 0.94rem;
        line-height: 1.68;
        color: #4b5563;
        margin-bottom: 0;
      }

      .protvis-flow-card {
        background: linear-gradient(180deg, #ffffff 0%, #f9fafb 100%);
        border: 1px solid #e5e7eb;
        border-radius: 18px;
        padding: 16px 15px;
        min-height: 150px;
        height: 100%;
        box-shadow: 0 7px 18px rgba(31, 41, 55, 0.04);
        position: relative;
      }

      .protvis-step-badge {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 34px;
        height: 34px;
        border-radius: 999px;
        background: #4b5563;
        color: white;
        font-size: 0.88rem;
        font-weight: 900;
        margin-bottom: 12px;
      }

      .protvis-flow-card h5 {
        font-size: 0.98rem;
        font-weight: 850;
        color: #111827;
        margin-bottom: 8px;
      }

      .protvis-flow-card p {
        font-size: 0.90rem;
        line-height: 1.62;
        color: #4b5563;
        margin-bottom: 0;
      }

      .protvis-support-box {
        background: linear-gradient(180deg, #f3f4f6 0%, #f9fafb 100%);
        border: 1px solid #d1d5db;
        border-radius: 18px;
        padding: 18px 20px;
        min-height: 100%;
        height: 100%;
      }

      .protvis-support-box p,
      .protvis-support-box li {
        font-size: 0.94rem;
        line-height: 1.72;
        color: #374151;
      }

      .protvis-support-box ul {
        margin-bottom: 0;
        padding-left: 18px;
      }

      .protvis-note {
        font-size: 0.92rem;
        color: #6b7280;
        line-height: 1.7;
        margin-top: 8px;
      }

      .protvis-home-bottom {
        margin-bottom: 24px;
      }

      .protvis-site-footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        z-index: 1050;
        background: #111827;
        color: rgba(255,255,255,0.92);
        padding: 16px 0 10px 0;
        border-top: 1px solid rgba(255,255,255,0.08);
        box-shadow: 0 -6px 20px rgba(0,0,0,0.18);
      }

      .protvis-site-footer .footer-inner {
        max-width: 1450px;
        margin: 0 auto;
        padding: 0 18px;
      }

      .protvis-site-footer .footer-title {
        font-size: 0.98rem;
        font-weight: 850;
        margin-bottom: 5px;
      }

      .protvis-site-footer .footer-text {
        font-size: 0.88rem;
        line-height: 1.65;
        color: rgba(255,255,255,0.80);
        margin-bottom: 0;
      }

      .protvis-site-footer .footer-small {
        margin-top: 8px;
        padding-top: 8px;
        border-top: 1px solid rgba(255,255,255,0.10);
        font-size: 0.82rem;
        color: rgba(255,255,255,0.68);
      }

      @media (max-width: 1200px) {
        .protvis-title {
          font-size: 2.7rem;
        }

        .protvis-hero {
          min-height: 580px;
        }

        .protvis-hero-image-wrap {
          flex: 0 0 36%;
        }
      }

      @media (max-width: 992px) {
        .protvis-page {
          padding-bottom: 340px;
        }

        .protvis-home-bottom {
          margin-bottom: 18px;
        }

        .protvis-hero {
          min-height: auto;
          border-radius: 24px;
        }

        .protvis-hero-inner {
          padding: 32px 24px 24px 24px;
        }

        .protvis-hero-top {
          flex-direction: column;
          align-items: flex-start;
          gap: 20px;
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
          min-height: auto;
        }

        .protvis-hero-figure {
          max-height: 260px;
        }

        .protvis-title {
          font-size: 2.35rem;
        }

        .protvis-subtitle {
          max-width: 100%;
        }
      }

      @media (max-width: 768px) {
        .protvis-page {
          padding-bottom: 390px;
        }

        .protvis-home-bottom {
          margin-bottom: 14px;
        }

        .protvis-hero-inner {
          padding: 26px 18px 22px 18px;
        }

        .protvis-title {
          font-size: 2rem;
        }

        .protvis-subtitle {
          font-size: 0.96rem;
          line-height: 1.72;
        }

        .protvis-stat-value {
          font-size: 1.55rem;
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
                    "Integrated Proteomics Analysis Platform"
                  ),
                  shiny::div(class = "protvis-title", "ProtVis"),
                  shiny::p(
                    class = "protvis-subtitle",
                    "ProtVis integrates proteomics processing, differential analysis, functional interpretation, protein 3D and PTM visualization, multi-omics analysis, and figure generation in one platform."
                  ),
                  shiny::div(
                    class = "protvis-badge-wrap",
                    shiny::span(class = "protvis-badge", "End-to-End"),
                    shiny::span(class = "protvis-badge", "Protein 3D"),
                    shiny::span(class = "protvis-badge", "PTM visualization"),
                    shiny::span(class = "protvis-badge", "Multi-omics"),
                    shiny::span(class = "protvis-badge", "Modular")
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
                  shiny::div(class = "protvis-stat-value", "End-to-End"),
                  shiny::div(class = "protvis-stat-label", "Integrated workflow"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "From data preprocessing to analysis and visualization."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "Protein 3D"),
                  shiny::div(class = "protvis-stat-label", "Structure visualization"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Supports protein structure exploration."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "PTM"),
                  shiny::div(class = "protvis-stat-label", "Modification visualization"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Supports PTM analysis and visualization."
                  )
                ),

                shiny::div(
                  class = "protvis-stat-card",
                  shiny::div(class = "protvis-stat-value", "Multi-omics"),
                  shiny::div(class = "protvis-stat-label", "Integrated analysis"),
                  shiny::div(
                    class = "protvis-stat-note",
                    "Supports multi-omics analysis and cross-omics exploration."
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
                "ProtVis provides an integrated workflow for proteomics data preprocessing, differential analysis, functional interpretation, protein 3D and PTM visualization, multi-omics analysis, and downstream visualization in a unified Shiny-based environment."
              ),
              shiny::p(
                "It supports both routine and exploratory workflows, helping users perform efficient and interpretable analyses with reduced technical barriers."
              )
            ),

            shiny::div(
              class = "protvis-panel",
              shiny::h4("At a Glance"),
              shiny::tags$ul(
                shiny::tags$li("End-to-End proteomics workflow"),
                shiny::tags$li("2D and 3D visualization of key proteins"),
                shiny::tags$li("PTM visualization"),
                shiny::tags$li("Multi-omics analysis"),
                shiny::tags$li("Modular extension and downstream visualization")
              )
            )
          ),

          shiny::h3(class = "protvis-section-title", "Core Highlights"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),

            shiny::div(
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "01"),
              shiny::h4("Integrated Workflow"),
              shiny::p(
                "ProtVis integrates data input, preprocessing, downstream analysis, biological interpretation, and visualization into a streamlined workflow."
              )
            ),

            shiny::div(
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "02"),
              shiny::h4("2D and 3D Visualization of Key Proteins"),
              shiny::p(
                "ProtVis supports intuitive 2D and 3D visualization of key proteins to facilitate structure-aware exploration and result interpretation."
              )
            ),

            shiny::div(
              class = "protvis-feature-card",
              shiny::div(class = "protvis-feature-icon", "03"),
              shiny::h4("PTM Visualization"),
              shiny::p(
                "ProtVis provides dedicated PTM visualization for modification-centric analysis, enabling clearer presentation of phosphorylation and other PTM-related results."
              )
            )
          ),

          shiny::h3(class = "protvis-section-title", "Workflow Overview"),

          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "1"),
              shiny::h5("Project Initialization"),
              shiny::p(
                "Set up the project environment and configure sample metadata."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "2"),
              shiny::h5("Data Input"),
              shiny::p(
                "Import supported proteomics tables and user-defined matrices."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "3"),
              shiny::h5("Pre-processing"),
              shiny::p(
                "Perform correction, transformation, imputation, and normalization."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "4"),
              shiny::h5("Overview & DEP"),
              shiny::p(
                "Run exploratory analysis, summary statistics, and differential analysis."
              )
            )
          ),

          bslib::layout_columns(
            class = "mt-3",
            col_widths = c(4, 4, 4),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "5"),
              shiny::h5("Biological Interpretation"),
              shiny::p(
                "Interpret results through enrichment analysis, GSEA, pathways, and functional exploration."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "6"),
              shiny::h5("Multi-omics Analysis & Toolkits"),
              shiny::p(
                "Support multi-omics analysis with dedicated modules and additional visualization utilities."
              )
            ),

            shiny::div(
              class = "protvis-flow-card",
              shiny::div(class = "protvis-step-badge", "7"),
              shiny::h5("Protein 3D & PTM Visualization"),
              shiny::p(
                "Explore key proteins through 3D structure and PTM visualization in dedicated modules."
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
                shiny::p(
                  "ProtVis supports multiple proteomics data tables and customizable matrix inputs for downstream analysis and visualization."
                ),
                shiny::tags$ul(
                  shiny::tags$li("MaxQuant output tables"),
                  shiny::tags$li("Proteome Discoverer output tables"),
                  shiny::tags$li("Skyline-based quantitative datasets"),
                  shiny::tags$li("Mascot and OpenMS compatible tabular inputs"),
                  shiny::tags$li("User-defined matrices")
                )
              )
            ),

            shiny::div(
              shiny::h3(class = "protvis-section-title", "Get Started"),
              shiny::div(
                class = "protvis-panel",
                shiny::h4("Recommended Navigation"),
                shiny::p(
                  "Begin with ",
                  shiny::strong("Project init"),
                  " to define the project and metadata, then proceed to ",
                  shiny::strong("Data input"),
                  " and the pre-processing modules before entering downstream analysis. ",
                  shiny::strong("Multi-omics"),
                  ", ",
                  shiny::strong("Protein Structure"),
                  ", and ",
                  shiny::strong("PTM"),
                  " are provided as dedicated modules for extended biological exploration."
                ),
                shiny::p(
                  "The modular design of ProtVis supports a clear workflow from preprocessing to downstream analysis and visualization."
                ),
                shiny::p(
                  class = "protvis-note",
                  "Suggested path: Project init → Data input → Pre-processing → Downstream analysis → Multi-omics / Toolkits → Protein Structure / PTM."
                )
              )
            )
          ),

          shiny::tags$footer(
            class = "protvis-site-footer",
            shiny::div(
              class = "footer-inner",
              bslib::layout_columns(
                col_widths = c(4, 5, 3),

                shiny::div(
                  shiny::div(class = "footer-title", "ProtVis"),
                  shiny::p(
                    class = "footer-text",
                    "ProtVis is an integrated platform for proteomics data processing, differential analysis, functional interpretation, protein 3D and PTM visualization, multi-omics analysis, and downstream visualization."
                  )
                ),

                shiny::div(
                  shiny::div(class = "footer-title", "Developer"),
                  shiny::p(
                    class = "footer-text",
                    shiny::strong("Fei Liang & Xiao Wang"),
                    shiny::br(),
                    "State Key Laboratory of Crop Stress Adaptation and Improvement,",
                    shiny::br(),
                    "Henan Joint International Laboratory for Crop Multi‐Omics Research,",
                    shiny::br(),
                    "School of Life Sciences, Henan University,",
                    shiny::br(),
                    "Kaifeng 475004, China"
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
        # bslib::nav_panel("Protein function", protein_fun_ui("protein_fun"))
      ),

      bslib::nav_menu(
        "Multi-omics",
        icon = bsicons::bs_icon("database-gear"),
        bslib::nav_panel(
          "Expression Profile",
          Expression_profile_ui("Expression_profile")
        ),
        bslib::nav_panel(
          "WGCNA",
          wgcna_ui("wgcna")
        ),
        bslib::nav_panel("Nine Quadrant", nine_quadrant_ui("nine")),
        bslib::nav_panel(
          title = "Co-enrichment",
          co_enrichment_ui("co_enrichment")
        ),
        bslib::nav_panel("Venn", venn_ui("venn"))
      ),

      bslib::nav_menu(
        "PTM",
        icon = bsicons::bs_icon("layers"),
        bslib::nav_panel("PTM", PTM_ui("PTM")),
        bslib::nav_panel(
          "PD Strict Spectrum",
          icon = bsicons::bs_icon("activity"),
          pd_strict_module_ui("pd_strict")
        )
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
        bslib::nav_panel("Correlation chord", correlation_chord_ui("correlation_chord")),
        bslib::nav_panel("DEG Analyse", DEG_ui("DEG"))
      ),

      help_ui()
    )
  )
}
