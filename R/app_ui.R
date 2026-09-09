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
      :root {
        --pv-bg: #f5f8fb;
        --pv-surface: #ffffff;
        --pv-surface-soft: #f8fbff;
        --pv-border: #dbe8f3;
        --pv-primary: #1787c9;
        --pv-primary-soft: #e8f5fc;
        --pv-text: #1f3447;
        --pv-muted: #657789;
        --pv-shadow: 0 12px 32px rgba(31, 52, 71, 0.08);
      }

      body {
        background:
          radial-gradient(circle at top left, rgba(23, 135, 201, 0.08), transparent 32rem),
          linear-gradient(180deg, #f8fbff 0%, var(--pv-bg) 100%);
        color: var(--pv-text);
      }

      .navbar {
        position: relative;
        z-index: 2500;
        background: rgba(255, 255, 255, 0.94) !important;
        border-bottom: 1px solid var(--pv-border);
        box-shadow: 0 8px 24px rgba(31, 52, 71, 0.07);
        backdrop-filter: blur(12px);
      }

      .navbar-brand,
      .navbar-nav .nav-link {
        color: var(--pv-text) !important;
      }

      .navbar-nav .nav-link.active,
      .navbar-nav .nav-link.show,
      .navbar-nav .nav-link:focus,
      .navbar-nav .nav-link:hover {
        color: var(--pv-primary) !important;
      }

      .navbar-nav .nav-link.active {
        border-bottom: 2px solid var(--pv-primary);
        font-weight: 800;
      }

      .dropdown-menu {
        border: 1px solid var(--pv-border);
        border-radius: 16px;
        box-shadow: var(--pv-shadow);
        padding: 0.45rem;
        z-index: 3000;
      }

      .dropdown-item {
        border-radius: 10px;
        color: var(--pv-text);
      }

      .dropdown-item:hover,
      .dropdown-item:focus,
      .dropdown-item.active {
        background: var(--pv-primary-soft);
        color: var(--pv-primary);
      }

      .card,
      .bslib-card,
      .well,
      .panel,
      .accordion,
      .accordion-item {
        border-color: var(--pv-border) !important;
        border-radius: 18px !important;
        box-shadow: 0 10px 26px rgba(31, 52, 71, 0.06);
      }

      .card,
      .bslib-card,
      .accordion-item {
        background: var(--pv-surface);
      }

      .card-header,
      .accordion-button {
        background: var(--pv-surface-soft) !important;
        color: var(--pv-text) !important;
        font-weight: 750;
      }

      .accordion-button:not(.collapsed) {
        background: var(--pv-primary-soft) !important;
        color: var(--pv-primary) !important;
        box-shadow: none;
      }

      .bslib-sidebar-layout > .sidebar,
      .sidebar {
        background: rgba(255, 255, 255, 0.92) !important;
        border-color: var(--pv-border) !important;
      }

      .form-control,
      .form-select,
      .selectize-input,
      .selectize-dropdown,
      .shiny-input-container input,
      .shiny-input-container select {
        border-color: #cfe0ee !important;
        border-radius: 12px !important;
      }

      .form-control:focus,
      .form-select:focus,
      .selectize-input.focus {
        border-color: var(--pv-primary) !important;
        box-shadow: 0 0 0 0.22rem rgba(23, 135, 201, 0.14) !important;
      }

      .btn {
        border-radius: 12px;
        font-weight: 700;
      }

      .btn-primary,
      .btn-success {
        border-color: var(--pv-primary) !important;
        background: linear-gradient(135deg, #22a5df 0%, var(--pv-primary) 100%) !important;
        box-shadow: 0 8px 18px rgba(23, 135, 201, 0.18);
      }

      .btn-outline-primary {
        border-color: #9ccce8 !important;
        color: var(--pv-primary) !important;
        background: #ffffff !important;
      }

      .btn-outline-primary:hover {
        color: #ffffff !important;
        background: var(--pv-primary) !important;
      }

      table.dataTable {
        border-radius: 14px;
        overflow: hidden;
      }

      table.dataTable thead th {
        background: var(--pv-surface-soft);
        color: var(--pv-text);
        border-bottom: 1px solid var(--pv-border) !important;
      }

      .pv-home {
        --pv-ink: #102b3f;
        --pv-cyan: #068fc8;
        --pv-green: #20a574;
        --pv-gold: #d49b28;
        max-width: 1500px;
        margin: 0 auto;
        padding: 30px 22px 64px;
      }

      .pv-home-hero {
        position: relative;
        overflow: hidden;
        display: grid;
        grid-template-columns: minmax(0, 1.08fr) minmax(420px, 0.92fr);
        gap: 54px;
        align-items: center;
        min-height: 520px;
        padding: 64px;
        border: 1px solid #cbdfea;
        border-radius: 32px;
        background: linear-gradient(132deg, #ffffff 0%, #f5fbfe 56%, #eaf7fb 100%);
        box-shadow: 0 22px 60px rgba(16, 43, 63, 0.11);
      }

      .pv-home-hero::before {
        content: '';
        position: absolute;
        inset: 0;
        pointer-events: none;
        background-image: radial-gradient(rgba(6, 143, 200, 0.14) 1px, transparent 1px);
        background-size: 24px 24px;
        mask-image: linear-gradient(90deg, transparent 20%, black 100%);
        opacity: 0.42;
      }

      .pv-home-copy,
      .pv-object-map {
        position: relative;
        z-index: 1;
      }

      .pv-eyebrow {
        display: inline-flex;
        align-items: center;
        gap: 9px;
        margin-bottom: 20px;
        color: var(--pv-cyan);
        font-size: 0.76rem;
        font-weight: 850;
        letter-spacing: 0.14em;
        text-transform: uppercase;
      }

      .pv-eyebrow::before {
        content: '';
        width: 28px;
        height: 2px;
        background: var(--pv-cyan);
      }

      .pv-home h1 {
        max-width: 720px;
        margin: 0 0 22px;
        color: var(--pv-ink);
        font-size: clamp(2.7rem, 4.8vw, 5.1rem);
        font-weight: 850;
        line-height: 0.98;
        letter-spacing: -0.055em;
      }

      .pv-home h1 span {
        color: var(--pv-cyan);
      }

      .pv-lead {
        max-width: 680px;
        margin: 0 0 26px;
        color: #536b7d;
        font-size: 1.08rem;
        line-height: 1.75;
      }

      .pv-pill-row {
        display: flex;
        flex-wrap: wrap;
        gap: 9px;
      }

      .pv-pill {
        padding: 8px 13px;
        border: 1px solid #c9e3ee;
        border-radius: 999px;
        background: rgba(255, 255, 255, 0.76);
        color: #2c5369;
        font-size: 0.82rem;
        font-weight: 700;
      }

      .pv-object-map {
        display: grid;
        grid-template-columns: 1fr 1.15fr 1fr;
        grid-template-rows: repeat(3, auto);
        gap: 14px;
        align-items: center;
      }

      .pv-object-core {
        grid-column: 2;
        grid-row: 1 / 4;
        display: flex;
        min-height: 236px;
        padding: 24px 18px;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        border: 1px solid rgba(255, 255, 255, 0.24);
        border-radius: 28px;
        color: #fff;
        background: linear-gradient(155deg, #0f3650 0%, #0b607e 58%, #078db2 100%);
        box-shadow: 0 24px 46px rgba(7, 92, 124, 0.24);
      }

      .pv-core-mark {
        width: 54px;
        height: 54px;
        margin-bottom: 16px;
        border: 1px solid rgba(255, 255, 255, 0.35);
        border-radius: 16px;
        display: grid;
        place-items: center;
        background: rgba(255, 255, 255, 0.1);
      }

      .pv-core-mark svg {
        width: 25px;
        height: 25px;
      }

      .pv-object-core strong {
        font-size: 1.16rem;
        letter-spacing: -0.02em;
      }

      .pv-object-core small {
        margin-top: 9px;
        color: #cdebf4;
        line-height: 1.5;
      }

      .pv-object-node {
        position: relative;
        padding: 14px 12px;
        border: 1px solid #d4e4ec;
        border-radius: 15px;
        background: rgba(255, 255, 255, 0.92);
        box-shadow: 0 10px 24px rgba(16, 43, 63, 0.07);
        color: #24475c;
        font-size: 0.79rem;
        font-weight: 800;
        text-align: center;
      }

      .pv-object-node::after {
        content: '';
        position: absolute;
        top: 50%;
        width: 14px;
        height: 1px;
        background: #9bc5d6;
      }

      .pv-node-left:nth-child(1) { grid-column: 1; grid-row: 1; }
      .pv-node-left:nth-child(2) { grid-column: 1; grid-row: 2; }
      .pv-node-left:nth-child(3) { grid-column: 1; grid-row: 3; }
      .pv-node-right:nth-child(5) { grid-column: 3; grid-row: 1; }
      .pv-node-right:nth-child(6) { grid-column: 3; grid-row: 2; }
      .pv-node-right:nth-child(7) { grid-column: 3; grid-row: 3; }
      .pv-node-left::after { right: -15px; }
      .pv-node-right::after { left: -15px; }

      .pv-metrics {
        position: relative;
        z-index: 2;
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        margin: -1px 34px 58px;
        border: 1px solid #d8e6ed;
        border-radius: 0 0 22px 22px;
        background: #fff;
        box-shadow: 0 14px 34px rgba(16, 43, 63, 0.07);
      }

      .pv-metric {
        padding: 23px 25px;
        border-right: 1px solid #e2ebf0;
      }

      .pv-metric:last-child { border-right: 0; }
      .pv-metric strong {
        display: block;
        color: var(--pv-ink);
        font-size: 1.55rem;
        line-height: 1.15;
      }

      .pv-metric span {
        display: block;
        margin-top: 5px;
        color: #718695;
        font-size: 0.8rem;
        font-weight: 650;
        letter-spacing: 0.02em;
      }

      .pv-section {
        margin: 0 0 64px;
      }

      .pv-section-head {
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        gap: 24px;
        margin-bottom: 25px;
      }

      .pv-section-index {
        display: block;
        margin-bottom: 8px;
        color: var(--pv-cyan);
        font-size: 0.72rem;
        font-weight: 850;
        letter-spacing: 0.14em;
        text-transform: uppercase;
      }

      .pv-section h2 {
        margin: 0;
        color: var(--pv-ink);
        font-size: clamp(1.8rem, 3vw, 2.65rem);
        font-weight: 820;
        line-height: 1.1;
        letter-spacing: -0.035em;
      }

      .pv-section-head p {
        max-width: 520px;
        margin: 0;
        color: #647b8b;
        line-height: 1.65;
      }

      .pv-capability-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 16px;
      }

      .pv-capability {
        min-height: 210px;
        padding: 24px;
        border: 1px solid #dbe6ec;
        border-radius: 21px;
        background: #fff;
        box-shadow: 0 10px 26px rgba(16, 43, 63, 0.05);
        transition: transform 0.2s ease, border-color 0.2s ease, box-shadow 0.2s ease;
      }

      .pv-capability:hover {
        transform: translateY(-3px);
        border-color: #a9d4e4;
        box-shadow: 0 16px 34px rgba(16, 43, 63, 0.09);
      }

      .pv-capability-icon {
        width: 43px;
        height: 43px;
        display: grid;
        place-items: center;
        margin-bottom: 21px;
        border-radius: 13px;
        color: var(--pv-cyan);
        background: #eaf7fc;
      }

      .pv-capability-icon svg { width: 21px; height: 21px; }
      .pv-capability:nth-child(2) .pv-capability-icon,
      .pv-capability:nth-child(5) .pv-capability-icon { color: var(--pv-green); background: #e9f8f2; }
      .pv-capability:nth-child(3) .pv-capability-icon,
      .pv-capability:nth-child(6) .pv-capability-icon { color: var(--pv-gold); background: #fff7e6; }

      .pv-capability h3 {
        margin: 0 0 10px;
        color: var(--pv-ink);
        font-size: 1.03rem;
        font-weight: 800;
      }

      .pv-capability p {
        margin: 0;
        color: #647b8b;
        font-size: 0.9rem;
        line-height: 1.7;
      }

      .pv-workflow {
        display: grid;
        grid-template-columns: repeat(7, 1fr);
        overflow: hidden;
        border: 1px solid #d6e5ec;
        border-radius: 22px;
        background: #fff;
        box-shadow: 0 12px 32px rgba(16, 43, 63, 0.06);
      }

      .pv-workflow-step {
        position: relative;
        min-height: 174px;
        padding: 24px 17px;
        border-right: 1px solid #e1ebf0;
      }

      .pv-workflow-step:last-child { border-right: 0; }
      .pv-workflow-step b {
        display: block;
        margin-bottom: 20px;
        color: var(--pv-cyan);
        font-size: 0.73rem;
        letter-spacing: 0.1em;
      }

      .pv-workflow-step h3 {
        margin: 0 0 8px;
        color: var(--pv-ink);
        font-size: 0.91rem;
        font-weight: 800;
        line-height: 1.35;
      }

      .pv-workflow-step p {
        margin: 0;
        color: #718695;
        font-size: 0.79rem;
        line-height: 1.5;
      }

      .pv-workflow-step::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: var(--pv-cyan);
        opacity: calc(0.34 + var(--step) * 0.08);
      }

      .pv-object-section {
        display: grid;
        grid-template-columns: 0.9fr 1.1fr;
        gap: 18px;
      }

      .pv-object-story,
      .pv-schema {
        border-radius: 24px;
        padding: 30px;
      }

      .pv-object-story {
        color: #fff;
        background: linear-gradient(145deg, #102f44 0%, #164c64 100%);
      }

      .pv-object-story h2 { color: #fff; }
      .pv-object-story p {
        margin: 18px 0 24px;
        color: #c9e0e9;
        line-height: 1.75;
      }

      .pv-proof-list {
        display: grid;
        gap: 10px;
      }

      .pv-proof-list span {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 0.88rem;
        font-weight: 700;
      }

      .pv-proof-list span::before {
        content: '✓';
        display: grid;
        place-items: center;
        width: 22px;
        height: 22px;
        border-radius: 50%;
        color: #0c4d3a;
        background: #72dbb6;
        font-size: 0.72rem;
      }

      .pv-schema {
        border: 1px solid #d8e6ed;
        background: #fff;
      }

      .pv-schema-row {
        display: grid;
        grid-template-columns: minmax(150px, 0.42fr) 1fr;
        gap: 18px;
        padding: 14px 0;
        border-bottom: 1px solid #e7eef2;
      }

      .pv-schema-row:last-child { border-bottom: 0; }
      .pv-schema-row code {
        color: #076f98;
        font-weight: 750;
      }

      .pv-schema-row span {
        color: #647b8b;
        font-size: 0.88rem;
        line-height: 1.55;
      }

      .pv-source-band {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 10px;
        padding: 22px 25px;
        border: 1px solid #d7e5ec;
        border-radius: 20px;
        background: #fff;
      }

      .pv-source-band strong {
        margin-right: 12px;
        color: var(--pv-ink);
      }

      .pv-source-band span {
        padding: 7px 11px;
        border-radius: 8px;
        color: #4c6677;
        background: #f2f7f9;
        font-size: 0.78rem;
        font-weight: 700;
      }

      @media (max-width: 1180px) {
        .pv-home-hero { grid-template-columns: 1fr; padding: 50px; }
        .pv-object-map { max-width: 700px; }
        .pv-workflow { grid-template-columns: repeat(4, 1fr); }
        .pv-workflow-step:nth-child(4) { border-right: 0; }
        .pv-workflow-step:nth-child(n+5) { border-top: 1px solid #e1ebf0; }
      }

      @media (max-width: 820px) {
        .pv-home { padding: 18px 12px 42px; }
        .pv-home-hero { padding: 34px 26px; border-radius: 24px; }
        .pv-object-map { grid-template-columns: 1fr 1.2fr 1fr; gap: 8px; }
        .pv-object-node { padding: 11px 7px; font-size: 0.7rem; }
        .pv-metrics { grid-template-columns: repeat(2, 1fr); margin: -1px 18px 44px; }
        .pv-metric:nth-child(2) { border-right: 0; }
        .pv-metric:nth-child(-n+2) { border-bottom: 1px solid #e2ebf0; }
        .pv-capability-grid { grid-template-columns: repeat(2, 1fr); }
        .pv-workflow { grid-template-columns: repeat(2, 1fr); }
        .pv-workflow-step:nth-child(even) { border-right: 0; }
        .pv-workflow-step:nth-child(n+3) { border-top: 1px solid #e1ebf0; }
        .pv-object-section { grid-template-columns: 1fr; }
        .pv-section-head { align-items: flex-start; flex-direction: column; }
      }

      @media (max-width: 560px) {
        .pv-home-hero { padding: 30px 20px; }
        .pv-home h1 { font-size: 2.55rem; }
        .pv-object-map { display: none; }
        .pv-metrics { margin: -1px 10px 38px; }
        .pv-metric { padding: 18px 15px; }
        .pv-capability-grid { grid-template-columns: 1fr; }
        .pv-workflow { grid-template-columns: 1fr; }
        .pv-workflow-step { border-right: 0; border-top: 1px solid #e1ebf0; }
        .pv-workflow-step:first-child { border-top: 0; }
        .pv-schema-row { grid-template-columns: 1fr; gap: 5px; }
      }

      @media (prefers-reduced-motion: reduce) {
        .pv-capability { transition: none; }
      }
    "))
  )
}

#' ProtVis Homepage
#'
#' Builds the static, publication-oriented landing page without coupling it to
#' any server-side reactive state.
#'
#' @keywords internal
protvis_homepage <- function() {
  capability <- function(icon, title, description) {
    shiny::div(
      class = "pv-capability",
      shiny::div(class = "pv-capability-icon", bsicons::bs_icon(icon)),
      shiny::h3(title),
      shiny::p(description)
    )
  }

  workflow_step <- function(number, title, description) {
    shiny::div(
      class = "pv-workflow-step",
      style = paste0("--step: ", number),
      shiny::tags$b(sprintf("%02d", number)),
      shiny::h3(title),
      shiny::p(description)
    )
  }

  shiny::div(
    class = "pv-home",
    shiny::tags$section(
      class = "pv-home-hero",
      shiny::div(
        class = "pv-home-copy",
        shiny::div(
          class = "pv-eyebrow",
          "Reproducible proteomics · end-to-end analysis"
        ),
        shiny::h1("From quantitative proteomes to ", shiny::span("biological insight.")),
        shiny::p(
          class = "pv-lead",
          "ProtVis unifies data preparation, statistical analysis, functional annotation, and publication-ready visualization in a traceable scientific workflow."
        ),
        shiny::div(
          class = "pv-pill-row",
          shiny::span(class = "pv-pill", "Transparent provenance"),
          shiny::span(class = "pv-pill", "Checkpoint recovery"),
          shiny::span(class = "pv-pill", "Interactive visualization")
        )
      ),
      shiny::div(
        class = "pv-object-map",
        shiny::div(class = "pv-object-node pv-node-left", "Expression matrix"),
        shiny::div(class = "pv-object-node pv-node-left", "Sample metadata"),
        shiny::div(class = "pv-object-node pv-node-left", "Variable metadata"),
        shiny::div(
          class = "pv-object-core",
          shiny::div(class = "pv-core-mark", bsicons::bs_icon("database")),
          shiny::tags$strong("ProtVis_dataset"),
          shiny::tags$small(
            "One independent S4 object across the complete workflow"
          )
        ),
        shiny::div(class = "pv-object-node pv-node-right", "Analysis results"),
        shiny::div(class = "pv-object-node pv-node-right", "Functional annotation"),
        shiny::div(class = "pv-object-node pv-node-right", "Process history")
      )
    ),
    shiny::div(
      class = "pv-metrics",
      shiny::div(class = "pv-metric", shiny::tags$strong("8"), shiny::span("supported input sources")),
      shiny::div(class = "pv-metric", shiny::tags$strong("7"), shiny::span("connected workflow stages")),
      shiny::div(class = "pv-metric", shiny::tags$strong("3"), shiny::span("annotation layers")),
      shiny::div(class = "pv-metric", shiny::tags$strong("S4"), shiny::span("independent data architecture"))
    ),
    shiny::tags$section(
      class = "pv-section",
      shiny::div(
        class = "pv-section-head",
        shiny::div(
          shiny::span(class = "pv-section-index", "01 · Analytical scope"),
          shiny::h2("A rigorous workflow, from input to interpretation")
        ),
        shiny::p("Purpose-built modules preserve biological context while reducing friction between data processing, statistics, and scientific communication.")
      ),
      shiny::div(
        class = "pv-capability-grid",
        capability("database", "Flexible data ingestion", "Import MaxQuant, DIA-NN, Spectronaut, FragPipe, Skyline, OpenMS, Proteome Discoverer, or a custom matrix."),
        capability("sliders", "Proteomics preprocessing", "Correct noise, transform intensities, impute missing values, normalize samples, and retain each analytical stage."),
        capability("bar-chart-line", "Statistical exploration", "Assess sample relationships, dimensionality reduction, quality metrics, and differential protein abundance."),
        capability("diagram-3", "Biological interpretation", "Connect significant proteins with enrichment, GSEA, pathway views, and structured functional annotations."),
        capability("layers", "Structure and PTM", "Explore protein structure, modification sites, and strict peptide-spectrum evidence in dedicated modules."),
        capability("arrow-repeat", "Multi-omics integration", "Extend proteomic findings through expression profiles, WGCNA, co-enrichment, and complementary omics views.")
      )
    ),
    shiny::tags$section(
      class = "pv-section",
      shiny::div(
        class = "pv-section-head",
        shiny::div(
          shiny::span(class = "pv-section-index", "02 · Reproducible workflow"),
          shiny::h2("Seven stages, one continuous analytical record")
        ),
        shiny::p("Every stage contributes data, parameters, status, and history to the same project object, enabling inspection and checkpoint recovery.")
      ),
      shiny::div(
        class = "pv-workflow",
        workflow_step(1, "Project init", "Define samples and experimental metadata."),
        workflow_step(2, "Data input", "Parse and validate quantitative evidence."),
        workflow_step(3, "Pre-process", "Transform, impute, and normalize."),
        workflow_step(4, "Overview", "Inspect QC and sample structure."),
        workflow_step(5, "DEP", "Model differential abundance."),
        workflow_step(6, "Interpret", "Enrichment, GSEA, and pathways."),
        workflow_step(7, "Communicate", "Visualize, export, and revisit results.")
      )
    ),
    shiny::tags$section(
      class = "pv-section pv-object-section",
      shiny::div(
        class = "pv-object-story",
        shiny::span(class = "pv-section-index", "03 · Data architecture"),
        shiny::h2("One object, complete provenance"),
        shiny::p("ProtVis_dataset is an independent S4 class designed specifically for proteomics. It keeps quantitative data aligned with sample and variable metadata while recording every processing decision."),
        shiny::div(
          class = "pv-proof-list",
          shiny::span("Validated sample-to-matrix alignment"),
          shiny::span("Stage-specific analysis results"),
          shiny::span("Parameters, timestamps, and history"),
          shiny::span("eggNOG, GO, and KEGG annotation")
        )
      ),
      shiny::div(
        class = "pv-schema",
        shiny::div(class = "pv-schema-row", shiny::tags$code("expression_data"), shiny::span("Quantitative protein abundance matrix across samples")),
        shiny::div(class = "pv-schema-row", shiny::tags$code("sample_info"), shiny::span("Experimental groups, tissue, species, and sample identifiers")),
        shiny::div(class = "pv-schema-row", shiny::tags$code("variable_info"), shiny::span("Protein identifiers and feature-level descriptors")),
        shiny::div(class = "pv-schema-row", shiny::tags$code("annotation"), shiny::span("eggNOG output plus GO and KEGG annotation tables")),
        shiny::div(class = "pv-schema-row", shiny::tags$code("analysis_results"), shiny::span("Outputs retained independently for each analytical stage")),
        shiny::div(class = "pv-schema-row", shiny::tags$code("process_info"), shiny::span("Parameters, execution time, history, and active stage"))
      )
    ),
    shiny::div(
      class = "pv-source-band",
      shiny::tags$strong("Input ecosystem"),
      lapply(
        c("MaxQuant", "Proteome Discoverer", "DIA-NN", "Spectronaut", "FragPipe", "Skyline", "OpenMS", "Custom matrix"),
        function(source) shiny::span(source)
      )
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
#' @importFrom bslib nav_panel nav_menu page_navbar bs_theme layout_columns
#' @name app_ui
#' @export
#'
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "ProtVis",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "lumen",
        bg = "#f8fbff",
        fg = "#1f3447",
        primary = "#1787c9",
        secondary = "#657789",
        success = "#2fb176",
        info = "#56b6d9"
      ),

      bslib::nav_panel(
        "Homepage",
        icon = bsicons::bs_icon("house-door-fill"),
        protvis_homepage()
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
        bslib::nav_panel(
          "Correct Noise",
          icon = bsicons::bs_icon("soundwave"),
          correct_noise_ui("correct_noise")
        ),
        bslib::nav_panel(
          "Data Transformation",
          icon = bsicons::bs_icon("arrow-repeat"),
          data_transformed_ui("data_transformed")
        ),
        bslib::nav_panel(
          "Data Imputation",
          icon = bsicons::bs_icon("patch-plus"),
          data_imputation_ui("data_imputation")
        ),
        bslib::nav_panel(
          "Data Normalization",
          icon = bsicons::bs_icon("sliders"),
          data_normalization_ui("data_normalization")
        )
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
        bslib::nav_panel("Pathview", pathview_ui("pathview"))
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
        bslib::nav_panel(
          "Metaproteomics",
          metaproteomics_ui("metaproteomics")
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
          "STRINGdb PPI",
          icon = bsicons::bs_icon("diagram-3"),
          stringdb_ppi_ui("stringdb_ppi")
        ),
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
