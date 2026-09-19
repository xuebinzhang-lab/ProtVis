# Additive UI integration for Protein Workbench.
# This file is intentionally collated after app_ui.R so the base navigation
# remains unchanged except for one additional Toolkits entry.

app_ui <- function(request) {
  shiny::tagList(
    shinyjs::useShinyjs(),
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
        value = "project_init",
        icon = bsicons::bs_icon("gear"),
        project_init_ui("project_init")
      ),

      bslib::nav_panel(
        "Search",
        value = "sage_search",
        icon = bsicons::bs_icon("search"),
        sage_search_ui("sage_search")
      ),

      bslib::nav_menu(
        "Pre-processing",
        icon = bsicons::bs_icon("wrench"),
        bslib::nav_panel(
          "MaxQuant Output Preparation",
          value = "data_input",
          icon = bsicons::bs_icon("usb-drive"),
          data_input_ui("data_input")
        ),
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
        bslib::nav_panel("Overview", icon = bsicons::bs_icon("clipboard-data"), overview_ui("overview")),
        bslib::nav_panel("DEP analysis", icon = bsicons::bs_icon("graph-up-arrow"), DEP_analysis_ui("DEP_analysis")),
        bslib::nav_panel(
          "Enrichment analysis",
          icon = bsicons::bs_icon("diagram-3"),
          enrichment_analysis_ui("enrichment_analysis")
        ),
        bslib::nav_panel("GSEA analysis", icon = bsicons::bs_icon("activity"), gsea_ui("gsea")),
        bslib::nav_panel("Pathview", icon = bsicons::bs_icon("signpost-2"), pathview_ui("pathview"))
      ),

      bslib::nav_menu(
        "Multi-omics",
        icon = bsicons::bs_icon("database-gear"),
        bslib::nav_panel(
          "Expression Profile",
          icon = bsicons::bs_icon("bezier2"),
          Expression_profile_ui("Expression_profile")
        ),
        bslib::nav_panel(
          "WGCNA",
          icon = bsicons::bs_icon("diagram-2"),
          wgcna_ui("wgcna")
        ),
        bslib::nav_panel(
          "Metaproteomics",
          icon = bsicons::bs_icon("layers"),
          metaproteomics_ui("metaproteomics")
        ),
        bslib::nav_panel("Nine Quadrant", icon = bsicons::bs_icon("grid-3x3-gap"), nine_quadrant_ui("nine")),
        bslib::nav_panel(
          title = "Co-enrichment",
          icon = bsicons::bs_icon("diagram-3"),
          co_enrichment_ui("co_enrichment")
        ),
        bslib::nav_panel("Venn", icon = bsicons::bs_icon("diagram-3-fill"), venn_ui("venn"))
      ),

      bslib::nav_panel(
        "PTM",
        icon = bsicons::bs_icon("layers"),
        PTM_ui("PTM")
      ),

      bslib::nav_panel(
        "Release data",
        icon = bsicons::bs_icon("folder2-open"),
        release_data_ui("release_data1")
      ),

      bslib::nav_menu(
        "Toolkits",
        icon = bsicons::bs_icon("tools"),
        bslib::nav_panel(
          "Protein Workbench",
          icon = bsicons::bs_icon("collection"),
          protein_workbench_ui("protein_workbench")
        ),
        bslib::nav_panel("Protein Extract", icon = bsicons::bs_icon("file-earmark-medical"), protein_extract_ui("protein_extract")),
        bslib::nav_panel("Plant-mPLoc", icon = bsicons::bs_icon("geo-alt"), plant_mploc_ui("plant_mploc")),
        bslib::nav_panel("Background Make", icon = bsicons::bs_icon("collection"), background_make_ui("background_make")),
        bslib::nav_panel("Protein Links", icon = bsicons::bs_icon("link-45deg"), protein_links_ui("prot_links")),
        bslib::nav_panel("Protein Structure", icon = bsicons::bs_icon("diagram-3"), protein_structure_ui("protein_structure")),
        bslib::nav_panel("Boxplot", icon = bsicons::bs_icon("box"), boxplot_module_ui("box1")),
        bslib::nav_panel("swissmodel", icon = bsicons::bs_icon("bezier"), swissmodel_ui("swissmodel")),
        bslib::nav_panel(
          "STRINGdb PPI",
          icon = bsicons::bs_icon("diagram-3"),
          stringdb_ppi_ui("stringdb_ppi")
        ),
        bslib::nav_panel(
          "Stacked Column Diagram",
          icon = bsicons::bs_icon("bar-chart-steps"),
          stacked_column_chart_ui("stacked_column_chart")
        ),
        bslib::nav_panel("Correlation chord", icon = bsicons::bs_icon("circle"), correlation_chord_ui("correlation_chord")),
        bslib::nav_panel("DEG Analyse", icon = bsicons::bs_icon("bar-chart-line"), DEG_ui("DEG"))
      ),

      help_ui()
    )
  )
}
