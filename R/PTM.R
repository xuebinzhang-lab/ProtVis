PTM_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # 使用 bslib::layout_sidebar 布局
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 300,
        accordion(
          accordion_panel(
            title = "Upload File",
            # icon = "upload",
            fileInput(ns("ptm_file"),
                      "Upload PTM Data",
                      accept = c(".csv", ".xlsx"),
                      multiple = FALSE)
          ),
          accordion_panel(
            title = "Ion Modifications",
            # icon = "plus",
            selectInput(ns("add_modification"),
                        label = "Select Ion(s) to Add",
                        choices = list(
                          "Add H⁺ (Protonation)" = "add_H_plus",
                          "Add H₂O (Hydration)" = "add_H2O",
                          "Add NH₃ (Ammonia)" = "add_NH3",
                          "Add CH₃ (Methylation)" = "add_methyl",
                          "Add H₂ (Hydrogenation)" = "add_H2",
                          "Add CO₂ (Decarboxylation)" = "add_CO2"
                        ),
                        multiple = TRUE  # Enable multiple selection
            ),
            selectInput(ns("remove_modification"),
                        label = "Select Ion(s) to Remove",
                        choices = list(
                          "Remove H⁺ (Deprotonation)" = "remove_H_plus",
                          "Remove H₂O (Dehydration)" = "remove_H2O",
                          "Remove NH₃ (Deammoniation)" = "remove_NH3",
                          "Remove CH₃ (Demethylation)" = "remove_methyl",
                          "Remove H₂ (Dehydrogenation)" = "remove_H2",
                          "Remove CO₂ (Decarboxylation)" = "remove_CO2"
                        ),
                        multiple = TRUE  # Enable multiple selection
            )
          ),
          accordion_panel(
            title = "Peptide Selection",
            # icon = "list",
            selectInput(ns("Peptide"),
                        "Select Peptide",
                        choices = c(
                          "GIVDQSQQAYQEAFEISK",
                          "ACCPLEGVRPSPQQTEYR",
                          "GLTMLDHEQVTPEDPGAQFLIR",
                          "ACFLMAHNGWVMGDDPLR",
                          "EILVGDVGQTVDDPYATFVK"),
                        selected = "ACCPLEGVRPSPQQTEYR")
          ),
          accordion_panel(
            title = "Modification Type",
            # icon = "cogs",
            selectInput(ns("mod_type"),
                        "Select Modification Type",
                        choices = c("Phosphorylation", "Ubiquitination", "Acetylation", "Methylation", "Glycosylation"),
                        selected = "Phosphorylation")
          ),
          accordion_panel(
            title = "Color Selection for Ions",
            # icon = "paint-brush",
            fluidRow(
              column(6,
                     tags$div(style = "font-weight: bold; text-align: left;", "b ion"),
                     colourpicker::colourInput(ns("color_up"),
                                               "",
                                               value = "blue")),  # Default blue
              column(6,
                     tags$div(style = "font-weight: bold; text-align: left;", "y ion"),
                     colourpicker::colourInput(ns("color_down"),
                                               "",
                                               value = "red"))  # Default red
            )
          ),
          accordion_panel(
            title = "Visualization",
            # icon = "eye",
            actionButton(ns("visualize"),
                         "VISUALIZE PTM"),
            downloadButton(ns("download_ptm_plot"),
                           "DOWNLOAD")
          )
        )
      ),

      # Main panel area for displaying visualization
      main = card(
        height = "500px",
        card_header("Protein Post-translational Modifications (PTMs) Visualization"),
        card_body(
          # Display the status or visualization here
          textOutput(ns("status_message"))
        )
      )
    )
  )
}
