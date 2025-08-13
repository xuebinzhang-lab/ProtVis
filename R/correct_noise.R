# correct_values = function(raw_mat) {
#   clean_mat<-mat_step2
#   rep1 <- clean_mat %>% dplyr::select(ID,contains('_1')) %>% dplyr::rowwise() %>% dplyr::filter(any(c_across(!ID) != 0))
#   rep2 <- clean_mat %>% dplyr::select(ID,contains('_2')) %>% dplyr::rowwise() %>% dplyr::filter(any(c_across(!ID) != 0))
#   rep3 <- clean_mat %>% dplyr::select(ID,contains('_3')) %>% dplyr::rowwise() %>% dplyr::filter(any(c_across(!ID) != 0))
#   mv_mat <- dplyr::full_join(rep1,rep2) %>% dplyr::full_join(.,rep3)
#   final_mat <- mv_mat %>%
#     data.table::setDT() %>%
#     data.table::melt(id.vars = "ID", variable.name = "sample", value.name = "value") %>%
#     .[is.na(value), value := 0] %>%
#     .[, `:=`(sample_group = sub("..$", "", sample),
#              tag = ifelse(value > 0, 1, 0))] %>%
#     .[, value_fix := {
#       tag_sum <- sum(tag)
#       ifelse(tag_sum >= 2 & value == 0, sum(value[tag == 1])/2,
#              ifelse(tag_sum >= 2 & value != 0, value,
#                     ifelse(tag_sum == 3, value, 0)))
#     }, by = .(ID, sample_group)] %>%
#     .[, .(ID, sample, value_fix)] %>%
#     reshape2::dcast(ID ~ sample, value.var = "value_fix") %>%
#     as.data.frame() %>%
#     dplyr::mutate(across(everything(), ~ifelse(. == 0,NA,.)))
#
#   return(final_mat)
# }

correct_values <- function(raw_mat) {
  clean_mat <- raw_mat

  rep1 <- dplyr::select(clean_mat, ID, dplyr::contains('_1')) %>%
    dplyr::rowwise() %>%
    dplyr::filter(any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  rep2 <- dplyr::select(clean_mat, ID, dplyr::contains('_2')) %>%
    dplyr::rowwise() %>%
    dplyr::filter(any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  rep3 <- dplyr::select(clean_mat, ID, dplyr::contains('_3')) %>%
    dplyr::rowwise() %>%
    dplyr::filter(any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  mv_mat <- dplyr::full_join(rep1, rep2, by = "ID") %>%
    dplyr::full_join(., rep3, by = "ID")

  long_df <- tidyr::pivot_longer(mv_mat,
                                 cols = -ID,
                                 names_to = "sample",
                                 values_to = "value") %>%
    dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
    dplyr::mutate(
      sample_group = stringr::str_sub(sample, 1, -3),
      tag = dplyr::if_else(value > 0, 1L, 0L)
    )

  value_fix_df <- long_df %>%
    dplyr::group_by(ID, sample_group) %>%
    dplyr::mutate(
      tag_sum = sum(tag),
      value_fix = dplyr::case_when(
        tag_sum >= 2 & value == 0 ~ sum(value[tag == 1]) / 2,
        tag_sum >= 2 & value != 0 ~ value,
        tag_sum == 3 ~ value,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID, sample, value_fix)

  final_mat <- tidyr::pivot_wider(value_fix_df,
                                  names_from = sample,
                                  values_from = value_fix) %>%
    dplyr::mutate(dplyr::across(-ID, ~dplyr::na_if(., 0)))

  return(final_mat)
}


correct_noise_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
      uiOutput(ns("load_status_panel")),
      accordion(
        accordion_panel(
          title = "Step1 Rename Columns",
          icon = bs_icon("tools"),
          tagList(
            shinyWidgets::switchInput(
              inputId = ns("rename_columns"),
              label = "Rename Columns",
              value = FALSE,
              onLabel = "✔",
              offLabel = "✘",
              size = "small",
              labelWidth = "120px",
              handleWidth = 60
            )
          )
        ),
        accordion_panel(
          title = "Step2 Correct Noise",
          icon = bs_icon("tools"),
          tagList(
            shinyWidgets::switchInput(
              inputId = ns("correct_noise"),
              label = "Correct Noise",
              value = FALSE,
              onLabel = "✔",
              offLabel = "✘",
              size = "small",
              labelWidth = "120px",
              handleWidth = 60
            )
          )
        )
      )
    ),
    div(
      card(
        card_header("Preview the data processing process"),
        card_body(
          fill = TRUE,
          navset_tab(
            id = ns("Expression_Matrix"),
            nav_panel("Sample info",
                      uiOutput(ns("sample_info_ui"))
            ),
            nav_panel("Expression Matrix",
                      htmlOutput(ns("matrix_check")),
                      DT::dataTableOutput(ns("expression_matrix_filtered"))
            ),
            nav_panel("Rename Columns",
                      DT::dataTableOutput(ns("tbl_rename_columns"))
            ),
            nav_panel("Correct Noise",
                      DT::dataTableOutput(ns("tbl_correct_noise"))
            )
          )
        )
      )
    ),
    actionButton(ns("export_correct_noise"), "export data", class = "btn btn-light fw-bold"),
    uiOutput(ns("export_correct_noise_status_panel"))
  )
}

correct_noise_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(load_success = FALSE)

    observeEvent(input$load_data, {
      req(shared_state$workdir)

      rda_path <- file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)

        if (exists("sample_info", envir = e)) {
          shared_state$sample_info <- e$sample_info
        }
        if (exists("expression_matrix_filtered", envir = e)) {
          shared_state$expression_matrix_filtered <- e$expression_matrix_filtered
        }

        rv$load_success <- TRUE
        showNotification("✅ Step2 data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("❌ Step2_remove_unreliable_peptide.rda not found in working directory.", type = "error")
      }
    })

    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$sample_info_ui <- renderUI({
      req(shared_state$sample_info)
      DT::dataTableOutput(ns("tbl_sample_info"))
    })
    output$tbl_sample_info <- DT::renderDataTable({
      req(shared_state$sample_info)
      DT::datatable(shared_state$sample_info, options = list(pageLength = 10))
    })

    output$expression_matrix_filtered <- DT::renderDataTable({
      req(shared_state$expression_matrix_filtered)
      DT::datatable(shared_state$expression_matrix_filtered, options = list(scrollX = TRUE, pageLength = 10))
    })

    correct_noise_step1 <- reactive({
      req(shared_state$expression_matrix_filtered, shared_state$sample_info)
      new_name <- left_join(
        data.frame(maxquant_id = colnames(shared_state$expression_matrix_filtered)[-1]),
        shared_state$sample_info
      ) %>% pull(sample_id)

      shared_state$expression_matrix_filtered %>%
        setNames(c("ID", new_name))
    })

    # 根据 rename_columns 开关决定是否显示重命名表格
    output$tbl_rename_columns <- DT::renderDataTable({
      req(correct_noise_step1())
      if (isTRUE(input$rename_columns)) {
        DT::datatable(correct_noise_step1(), options = list(scrollX = TRUE, pageLength = 10))
      } else {
        NULL
      }
    })

    # 根据 correct_noise 开关决定是否显示校正噪声表格
    output$tbl_correct_noise <- DT::renderDataTable({
      req(correct_noise_step1())
      if (isTRUE(input$correct_noise)) {
        correct_noise_step1() %>%
          correct_values() %>%
          tibble::column_to_rownames("ID") %>%
          DT::datatable(options = list(scrollX = TRUE, pageLength = 10))
      } else {
        NULL
      }
    })

    # -------------------------------------------------------------------------

    corrected_matrix <- reactive({
      req(correct_noise_step1())
      correct_values(correct_noise_step1())
    })

    observeEvent(input$export_correct_noise, {
      req(rv$load_success, shared_state$workdir, shared_state$sample_info, corrected_matrix())
      save_path <- file.path(shared_state$workdir, "Step3_correct_noise.rda")
      sample_info <- shared_state$sample_info
      correct_noise_result <- corrected_matrix()
      save(sample_info, correct_noise_result, file = save_path)
      showNotification(paste0("✅ Saved to ", save_path), type = "message")
    })



  })
}




