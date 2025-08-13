#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
#'
options(shiny.maxRequestSize = 300 * 1024^2)
# UI
DEP_analysis_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'DEP analysis',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'Expression matrix',
            multiple = FALSE,
            accept = '.csv'
          ),
          fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          ),
          fileInput(
            inputId = ns('CompareGroup'),
            label = 'Compare Group information',
            multiple = FALSE,
            accept = '.csv'
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                textInput(
                  inputId = "LogFC",
                  label = "LogFC:",
                  value = "1"
                ),
                textInput(
                  inputId = "Pvalue",
                  label = "Pvalue:",
                  value = "0.05"
                ),
              ),
              accordion_panel(
                title = "Run",
                actionButton(ns("run_button"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("download"), label = "Download Table", icon = icon("download"))
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  title = "Expression matrix",
                  DT::DTOutput(ns("Expression_dataTable"))
                ),
                tabPanel(
                  title = "Group information",
                  DT::DTOutput(ns("Group_dataTable"))
                ),
                tabPanel(
                  title = "Compare Group information",
                  DT::DTOutput(ns("Compare_Group_dataTable"))
                ),
                tabPanel(
                  title = "Result",
                  uiOutput(ns("dynamic_tabs"))  # 动态生成的标签栏
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server
#' @import limma

utils::globalVariables(c("Accession", "judge", "sample_name",
                         "value2"))
DEP_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 上传表达矩阵
    data <- reactive({
      req(input$file)
      utils::read.csv(input$file$datapath)
    })

    # 上传分组信息
    group <- reactive({
      req(input$GroupInfo)
      utils::read.csv(input$GroupInfo$datapath)
    })

    # 上传Compare Group信息
    compare_group <- reactive({
      req(input$CompareGroup)
      utils::read.csv(input$CompareGroup$datapath)
    })

    # 显示上传的表格
    output$Expression_dataTable <- DT::renderDT({
      req(data())
      DT::datatable(data())
    })

    output$Group_dataTable <- DT::renderDT({
      req(group())
      DT::datatable(group())
    })

    output$Compare_Group_dataTable <- DT::renderDT({
      req(compare_group())
      DT::datatable(compare_group())
    })

    # 动态生成标签栏
    output$dynamic_tabs <- renderUI({
      req(compare_group())

      # 获取 CompareGroup 数据的行数
      num_groups <- nrow(compare_group())

      # 创建动态的 tabPanel，标题为 compare_group[i,1] vs compare_group[i,2]
      tab_panels <- lapply(1:num_groups, function(i) {
        group_name <- paste0(compare_group()[i, 1], "_vs_", compare_group()[i, 2])

        tabPanel(
          title = group_name,
          DT::DTOutput(ns(paste0("DEP_dataTable_", i)))  # 每个标签栏有独立的 DT 输出
        )
      })

      # 返回动态生成的标签栏
      base::do.call(tabsetPanel, c(list(type = "tabs"), tab_panels))
    })

    # 运行差异表达分析
    observeEvent(input$run_button, {
      req(data(), group(), compare_group())

      # 获取表达矩阵，分组信息，和比较组信息
      exp_mat <- data() %>%
        dplyr::mutate(dplyr::across(everything(), ~ ifelse(is.na(.),0,.))) %>%
        tidyr::pivot_longer(!Accession,names_to = 'sample_name',values_to = 'value') %>%
        dplyr::left_join(group(), by = c("sample_name" = "Sample")) %>%
        dplyr::group_by(Accession,Group) %>%
        dplyr::mutate(judge = case_when(
          value == 0 ~ 0, # 没有表达量的统计为0
          TRUE ~ 1# 有表达量的统计为1
        ),
        judge2 = sum(judge) # 计算一下3个重复中有几个有表达量
        ) %>%
        dplyr::mutate(value2 = dplyr::case_when(
          judge == 0 & judge2 == 2 ~ sum(value)/2, # 如果这个值是0，但是3个重复中有2个有表达量，那么就给他去平均值替换0
          judge == 0 & judge2 == 1 ~ 0, # 如果这个值是0，但是3个重复中只有1个有表达量，那么认为是噪音，仍旧给他取0
          judge == 1 & judge2 == 1 ~ 0, # 如果这个值是1（有表达量），三个重复只有一个有表达量，那么认为是噪音，把他原始的值替换成0
          TRUE ~ value # 其他情况一律保留原始的值
        )) %>%
        dplyr::ungroup() %>%
        dplyr::select(Accession,sample_name,value2) %>%
        tidyr::pivot_wider(names_from = sample_name,values_from = value2)
      group_info <- group()
      compare_info <- compare_group()

      # 根据用户输入的LogFC和P值设置阈值
      logFC_threshold <- as.numeric(input$LogFC)
      pvalue_threshold <- as.numeric(input$Pvalue)

      # 处理每个比较组
      lapply(1:nrow(compare_info), function(i) {
        group1 <- compare_info[i, 1]
        group2 <- compare_info[i, 2]
        select_sample <- group() %>%
          dplyr::filter(Group == group1 | Group == group2) %>%
          dplyr::pull(Sample)
        # 选择对应的样本数据
        select_data <- exp_mat %>%
          dplyr::select(Accession,all_of(select_sample)) %>%
          tibble::column_to_rownames("Accession")
        print(select_data)
        # 创建设计矩阵
        group_list <- factor(c(rep(group1, ncol(select_data) / 2), rep(group2, ncol(select_data) / 2)))
        design <- stats::model.matrix(~ group_list + 0)
        colnames(design) <- c(group1, group2)

        # 构建线性模型并计算对比
        fit <- limma::lmFit(select_data,design)
        contrast_matrix <- limma::makeContrasts(contrasts = paste0(group1, "-", group2), levels = design)
        fit2 <- limma::contrasts.fit(fit, contrast_matrix)
        fit2 <- limma::eBayes(fit2)

        # 获取差异表达分析的结果
        results <- limma::topTable(fit2, coef = 1, number = Inf, adjust.method = "BH", sort.by = "logFC")
        print(results)
        # # 筛选差异表达基因
        # dif <- results %>%
        #   filter(adj.P.Val <= pvalue_threshold & abs(logFC) > log2(logFC_threshold))

        # 渲染数据表
        output[[paste0("DEP_dataTable_", i)]] <- DT::renderDT({
          DT::datatable(
            results,
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',
              buttons = list(
                list(
                  extend = 'csv',
                  text = 'Download CSV',
                  filename = paste('DEP_data_',
                                   compare_group()[i, 1],
                                   "_vs_",
                                   compare_group()[i, 2]), # 可以设置你想要的文件名
                  exportOptions = list(
                    modifier = list(
                      page = 'all'))
                )
              )
            )
          )
        })
      })
    })



  })
}
