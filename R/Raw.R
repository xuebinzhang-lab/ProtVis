#' 临时 Raw 数据模块UI
#' @param id 模块ID
Raw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload Raw Data File"),
    verbatimTextOutput(ns("file_info"))
  )
}

#' 临时 Raw 数据模块服务器逻辑
#' @param id 模块ID
Raw_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 临时实现：仅显示上传的文件信息
    output$file_info <- renderPrint({
      req(input$file)
      cat("Raw data file uploaded:\n")
      cat("Name:", input$file$name, "\n")
      cat("Size:", format(object.size(input$file$datapath), units = "auto"), "\n")
      cat("Type:", tools::file_ext(input$file$name), "\n")
    })

    # 返回上传的数据（实际应用中替换为真实处理逻辑）
    return(reactive({
      req(input$file)
      list(
        path = input$file$datapath,
        name = input$file$name
      )
    }))
  })
}
