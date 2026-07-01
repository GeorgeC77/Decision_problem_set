# 依赖包：shiny
# install.packages("shiny")

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f7f9fb; }
      .title-main { font-weight: 700; color: #1f3b5b; }
      .copyright-box {
        background: #fff3cd; border: 1px solid #f0d98c; border-radius: 8px;
        padding: 12px 16px; margin: 8px 0 16px 0; font-size: 14px;
        line-height: 1.7; color: #7a4b00;
      }
      .info-box {
        background: white; border: 1px solid #d9e2ec; border-radius: 10px;
        padding: 16px 18px; margin-bottom: 14px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
      }
      .component-card {
        background: #eef6ff; border: 1px solid #cfe2ff; border-radius: 8px;
        padding: 10px; margin-bottom: 8px; cursor: pointer;
      }
      .component-card.selected {
        background: #d4edda; border-color: #c3e6cb;
      }
    "))
  ),
  
  titlePanel(div(class = "title-main", "决策支持系统教学网页：DSS 架构认知")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《决策支持系统教学网页：DSS 架构认知》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("DSS 基本组成"),
      checkboxGroupInput(
        "components",
        "请选择你认为一个完整 DSS 应包含的子系统：",
        choices = c(
          "数据库管理系统 DBMS" = "dbms",
          "模型库管理系统 MBMS" = "mbms",
          "方法库管理系统 MEBMS" = "mebms",
          "对话管理系统 DGMS" = "dgms",
          "知识库系统" = "kb",
          "操作系统" = "os"
        ),
        selected = NULL
      ),
      actionButton("check", "检查答案", class = "btn-success")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "知识要点",
          br(),
          div(
            class = "info-box",
            h4("决策支持系统（DSS）的核心结构"),
            tags$p("教材第八章指出，DSS 是以管理学、运筹学、控制论和行为科学为基础，以信息、仿真和计算机等技术为手段，综合利用数据、信息和模型，辅助决策解决半结构化或非结构化决策问题的人机交互系统。"),
            tags$hr(),
            h4("三库结构"),
            tags$ul(
              tags$li("数据库管理系统（DBMS）：存储和管理决策所需的数据。"),
              tags$li("模型库管理系统（MBMS）：存储和管理各种决策模型。"),
              tags$li("方法库管理系统（MEBMS）：存储和管理求解模型所需的方法/算法。"),
              tags$li("对话管理系统（DGMS）：提供用户与系统之间的交互界面。")
            ),
            tags$p("有些 DSS 还会加入知识库，用于处理更复杂的非结构化问题。")
          )
        ),
        
        tabPanel(
          "互动测试",
          br(),
          div(
            class = "info-box",
            h4("你的选择"),
            verbatimTextOutput("selected_text")
          ),
          div(
            class = "info-box",
            h4("检查结果"),
            uiOutput("check_result")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  correct <- c("dbms", "mbms", "mebms", "dgms")
  optional <- "kb"
  
  output$selected_text <- renderText({
    if (length(input$components) == 0) return("尚未选择任何组件。")
    labels <- c(
      dbms = "数据库管理系统 DBMS",
      mbms = "模型库管理系统 MBMS",
      mebms = "方法库管理系统 MEBMS",
      dgms = "对话管理系统 DGMS",
      kb = "知识库系统",
      os = "操作系统"
    )
    paste(labels[input$components], collapse = "\n")
  })
  
  output$check_result <- renderUI({
    input$check
    sel <- isolate(input$components)
    if (length(sel) == 0) {
      return(div("请先选择组件再点击检查。"))
    }
    
    has_all_core <- all(correct %in% sel)
    has_os <- "os" %in% sel
    has_kb <- "kb" %in% sel
    
    msg <- character()
    if (has_all_core) {
      msg <- c(msg, "✅ 你已选全 DSS 的四个核心子系统（DBMS、MBMS、MEBMS、DGMS）。")
    } else {
      missing <- setdiff(correct, sel)
      msg <- c(msg, paste0("❌ 还缺少核心子系统：", paste(missing, collapse = "、")))
    }
    if (has_kb) {
      msg <- c(msg, "ℹ️ 知识库系统可作为扩展组件，用于处理非结构化问题。")
    }
    if (has_os) {
      msg <- c(msg, "❌ 操作系统是计算机系统的基础软件，不是 DSS 特有的子系统。")
    }
    
    div(
      class = "info-box",
      HTML(paste(msg, collapse = "<br/>"))
    )
  })
}

shinyApp(ui, server)
