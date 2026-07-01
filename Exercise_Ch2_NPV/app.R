# 依赖包：shiny, DT, ggplot2
# install.packages(c("shiny", "DT", "ggplot2"))

library(shiny)
library(DT)
library(ggplot2)

# =========================
# 工具函数
# =========================
fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits, big.mark = ","))
}

npv <- function(flows, rate) {
  sum(flows / (1 + rate)^(seq_along(flows) - 1))
}

# 教材第二章习题 10 默认数据
default_years <- 0:6
default_A <- c(-250, 100, 100, 100, 50, 50, 50)
default_B <- c(-100, 30, 30, 60, 60, 60, 60)

# =========================
# UI
# =========================
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
      .metric-card {
        background: white; border: 1px solid #d9e2ec; border-radius: 12px;
        padding: 14px 16px; margin-bottom: 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
        min-height: 100px;
      }
      .metric-title { color: #5b7083; font-size: 13px; margin-bottom: 8px; }
      .metric-value { font-size: 24px; font-weight: 700; color: #1f3b5b; }
    "))
  ),
  
  titlePanel(div(class = "title-main", "确定型决策分析教学网页：互斥投资方案 NPV 比较")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《确定型决策分析教学网页：互斥投资方案 NPV 比较》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("折现率设置"),
      numericInput("r1", "折现率 1 (%)", value = 10, min = 0, max = 100, step = 1),
      numericInput("r2", "折现率 2 (%)", value = 20, min = 0, max = 100, step = 1),
      
      tags$hr(),
      h4("方案现金流"),
      p("在右侧表格中编辑各年现金流，单位：万元"),
      
      tags$hr(),
      actionButton("calculate", "计算 NPV 并比较", class = "btn-success")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("案例说明", br(),
                 div(class = "info-box",
                     h4("教学目标"),
                     p("本章对应教材第二章“确定型决策分析”。通过本网页，学生可以："),
                     tags$ul(
                       tags$li("理解净现值（NPV）的经济含义与计算方法；"),
                       tags$li("比较不同折现率下互斥投资方案的优劣；"),
                       tags$li("观察折现率变化对方案选择的影响。")
                     ),
                     h4("教材题目"),
                     p("两个互斥投资方案 A 和 B，有关投资信息如表 2-17 所示。试在折现率为 10% 和 20% 的情况下，用净现值法对两方案进行决策分析。"),
                     p("默认现金流（单位：万元）已按教材预置，可直接点击“计算 NPV 并比较”。")
                 )
        ),
        tabPanel("现金流输入", br(),
                 div(class = "info-box",
                     h4("方案 A 现金流"),
                     DTOutput("table_A")
                 ),
                 div(class = "info-box",
                     h4("方案 B 现金流"),
                     DTOutput("table_B")
                 )
        ),
        tabPanel("计算结果", br(),
                 fluidRow(
                   column(3, uiOutput("metric_A1")),
                   column(3, uiOutput("metric_A2")),
                   column(3, uiOutput("metric_B1")),
                   column(3, uiOutput("metric_B2"))
                 ),
                 br(),
                 h4("NPV 比较表"),
                 DTOutput("npv_table"),
                 br(),
                 uiOutput("recommend")
        ),
        tabPanel("图形分析", br(),
                 plotOutput("npv_plot", height = "420px")
        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    df_A = data.frame(年份 = default_years, 现金流 = default_A),
    df_B = data.frame(年份 = default_years, 现金流 = default_B)
  )
  
  output$table_A <- renderDT({
    datatable(rv$df_A, editable = "cell", options = list(paging = FALSE, searching = FALSE),
              rownames = FALSE)
  })
  
  output$table_B <- renderDT({
    datatable(rv$df_B, editable = "cell", options = list(paging = FALSE, searching = FALSE),
              rownames = FALSE)
  })
  
  observeEvent(input$table_A_cell_edit, {
    info <- input$table_A_cell_edit
    rv$df_A[info$row, info$col + 1] <- as.numeric(info$value)
  })
  
  observeEvent(input$table_B_cell_edit, {
    info <- input$table_B_cell_edit
    rv$df_B[info$row, info$col + 1] <- as.numeric(info$value)
  })
  
  calc <- eventReactive(input$calculate, {
    r1 <- input$r1 / 100
    r2 <- input$r2 / 100
    
    flows_A <- rv$df_A$现金流
    flows_B <- rv$df_B$现金流
    
    npv_A1 <- npv(flows_A, r1)
    npv_A2 <- npv(flows_A, r2)
    npv_B1 <- npv(flows_B, r1)
    npv_B2 <- npv(flows_B, r2)
    
    list(
      r1 = r1, r2 = r2,
      npv_A1 = npv_A1, npv_A2 = npv_A2,
      npv_B1 = npv_B1, npv_B2 = npv_B2,
      flows_A = flows_A, flows_B = flows_B
    )
  })
  
  output$metric_A1 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 A @ %.0f%%", input$r1)),
        div(class = "metric-value", fmt_num(calc()$npv_A1))
    )
  })
  
  output$metric_A2 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 A @ %.0f%%", input$r2)),
        div(class = "metric-value", fmt_num(calc()$npv_A2))
    )
  })
  
  output$metric_B1 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 B @ %.0f%%", input$r1)),
        div(class = "metric-value", fmt_num(calc()$npv_B1))
    )
  })
  
  output$metric_B2 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 B @ %.0f%%", input$r2)),
        div(class = "metric-value", fmt_num(calc()$npv_B2))
    )
  })
  
  output$npv_table <- renderDT({
    req(calc())
    df <- data.frame(
      方案 = c("A", "B"),
      NPV_10 = c(calc()$npv_A1, calc()$npv_B1),
      NPV_20 = c(calc()$npv_A2, calc()$npv_B2)
    )
    colnames(df) <- c("方案", sprintf("NPV@%.0f%%", input$r1), sprintf("NPV@%.0f%%", input$r2))
    datatable(df, options = list(paging = FALSE, searching = FALSE), rownames = FALSE) %>%
      formatRound(columns = 2:3, digits = 2)
  })
  
  output$recommend <- renderUI({
    req(calc())
    rec_r1 <- ifelse(calc()$npv_A1 >= calc()$npv_B1, "A", "B")
    rec_r2 <- ifelse(calc()$npv_A2 >= calc()$npv_B2, "A", "B")
    div(class = "info-box",
        h4("推荐结果"),
        p(sprintf("折现率 %.0f%% 时，推荐方案 %s（NPV %.2f > %.2f）。",
                  input$r1, rec_r1, max(calc()$npv_A1, calc()$npv_B1), min(calc()$npv_A1, calc()$npv_B1))),
        p(sprintf("折现率 %.0f%% 时，推荐方案 %s（NPV %.2f > %.2f）。",
                  input$r2, rec_r2, max(calc()$npv_A2, calc()$npv_B2), min(calc()$npv_A2, calc()$npv_B2))),
        p("若两折现率下推荐方案不同，说明方案选择对折现率敏感，需进一步分析临界折现率。")
    )
  })
  
  output$npv_plot <- renderPlot({
    req(calc())
    rates <- seq(0, 30, 0.5) / 100
    npv_A <- sapply(rates, function(r) npv(calc()$flows_A, r))
    npv_B <- sapply(rates, function(r) npv(calc()$flows_B, r))
    df <- data.frame(
      折现率 = rep(rates * 100, 2),
      NPV = c(npv_A, npv_B),
      方案 = rep(c("A", "B"), each = length(rates))
    )
    ggplot(df, aes(x = 折现率, y = NPV, color = 方案)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = c(input$r1, input$r2) * 100, linetype = "dotted") +
      labs(title = "NPV 随折现率变化曲线", x = "折现率 (%)", y = "NPV（万元）") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
