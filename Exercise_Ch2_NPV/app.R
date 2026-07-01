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
  # NPV = sum_{t=0}^T CF_t / (1+r)^t，第0年指数恰为0，不折现
  sum(flows / (1 + rate)^(seq_along(flows) - 1))
}

# 在指定利率网格内查找 NPV_A = NPV_B 的临界折现率
find_crossover_rates <- function(flows_A, flows_B, rates = seq(0, 0.30, 0.005)) {
  diff <- flows_A - flows_B
  npv_diff <- function(r) sum(diff / (1 + r)^(seq_along(diff) - 1))
  vals <- vapply(rates, npv_diff, numeric(1))
  
  crosses <- c()
  # 边界 r = 0 恰为交点
  if (abs(vals[1]) < 1e-6) crosses <- c(crosses, rates[1])
  
  for (i in seq_len(length(rates) - 1)) {
    if (!is.na(vals[i]) && !is.na(vals[i + 1]) && vals[i] * vals[i + 1] < 0) {
      root <- uniroot(npv_diff, c(rates[i], rates[i + 1]), tol = 1e-6)$root
      crosses <- c(crosses, root)
    }
  }
  if (length(crosses) == 0) NA_real_ else crosses
}

# 教材第二章习题 10 默认数据（单位：万元）
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
      .formula-box {
        background: #f8fafc; border: 1px solid #d9e2ec;
        border-radius: 10px; padding: 14px 16px; line-height: 1.8;
      }
      .sidebar-note {
        font-size: 13px; color: #5b7083; line-height: 1.7;
      }
    "))
  ),
  
  titlePanel(div(class = "title-main", withMathJax("确定型决策分析教学网页：习题10 互斥投资方案 NPV 比较"))),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    本应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("折现率设置"),
      numericInput("r1", "折现率 1 (%)", value = 10, min = -99, max = 100, step = 1),
      numericInput("r2", "折现率 2 (%)", value = 20, min = -99, max = 100, step = 1),
      
      tags$hr(),
      h4("操作"),
      actionButton("reset_default", "恢复教材默认值", class = "btn-warning"),
      tags$hr(),
      h4("方案现金流"),
      div(class = "sidebar-note",
          HTML("
            <b>现金流输入说明：</b><br/>
            1. <b>年份 0</b> 为初始投资，通常为负值，<b>不折现</b>；<br/>
            2. 年份 1~6 为后续年度现金流，按 $$NPV=\\sum_{t=0}^{T}\\frac{CF_t}{(1+r)^t}$$ 折现；<br/>
            3. 所有金额单位均为<b>万元</b>。
          ")
      ),
      
      tags$hr(),
      actionButton("calculate", "计算 NPV 并比较", class = "btn-success")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("案例说明", br(),
                 div(class = "info-box",
                     h4("问题背景"),
                     p("两个互斥投资方案 A 和 B，有关投资信息如表 2-17 所示。试在折现率为 10% 和 20% 的情况下，用净现值法对两方案进行决策分析。"),
                     tags$details(
                       tags$summary("查看计算说明"),
                       br(),
                       tags$ul(
                         tags$li("净现值公式：$$NPV=\\sum_{t=0}^{T}\\frac{CF_t}{(1+r)^t}$$，其中第 0 年不折现。"),
                         tags$li("互斥方案比较以 NPV 较高者优先，不要与内部收益率 IRR 的高低混淆。"),
                         tags$li("若允许放弃投资，两个方案 NPV 均 ≤ 0 时选择“不投资”；若题目要求必须二选一，则选 NPV 较高者。"),
                         tags$li("临界折现率是两方案 NPV 曲线的交点，可能不存在或多个，图中只标出搜索范围内的交点。")
                       )
                     ),
                     tags$hr(),
                     h4("核心教学目标"),
                     withMathJax(),
                     tags$ul(
                       tags$li("理解净现值 $$NPV=\\sum_{t=0}^{T}\\frac{CF_t}{(1+r)^t}$$ 的经济含义与计算方法；"),
                       tags$li("明确第 0 年初始投资不折现，后续年度现金流按复利折现；"),
                       tags$li("比较不同折现率下互斥投资方案的优劣，并识别临界折现率；"),
                       tags$li("当两个方案 NPV 均为负时，能够判断“不投资”更优。")
                     ),
                     tags$hr(),
                     h4("默认现金流"),
                     p("单位：万元。年份 0 为初始投资，年份 1~6 为经营期现金流。")
                 )
        ),
        tabPanel("现金流输入", br(),
                 div(class = "info-box",
                     h4("方案 A 现金流（万元）"),
                     DTOutput("table_A")
                 ),
                 div(class = "info-box",
                     h4("方案 B 现金流（万元）"),
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
                 h4("NPV 比较与决策表"),
                 DTOutput("npv_table"),
                 br(),
                 uiOutput("recommend")
        ),
        tabPanel("图形分析", br(),
                 plotOutput("npv_plot", height = "460px"),
                 br(),
                 uiOutput("crossover_note")
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
    df_A = data.frame(年份 = default_years, 现金流_万元 = default_A),
    df_B = data.frame(年份 = default_years, 现金流_万元 = default_B)
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
  
  observeEvent(input$reset_default, {
    rv$df_A <- data.frame(年份 = default_years, 现金流_万元 = default_A)
    rv$df_B <- data.frame(年份 = default_years, 现金流_万元 = default_B)
    updateNumericInput(session, "r1", value = 10)
    updateNumericInput(session, "r2", value = 20)
  })
  
  calc <- eventReactive(input$calculate, {
    # 折现率边界校验
    if (input$r1 <= -100 || input$r2 <= -100) {
      showNotification("折现率不能小于或等于 -100%，请重新输入。", type = "error")
      return(NULL)
    }
    r1 <- input$r1 / 100
    r2 <- input$r2 / 100
    
    flows_A <- rv$df_A$现金流_万元
    flows_B <- rv$df_B$现金流_万元
    
    if (any(is.na(flows_A)) || any(is.na(flows_B))) {
      showNotification("现金流存在空值或非数值，请检查并补全。", type = "error")
      return(NULL)
    }
    
    npv_A1 <- npv(flows_A, r1)
    npv_A2 <- npv(flows_A, r2)
    npv_B1 <- npv(flows_B, r1)
    npv_B2 <- npv(flows_B, r2)
    
    # 决策辅助函数：允许“不投资”
    decision <- function(npv_A, npv_B) {
      if (max(npv_A, npv_B) <= 0) {
        list(best = "不投资", invest = "否",
             explain = sprintf("方案 A NPV = %.2f 万元，方案 B NPV = %.2f 万元，两方案 NPV 均 ≤ 0。若允许放弃投资，选择“不投资”更优；若题目要求必须二选一，则选 NPV 较高者。",
                               npv_A, npv_B))
      } else if (npv_A > npv_B) {
        list(best = "A", invest = "是",
             explain = sprintf("方案 A 的 NPV（%.2f 万元）高于方案 B（%.2f 万元），推荐投资方案 A。",
                               npv_A, npv_B))
      } else if (npv_B > npv_A) {
        list(best = "B", invest = "是",
             explain = sprintf("方案 B 的 NPV（%.2f 万元）高于方案 A（%.2f 万元），推荐投资方案 B。",
                               npv_B, npv_A))
      } else {
        list(best = "A/B 无差异", invest = "是",
             explain = sprintf("两个方案在该折现率下 NPV 相等（%.2f 万元），无差异。", npv_A))
      }
    }
    
    list(
      r1 = r1, r2 = r2,
      npv_A1 = npv_A1, npv_A2 = npv_A2,
      npv_B1 = npv_B1, npv_B2 = npv_B2,
      flows_A = flows_A, flows_B = flows_B,
      dec_r1 = decision(npv_A1, npv_B1),
      dec_r2 = decision(npv_A2, npv_B2)
    )
  })
  
  output$metric_A1 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 A @ %.0f%%（万元）", input$r1)),
        div(class = "metric-value", fmt_num(calc()$npv_A1))
    )
  })
  
  output$metric_A2 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 A @ %.0f%%（万元）", input$r2)),
        div(class = "metric-value", fmt_num(calc()$npv_A2))
    )
  })
  
  output$metric_B1 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 B @ %.0f%%（万元）", input$r1)),
        div(class = "metric-value", fmt_num(calc()$npv_B1))
    )
  })
  
  output$metric_B2 <- renderUI({
    req(calc())
    div(class = "metric-card",
        div(class = "metric-title", sprintf("方案 B @ %.0f%%（万元）", input$r2)),
        div(class = "metric-value", fmt_num(calc()$npv_B2))
    )
  })
  
  output$npv_table <- renderDT({
    req(calc())
    make_row <- function(r, npv_A, npv_B, dec) {
      data.frame(
        折现率 = paste0(r * 100, "%"),
        方案A_NPV_万元 = npv_A,
        方案B_NPV_万元 = npv_B,
        较优方案 = dec$best,
        是否建议投资 = dec$invest,
        决策解释 = dec$explain,
        stringsAsFactors = FALSE
      )
    }
    
    df <- rbind(
      make_row(calc()$r1, calc()$npv_A1, calc()$npv_B1, calc()$dec_r1),
      make_row(calc()$r2, calc()$npv_A2, calc()$npv_B2, calc()$dec_r2)
    )
    
    datatable(df, options = list(paging = FALSE, searching = FALSE), rownames = FALSE) %>%
      formatRound(columns = c("方案A_NPV_万元", "方案B_NPV_万元"), digits = 2)
  })
  
  output$recommend <- renderUI({
    req(calc())
    div(class = "info-box",
        h4("综合决策建议"),
        p(sprintf("折现率 %.0f%%：%s", input$r1, calc()$dec_r1$explain)),
        p(sprintf("折现率 %.0f%%：%s", input$r2, calc()$dec_r2$explain)),
        p("提示：若两个折现率下推荐方案不同，说明方案选择对折现率敏感，可结合“图形分析”中的方案转换点进一步判断。互斥项目应以 NPV 较高者优先，不要简单按“收益率高低”选择。")
    )
  })
  
  output$npv_plot <- renderPlot({
    req(calc())
    rates <- seq(0, 30, 0.5) / 100
    npv_A <- vapply(rates, function(r) npv(calc()$flows_A, r), numeric(1))
    npv_B <- vapply(rates, function(r) npv(calc()$flows_B, r), numeric(1))
    
    df <- data.frame(
      折现率 = rep(rates * 100, 2),
      NPV_万元 = c(npv_A, npv_B),
      方案 = rep(c("A", "B"), each = length(rates))
    )
    
    cross_rates <- find_crossover_rates(calc()$flows_A, calc()$flows_B, rates)
    
    p <- ggplot(df, aes(x = 折现率, y = NPV_万元, color = 方案)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = c(input$r1, input$r2), linetype = "dotted", color = "gray40") +
      labs(title = "NPV 随折现率变化曲线",
           subtitle = "虚线：两方案 NPV 相等时的方案转换点；点线：用户设置的折现率",
           x = "折现率 (%)",
           y = "NPV（万元）") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    if (!all(is.na(cross_rates))) {
      cross_y <- max(c(npv_A, npv_B), na.rm = TRUE) * 0.9
      p <- p +
        geom_vline(xintercept = cross_rates * 100, linetype = "longdash", color = "#b42318") +
        annotate("text",
                 x = cross_rates * 100, y = cross_y,
                 label = paste0("临界折现率\n", sprintf("%.2f%%", cross_rates * 100)),
                 color = "#b42318", hjust = -0.05, vjust = 1, size = 3.5)
    }
    
    p
  })
  
  output$crossover_note <- renderUI({
    req(calc())
    cross_rates <- find_crossover_rates(calc()$flows_A, calc()$flows_B, seq(0, 0.30, 0.005))
    if (all(is.na(cross_rates))) {
      div(class = "info-box", p("在 0%~30% 范围内未找到两方案 NPV 相等的方案转换点（除 r=0 外），说明在此区间内某一方案始终占优。复杂现金流下可能存在多个转换点，图中仅标出搜索范围内的交点。"))
    } else {
      div(class = "info-box",
          p(sprintf("在 0%%~30%% 范围内，两方案 NPV 相等的方案转换点（临界折现率）为：%s。",
                    paste(sprintf("%.2f%%", cross_rates * 100), collapse = ", "))),
          p("方案转换点左侧与右侧可能对应不同的最优方案；若现金流更复杂，也可能存在多个转换点。教学中可据此讨论折现率敏感度。")
      )
    }
  })
}

shinyApp(ui, server)
