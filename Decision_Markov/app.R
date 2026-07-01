# 依赖包：shiny, DT, ggplot2
# install.packages(c("shiny", "DT", "ggplot2"))

library(shiny)
library(DT)
library(ggplot2)

# =========================
# 马尔可夫决策辅助函数
# =========================

# 矩阵幂（避免依赖 expm）
mat_power <- function(P, n) {
  if (n == 0) return(diag(nrow(P)))
  result <- P
  if (n == 1) return(result)
  for (i in 2:n) {
    result <- result %*% P
  }
  result
}

# n 步转移概率
step_prob <- function(P, S0, n) {
  if (n == 0) return(S0)
  as.vector(S0 %*% mat_power(P, n))
}

# 稳定状态概率（解 S*P = S, sum(S)=1）
steady_state <- function(P) {
  n <- nrow(P)
  if (n != ncol(P)) return(rep(NA_real_, n))
  A <- t(P) - diag(n)
  A <- rbind(A, rep(1, n))
  b <- c(rep(0, n), 1)
  # 使用 QR 分解求解最小二乘问题，提高数值稳健性
  S <- qr.solve(A, b)
  S <- pmax(S, 0)
  S <- S / sum(S)
  as.vector(S)
}

# 两期期望利润（当前状态已知为 s0，前一状态为 s_prev）
expected_profit_2yr <- function(P, R, S0) {
  # 第1年利润：当前状态 s0 的利润与上一期状态无关？
  # 教材表6-13：利润与"本时期"和"前一时期"有关。
  # 为简化教学演示，采用稳态期望年利润近似：
  # 年期望利润 = sum_{i,j} S_i * P_{ij} * R_{ij}
  n <- nrow(P)
  exp <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      exp <- exp + S0[i] * P[i, j] * R[i, j]
    }
  }
  exp
}

# 教材第六章习题 7 默认数据
states <- c("畅销", "滞销")
# 利润矩阵 R[i,j]：上一期状态 i，本期状态 j 的利润
profit_mat <- matrix(
  c(200, -20,
    100, -60),
  nrow = 2, byrow = TRUE
)
rownames(profit_mat) <- states
colnames(profit_mat) <- states

# 策略 A 转移矩阵
PA <- matrix(
  c(0.8, 0.2,
    0.4, 0.6),
  nrow = 2, byrow = TRUE
)
rownames(PA) <- states
colnames(PA) <- states

# 策略 B 转移矩阵
PB <- matrix(
  c(0.7, 0.3,
    0.5, 0.5),
  nrow = 2, byrow = TRUE
)
rownames(PB) <- states
colnames(PB) <- states

# 已知上一时期为滞销 => 当前初始分布
S0_default <- c(0, 1)
names(S0_default) <- states

# 自定义矩阵输入 UI 组件
matrixInput <- function(inputId, label, mat) {
  tagList(
    p(class = "small-note", "行=当前状态，列=下一状态。每行概率之和必须等于 1。"),
    lapply(seq_len(nrow(mat)), function(i) {
      fluidRow(
        lapply(seq_len(ncol(mat)), function(j) {
          column(
            width = floor(12 / ncol(mat)),
            numericInput(
              inputId = paste0(inputId, "_", i, "_", j),
              label = if (i == 1) colnames(mat)[j] else NULL,
              value = mat[i, j],
              step = 0.01
            )
          )
        })
      )
    })
  )
}

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
      .warning-box {
        background: #fff3cd; border: 1px solid #f0d98c; border-radius: 8px;
        padding: 12px 16px; margin-bottom: 14px; color: #7a4b00;
      }
      .small-note { color: #667085; font-size: 13px; }
    "))
  ),
  
  titlePanel(div(class = "title-main", "序贯决策分析教学网页：马尔可夫销售策略")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《序贯决策分析教学网页：马尔可夫销售策略》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("初始状态"),
      selectInput("init_state", "已知上一时期状态", choices = states, selected = "滞销"),
      numericInput("n_steps", "预测时期数 n", value = 5, min = 1, max = 50, step = 1),
      
      tags$hr(),
      h4("策略 A 转移矩阵"),
      matrixInput("PA", "策略 A", PA),
      
      tags$hr(),
      h4("策略 B 转移矩阵"),
      matrixInput("PB", "策略 B", PB),
      
      tags$hr(),
      h4("利润矩阵 R（行：上一期，列：本期）"),
      matrixInput("R", "利润", profit_mat),
      
      tags$hr(),
      actionButton("calculate", "计算并比较", class = "btn-success")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("案例说明", br(),
                 div(class = "info-box",
                     h4("教学目标"),
                     p("本章对应教材第六章“序贯决策分析”。通过本网页，学生可以："),
                     tags$ul(
                       tags$li("理解马尔可夫链中状态转移矩阵的作用；"),
                       tags$li("计算 n 步转移后的状态概率；"),
                       tags$li("比较不同长期策略下的稳态期望利润；"),
                       tags$li("为序贯/长期经营决策提供定量依据。")
                     ),
                     h4("教材题目"),
                     p("某企业商品的销售状态只分为“畅销”和“滞销”两种。每个时期的利润与上一时期及本时期所处状态有关（见利润矩阵）。已知目前的前一时期为“滞销”。现有策略 A、B 两种状态转移矩阵。问：为使利润最大化，在不同经营期限下应采取哪种策略？"),
                     tags$hr(),
                     h4("马尔可夫链基本约定"),
                     tags$ul(
                       tags$li("无后效性（马尔可夫性）：下一时期状态只依赖当前状态，与更早历史无关；"),
                       tags$li("行随机矩阵：转移矩阵 P 的每一行表示“当前状态 → 下一状态”的概率分布，因此每行之和为 1；"),
                       tags$li("稳态存在条件：若马尔可夫链不可约且非周期，则存在唯一稳态分布 π，满足 π = πP；"),
                       tags$li("稳态结论仅适用于长期经营；若经营期限较短，应结合 n 步状态概率演化综合判断。")
                     )
                 )
        ),
        tabPanel("计算结果", br(),
                 uiOutput("validation_msg"),
                 fluidRow(
                   column(6, uiOutput("metric_A")),
                   column(6, uiOutput("metric_B"))
                 ),
                 br(),
                 h4("状态概率演化"),
                 DTOutput("prob_table"),
                 br(),
                 h4("推荐策略"),
                 uiOutput("recommend")
        ),
        tabPanel("图形分析", br(),
                 plotOutput("trend_plot", height = "400px")
        )
      )
    )
  )
)


# 读取自定义矩阵
readMatrix <- function(input, inputId, n, m) {
  mat <- matrix(0, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      mat[i, j] <- input[[paste0(inputId, "_", i, "_", j)]]
    }
  }
  mat
}

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  PA_mat <- reactive({
    readMatrix(input, "PA", 2, 2)
  })
  
  PB_mat <- reactive({
    readMatrix(input, "PB", 2, 2)
  })
  
  R_mat <- reactive({
    readMatrix(input, "R", 2, 2)
  })
  
  S0 <- reactive({
    s <- c(1, 0)
    if (input$init_state == "滞销") s <- c(0, 1)
    names(s) <- states
    s
  })
  
  validate_matrix <- function(P, name) {
    msg <- NULL
    if (any(P < 0)) msg <- paste0(name, " 存在负概率，已自动截断为 0。")
    P <- pmax(P, 0)
    row_sums <- rowSums(P)
    if (any(row_sums == 0)) {
      return(list(valid = FALSE, msg = paste0(name, " 某行概率之和为 0，无法作为转移矩阵。"), P = P))
    }
    if (any(abs(row_sums - 1) > 1e-06)) {
      P <- P / row_sums
      msg <- paste0(name, " 每行概率之和未严格为 1，已自动归一化。")
    }
    list(valid = TRUE, msg = msg, P = P)
  }
  
  results <- eventReactive(input$calculate, {
    S0v <- S0()
    n <- input$n_steps
    
    val_A <- validate_matrix(PA_mat(), "策略 A 转移矩阵")
    val_B <- validate_matrix(PB_mat(), "策略 B 转移矩阵")
    
    if (!val_A$valid || !val_B$valid) {
      showNotification(paste(val_A$msg, val_B$msg, sep = " "), type = "error")
      return(NULL)
    }
    if (!is.null(val_A$msg)) showNotification(val_A$msg, type = "warning")
    if (!is.null(val_B$msg)) showNotification(val_B$msg, type = "warning")
    
    PA <- val_A$P
    PB <- val_B$P
    
    # 各时期状态概率
    prob_A <- t(sapply(0:n, function(k) step_prob(PA, S0v, k)))
    prob_B <- t(sapply(0:n, function(k) step_prob(PB, S0v, k)))
    colnames(prob_A) <- states
    colnames(prob_B) <- states
    prob_A <- as.data.frame(prob_A)
    prob_B <- as.data.frame(prob_B)
    prob_A$策略 <- "A"
    prob_B$策略 <- "B"
    prob_A$时期 <- 0:n
    prob_B$时期 <- 0:n
    
    # 稳态
    ss_A <- steady_state(PA)
    ss_B <- steady_state(PB)
    
    # 稳态年期望利润（简化）
    exp_A <- expected_profit_2yr(PA, R_mat(), ss_A)
    exp_B <- expected_profit_2yr(PB, R_mat(), ss_B)
    
    list(prob_A = prob_A, prob_B = prob_B, ss_A = ss_A, ss_B = ss_B,
         exp_A = exp_A, exp_B = exp_B)
  })
  
  output$metric_A <- renderUI({
    req(results())
    div(class = "metric-card",
        div(class = "metric-title", "策略 A 稳态期望年利润"),
        div(class = "metric-value", sprintf("%.2f 万元", results()$exp_A)),
        div(style = "margin-top:8px; font-size:13px; color:#5b7083;",
            sprintf("稳态概率：畅销 %.3f，滞销 %.3f", results()$ss_A[1], results()$ss_A[2]))
    )
  })
  
  output$metric_B <- renderUI({
    req(results())
    div(class = "metric-card",
        div(class = "metric-title", "策略 B 稳态期望年利润"),
        div(class = "metric-value", sprintf("%.2f 万元", results()$exp_B)),
        div(style = "margin-top:8px; font-size:13px; color:#5b7083;",
            sprintf("稳态概率：畅销 %.3f，滞销 %.3f", results()$ss_B[1], results()$ss_B[2]))
    )
  })
  
  output$prob_table <- renderDT({
    req(results())
    df <- rbind(results()$prob_A, results()$prob_B)
    df <- df[, c("策略", "时期", states)]
    datatable(df, options = list(pageLength = 12, searching = FALSE),
              rownames = FALSE) %>%
      formatRound(columns = states, digits = 4)
  })
  
  output$validation_msg <- renderUI({
    req(results())
    div(class = "info-box",
        p(strong("转移矩阵已校验。")),
        p("当前采用行随机矩阵约定：π_{t+1} = π_t · P。每行概率之和已校验为 1。"),
        p("稳态结论成立的前提是马尔可夫链不可约且非周期；若矩阵不满足该条件，稳态可能不唯一或不存在。")
    )
  })
  
  output$recommend <- renderUI({
    req(results())
    better <- if (results()$exp_A >= results()$exp_B) "A" else "B"
    div(class = "info-box",
        h4("推荐结果"),
        p(sprintf("若两策略对应的马尔可夫链均收敛到唯一稳态，则从稳态期望年利润看，策略 %s 更优（%.2f vs %.2f 万元）。",
                  better, max(results()$exp_A, results()$exp_B), min(results()$exp_A, results()$exp_B))),
        p("若经营期限较短，可结合“图形分析”标签页查看各时期畅销概率变化，综合判断。")
    )
  })
  
  output$trend_plot <- renderPlot({
    req(results())
    df <- rbind(results()$prob_A, results()$prob_B)
    df_long <- tidyr::pivot_longer(df, cols = all_of(states), names_to = "状态", values_to = "概率")
    ggplot(df_long, aes(x = 时期, y = 概率, color = 状态)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~策略) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "不同策略下状态概率演化", x = "时期", y = "概率") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
