# 依赖包：shiny, DT, ggplot2, rhandsontable, tidyr
# install.packages(c("shiny", "DT", "ggplot2", "rhandsontable", "tidyr"))

library(shiny)
library(DT)
library(ggplot2)
library(rhandsontable)
library(tidyr)

# =========================
# 马尔可夫决策辅助函数
# =========================

mat_power <- function(P, n) {
  if (n == 0) return(diag(nrow(P)))
  result <- P
  if (n == 1) return(result)
  for (i in 2:n) {
    result <- result %*% P
  }
  result
}

step_prob <- function(P, S0, n) {
  if (n == 0) return(as.vector(S0))
  as.vector(S0 %*% mat_power(P, n))
}

# 解 pi = pi P，sum(pi)=1
steady_state <- function(P) {
  n <- nrow(P)
  if (n != ncol(P)) return(rep(NA_real_, n))
  A <- t(P) - diag(n)
  A <- rbind(A, rep(1, n))
  b <- c(rep(0, n), 1)
  S <- tryCatch(qr.solve(A, b), error = function(e) rep(NA_real_, n))
  if (any(is.na(S))) return(rep(NA_real_, n))
  S <- pmax(S, 0)
  S <- S / sum(S)
  as.vector(S)
}

# 稳态期望年利润：sum_{i,j} pi_i * P_{ij} * R_{ij}
expected_profit <- function(P, R, pi) {
  n <- nrow(P)
  exp <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      exp <- exp + pi[i] * P[i, j] * R[i, j]
    }
  }
  exp
}

# 校验转移矩阵：非负、行和为 1
validate_transition <- function(P, name = "转移矩阵") {
  P <- suppressWarnings(as.numeric(P))
  if (any(is.na(P))) {
    return(list(valid = FALSE, msg = sprintf("%s 存在缺失或非数值。", name), P = P))
  }
  n <- sqrt(length(P))
  P <- matrix(pmax(P, 0), nrow = n)
  rs <- rowSums(P)
  if (any(rs == 0)) {
    return(list(valid = FALSE, msg = sprintf("%s 存在行和为 0 的行。", name), P = P))
  }
  if (any(abs(rs - 1) > 1e-06)) {
    P <- P / rs
  }
  list(valid = TRUE, msg = NULL, P = P)
}

# 吸收链分析
absorption_analysis <- function(P) {
  n <- nrow(P)
  absorbing <- which(abs(diag(P) - 1) < 1e-6)
  nonabs <- setdiff(seq_len(n), absorbing)
  if (length(absorbing) == 0 || length(nonabs) == 0) {
    return(NULL)
  }
  Q <- P[nonabs, nonabs, drop = FALSE]
  R <- P[nonabs, absorbing, drop = FALSE]
  N <- solve(diag(length(nonabs)) - Q)
  B <- N %*% R
  t <- as.vector(N %*% rep(1, length(nonabs)))
  list(
    absorbing = absorbing,
    nonabs = nonabs,
    Q = Q,
    R = R,
    N = N,
    B = B,
    t = t
  )
}

# =========================
# 默认数据（教材第六章习题 7：2 状态）
# =========================
default_states <- c("畅销", "滞销")
profit_mat <- matrix(
  c(200, -20,
    100, -60),
  nrow = 2, byrow = TRUE
)
PA <- matrix(
  c(0.8, 0.2,
    0.4, 0.6),
  nrow = 2, byrow = TRUE
)
PB <- matrix(
  c(0.7, 0.3,
    0.5, 0.5),
  nrow = 2, byrow = TRUE
)
S0_default <- c(0, 1)

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
      .warning-box {
        background: #fff3cd; border: 1px solid #f0d98c; border-radius: 8px;
        padding: 12px 16px; margin-bottom: 14px; color: #7a4b00;
      }
      .small-note { color: #667085; font-size: 13px; }
    "))
  ),

  titlePanel(div(class = "title-main", "马尔可夫分析教学网页：状态转移、稳态分布与吸收链")),

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
      h4("状态设置"),
      numericInput("n_states", "状态数量", value = 2, min = 2, max = 8, step = 1),
      actionButton("generate", "生成/重置矩阵", class = "btn-primary"),
      actionButton("reset_default", "恢复默认案例", class = "btn-warning"),
      tags$hr(),

      h4("预测期数"),
      numericInput("n_steps", "预测时期数 n", value = 5, min = 1, max = 50, step = 1),
      tags$hr(),

      h4("初始状态分布 π₀"),
      helpText("行=概率，列=状态，非负且和为 1。"),
      rHandsontableOutput("S0_hot"),
      tags$hr(),

      h4("策略 A 转移矩阵"),
      helpText("行=当前状态，列=下一状态，每行和为 1。"),
      rHandsontableOutput("PA_hot"),
      tags$hr(),

      h4("策略 B 转移矩阵"),
      helpText("行=当前状态，列=下一状态，每行和为 1。"),
      rHandsontableOutput("PB_hot"),
      tags$hr(),

      h4("利润矩阵 R"),
      helpText("行=上一期状态，列=本期状态，单位：万元。"),
      rHandsontableOutput("R_hot"),
      tags$hr(),

      actionButton("calculate", "计算并比较", class = "btn-success")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "案例说明",
          br(),
          div(
            class = "info-box",
            h4("教学目标"),
            p("本章对应教材第六章“序贯决策分析”。通过本网页，学生可以："),
            tags$ul(
              tags$li("理解马尔可夫链的状态转移矩阵、行随机约定与无后效性；"),
              tags$li("计算 n 步转移后的状态概率 π_t = π_0 P^t；"),
              tags$li("求稳态分布 π（满足 π = πP）并计算稳态期望利润；"),
              tags$li("识别吸收状态，计算基本矩阵 N、吸收概率 B 与期望吸收时间 t。")
            ),
            tags$hr(),
            h4("基本约定"),
            tags$ul(
              tags$li("行随机矩阵：P 的每一行是概率分布，行之和为 1；"),
              tags$li("状态更新：π_{t+1} = π_t · P；"),
              tags$li("稳态存在条件：马尔可夫链不可约且非周期；吸收链不存在唯一稳态分布；"),
              tags$li("稳态结论仅适用于长期经营，短期决策应结合 n 步状态概率演化。")
            )
          )
        ),

        tabPanel(
          "计算结果",
          br(),
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

        tabPanel(
          "吸收链分析",
          br(),
          uiOutput("absorb_summary"),
          br(),
          uiOutput("absorb_tables")
        ),

        tabPanel(
          "图形分析",
          br(),
          plotOutput("trend_plot", height = "420px")
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
    state_names = default_states,
    S0_df = as.data.frame(matrix(S0_default, nrow = 1)),
    PA_df = as.data.frame(PA),
    PB_df = as.data.frame(PB),
    R_df = as.data.frame(profit_mat),
    res = NULL
  )

  observe({
    colnames(rv$S0_df) <- rv$state_names
    colnames(rv$PA_df) <- rv$state_names
    rownames(rv$PA_df) <- rv$state_names
    colnames(rv$PB_df) <- rv$state_names
    rownames(rv$PB_df) <- rv$state_names
    colnames(rv$R_df) <- rv$state_names
    rownames(rv$R_df) <- rv$state_names
  })

  observeEvent(input$generate, {
    n <- input$n_states
    rv$state_names <- paste0("状态", seq_len(n))
    rv$S0_df <- as.data.frame(matrix(0, nrow = 1, ncol = n))
    rv$PA_df <- as.data.frame(diag(n))
    rv$PB_df <- as.data.frame(diag(n))
    rv$R_df <- as.data.frame(matrix(0, nrow = n, ncol = n))
    colnames(rv$S0_df) <- rv$state_names
    colnames(rv$PA_df) <- rv$state_names
    rownames(rv$PA_df) <- rv$state_names
    colnames(rv$PB_df) <- rv$state_names
    rownames(rv$PB_df) <- rv$state_names
    colnames(rv$R_df) <- rv$state_names
    rownames(rv$R_df) <- rv$state_names
    rv$res <- NULL
  })

  observeEvent(input$reset_default, {
    rv$state_names <- default_states
    rv$S0_df <- as.data.frame(matrix(S0_default, nrow = 1))
    rv$PA_df <- as.data.frame(PA)
    rv$PB_df <- as.data.frame(PB)
    rv$R_df <- as.data.frame(profit_mat)
    colnames(rv$S0_df) <- rv$state_names
    colnames(rv$PA_df) <- rv$state_names
    rownames(rv$PA_df) <- rv$state_names
    colnames(rv$PB_df) <- rv$state_names
    rownames(rv$PB_df) <- rv$state_names
    colnames(rv$R_df) <- rv$state_names
    rownames(rv$R_df) <- rv$state_names
    updateNumericInput(session, "n_states", value = 2)
    rv$res <- NULL
  })

  make_hot <- function(df, h = 240) {
    rhandsontable(df, rowHeaders = rownames(df), stretchH = "all", height = h, contextMenu = TRUE) %>%
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0000") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }
      ")
  }

  output$S0_hot <- renderRHandsontable({ make_hot(rv$S0_df, 100) })
  output$PA_hot <- renderRHandsontable({ make_hot(rv$PA_df, 240) })
  output$PB_hot <- renderRHandsontable({ make_hot(rv$PB_df, 240) })
  output$R_hot <- renderRHandsontable({ make_hot(rv$R_df, 240) })

  observeEvent(input$calculate, {
    S0_tbl <- hot_to_r(input$S0_hot)
    PA_tbl <- hot_to_r(input$PA_hot)
    PB_tbl <- hot_to_r(input$PB_hot)
    R_tbl <- hot_to_r(input$R_hot)

    if (is.null(S0_tbl) || is.null(PA_tbl) || is.null(PB_tbl) || is.null(R_tbl)) {
      showNotification("请先生成并填写所有矩阵。", type = "error")
      return()
    }

    S0v <- suppressWarnings(as.numeric(S0_tbl[1, ]))
    if (any(is.na(S0v))) {
      showNotification("初始状态分布存在缺失或非数值。", type = "error")
      return()
    }
    if (any(S0v < 0)) S0v <- pmax(S0v, 0)
    s <- sum(S0v)
    if (s == 0) {
      showNotification("初始状态分布之和不能为 0。", type = "error")
      return()
    }
    S0v <- S0v / s

    val_A <- validate_transition(as.matrix(PA_tbl), "策略 A 转移矩阵")
    val_B <- validate_transition(as.matrix(PB_tbl), "策略 B 转移矩阵")
    if (!val_A$valid || !val_B$valid) {
      showNotification(paste(val_A$msg, val_B$msg, sep = " "), type = "error")
      return()
    }
    if (!is.null(val_A$msg)) showNotification(val_A$msg, type = "warning")
    if (!is.null(val_B$msg)) showNotification(val_B$msg, type = "warning")

    PA <- val_A$P
    PB <- val_B$P
    R <- suppressWarnings(as.matrix(R_tbl))
    if (any(is.na(R))) {
      showNotification("利润矩阵存在缺失或非数值。", type = "error")
      return()
    }

    n <- input$n_steps
    state_names <- colnames(PA_tbl)

    # n 步概率演化
    prob_A <- t(sapply(0:n, function(k) step_prob(PA, S0v, k)))
    prob_B <- t(sapply(0:n, function(k) step_prob(PB, S0v, k)))
    colnames(prob_A) <- state_names
    colnames(prob_B) <- state_names
    prob_A <- as.data.frame(prob_A)
    prob_B <- as.data.frame(prob_B)
    prob_A$策略 <- "A"
    prob_B$策略 <- "B"
    prob_A$时期 <- 0:n
    prob_B$时期 <- 0:n

    # 稳态
    ss_A <- steady_state(PA)
    ss_B <- steady_state(PB)

    # 稳态期望利润
    exp_A <- expected_profit(PA, R, ss_A)
    exp_B <- expected_profit(PB, R, ss_B)

    # 吸收链分析
    abs_A <- absorption_analysis(PA)
    abs_B <- absorption_analysis(PB)

    rv$res <- list(
      PA = PA, PB = PB, R = R, S0 = S0v,
      prob_A = prob_A, prob_B = prob_B,
      ss_A = ss_A, ss_B = ss_B,
      exp_A = exp_A, exp_B = exp_B,
      abs_A = abs_A, abs_B = abs_B,
      state_names = state_names
    )
  })

  output$validation_msg <- renderUI({
    req(rv$res)
    div(class = "info-box",
        p(strong("转移矩阵已校验。")),
        p("采用行随机矩阵约定：π_{t+1} = π_t · P。每行概率之和已校验为 1。"),
        p("稳态结论成立的前提是马尔可夫链不可约且非周期；若链为吸收链或周期链，稳态可能不唯一或不存在。")
    )
  })

  output$metric_A <- renderUI({
    req(rv$res)
    div(class = "metric-card",
        div(class = "metric-title", "策略 A 稳态期望年利润"),
        div(class = "metric-value", sprintf("%.2f 万元", rv$res$exp_A)),
        div(style = "margin-top:8px; font-size:13px; color:#5b7083;",
            sprintf("稳态分布：%s", paste(sprintf("%s %.3f", rv$res$state_names, rv$res$ss_A), collapse = ", ")))
    )
  })

  output$metric_B <- renderUI({
    req(rv$res)
    div(class = "metric-card",
        div(class = "metric-title", "策略 B 稳态期望年利润"),
        div(class = "metric-value", sprintf("%.2f 万元", rv$res$exp_B)),
        div(style = "margin-top:8px; font-size:13px; color:#5b7083;",
            sprintf("稳态分布：%s", paste(sprintf("%s %.3f", rv$res$state_names, rv$res$ss_B), collapse = ", ")))
    )
  })

  output$prob_table <- renderDT({
    req(rv$res)
    df <- rbind(rv$res$prob_A, rv$res$prob_B)
    df <- df[, c("策略", "时期", rv$res$state_names)]
    datatable(df, options = list(pageLength = 12, searching = FALSE), rownames = FALSE) %>%
      formatRound(columns = rv$res$state_names, digits = 4)
  })

  output$recommend <- renderUI({
    req(rv$res)
    better <- if (rv$res$exp_A >= rv$res$exp_B) "A" else "B"
    div(class = "info-box",
        h4("推荐结果"),
        p(sprintf("若两策略对应的马尔可夫链均收敛到唯一稳态，则从稳态期望年利润看，策略 %s 更优（%.2f vs %.2f 万元）。",
                  better, max(rv$res$exp_A, rv$res$exp_B), min(rv$res$exp_A, rv$res$exp_B))),
        p("若存在吸收状态，稳态分布不唯一，应参考“吸收链分析”标签页。若经营期限较短，应结合“图形分析”中 n 步状态概率演化综合判断。")
    )
  })

  output$absorb_summary <- renderUI({
    req(rv$res)
    has_A <- !is.null(rv$res$abs_A)
    has_B <- !is.null(rv$res$abs_B)
    if (!has_A && !has_B) {
      return(div(class = "info-box", p("当前两个策略均未检测到吸收状态，因此不属于吸收马尔可夫链。")))
    }
    txt <- paste(
      if (has_A) "策略 A 检测到吸收状态。" else "策略 A 未检测到吸收状态。",
      if (has_B) "策略 B 检测到吸收状态。" else "策略 B 未检测到吸收状态。"
    )
    div(class = "info-box", p(strong(txt)),
        p("吸收链基本公式：N = (I - Q)^{-1}，B = N·R，t = N·1，其中 Q 为非吸收状态之间的转移子矩阵，R 为非吸收状态到吸收状态的转移子矩阵。"))
  })

  output$absorb_tables <- renderUI({
    req(rv$res)
    out <- tagList()
    for (pol in c("A", "B")) {
      abs_info <- rv$res[[paste0("abs_", pol)]]
      if (!is.null(abs_info)) {
        sn <- rv$res$state_names
        nonabs_names <- sn[abs_info$nonabs]
        abs_names <- sn[abs_info$absorb]

        N_df <- as.data.frame(round(abs_info$N, 4))
        colnames(N_df) <- nonabs_names
        rownames(N_df) <- nonabs_names
        N_df$状态 <- nonabs_names
        N_df <- N_df[, c("状态", nonabs_names)]

        B_df <- as.data.frame(round(abs_info$B, 4))
        colnames(B_df) <- abs_names
        rownames(B_df) <- nonabs_names
        B_df$状态 <- nonabs_names
        B_df <- B_df[, c("状态", abs_names)]

        t_df <- data.frame(
          状态 = nonabs_names,
          期望吸收时间 = round(abs_info$t, 4),
          stringsAsFactors = FALSE
        )

        out <- tagList(
          out,
          h4(sprintf("策略 %s 吸收链分析", pol)),
          div(class = "info-box",
              h5("基本矩阵 N = (I - Q)^{-1}"),
              DTOutput(paste0("N_table_", pol))
          ),
          div(class = "info-box",
              h5("吸收概率 B = N·R"),
              DTOutput(paste0("B_table_", pol))
          ),
          div(class = "info-box",
              h5("期望吸收时间 t = N·1"),
              DTOutput(paste0("t_table_", pol))
          )
        )
      }
    }
    out
  })

  # 为吸收链表格动态注册输出（利用局部变量在 observe 内）
  observe({
    req(rv$res)
    for (pol in c("A", "B")) {
      abs_info <- rv$res[[paste0("abs_", pol)]]
      if (!is.null(abs_info)) {
        local({
          p <- pol
          ai <- abs_info
          sn <- rv$res$state_names
          nonabs_names <- sn[ai$nonabs]
          abs_names <- sn[ai$absorb]

          N_df <- as.data.frame(round(ai$N, 4))
          colnames(N_df) <- nonabs_names
          N_df$状态 <- nonabs_names
          N_df <- N_df[, c("状态", nonabs_names)]

          B_df <- as.data.frame(round(ai$B, 4))
          colnames(B_df) <- abs_names
          B_df$状态 <- nonabs_names
          B_df <- B_df[, c("状态", abs_names)]

          t_df <- data.frame(状态 = nonabs_names, 期望吸收时间 = round(ai$t, 4), stringsAsFactors = FALSE)

          output[[paste0("N_table_", p)]] <- renderDT({
            datatable(N_df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
          })
          output[[paste0("B_table_", p)]] <- renderDT({
            datatable(B_df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
          })
          output[[paste0("t_table_", p)]] <- renderDT({
            datatable(t_df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
          })
        })
      }
    }
  })

  output$trend_plot <- renderPlot({
    req(rv$res)
    df <- rbind(rv$res$prob_A, rv$res$prob_B)
    df_long <- pivot_longer(df, cols = all_of(rv$res$state_names), names_to = "状态", values_to = "概率")
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
