# 依赖包：shiny, DT, rhandsontable, ggplot2, tidyr
# install.packages(c("shiny", "DT", "rhandsontable", "ggplot2", "tidyr"))

library(shiny)
library(DT)
library(rhandsontable)
library(ggplot2)
library(tidyr)

# =========================
# 核心计算函数
# =========================

validate_transition_matrix <- function(P, name = "转移矩阵") {
  P <- as.matrix(P)
  if (nrow(P) != ncol(P)) {
    return(list(ok = FALSE, msg = sprintf("%s 不是方阵（%d 行 × %d 列），请检查输入。", name, nrow(P), ncol(P))))
  }
  if (any(is.na(P))) {
    return(list(ok = FALSE, msg = sprintf("%s 存在缺失值，请检查输入。", name)))
  }
  if (any(P < 0)) {
    return(list(ok = FALSE, msg = sprintf("%s 存在负值，转移概率必须非负，请检查输入。", name)))
  }
  rs <- rowSums(P)
  if (any(abs(rs - 1) > 1e-6)) {
    bad <- which(abs(rs - 1) > 1e-6)
    return(list(ok = FALSE, msg = sprintf("%s 第 %s 行的概率之和不等于 1（实际为 %s），请检查输入。",
                                          name, paste(bad, collapse = ", "), paste(round(rs[bad], 4), collapse = ", "))))
  }
  list(ok = TRUE, msg = "")
}

validate_initial_distribution <- function(pi0, n) {
  pi0 <- as.vector(pi0)
  if (length(pi0) != n) {
    return(list(ok = FALSE, msg = sprintf("初始分布长度（%d）与状态数（%d）不一致，请检查输入。", length(pi0), n)))
  }
  if (any(is.na(pi0))) {
    return(list(ok = FALSE, msg = "初始分布存在缺失值，请检查输入。"))
  }
  if (any(pi0 < 0)) {
    return(list(ok = FALSE, msg = "初始分布存在负值，概率必须非负。"))
  }
  if (abs(sum(pi0) - 1) > 1e-6) {
    return(list(ok = FALSE, msg = sprintf("初始分布之和为 %.6f，必须等于 1，请检查输入。", sum(pi0))))
  }
  list(ok = TRUE, msg = "")
}

mat_power <- function(M, k) {
  if (k == 0) return(diag(nrow(M)))
  if (k == 1) return(M)
  res <- diag(nrow(M))
  base <- M
  while (k > 0) {
    if (k %% 2 == 1) res <- res %*% base
    base <- base %*% base
    k <- k %/% 2
  }
  res
}

# n 步演化：π_t = π_0 P^t
calc_markov_evolution <- function(P, pi0, n_steps) {
  v <- validate_transition_matrix(P, "转移矩阵")
  if (!v$ok) stop(v$msg)
  v2 <- validate_initial_distribution(pi0, nrow(P))
  if (!v2$ok) stop(v2$msg)

  P <- as.matrix(P)
  pi0 <- as.vector(pi0)
  rows <- lapply(0:n_steps, function(k) as.vector(pi0 %*% mat_power(P, k)))
  mat <- do.call(rbind, rows)
  colnames(mat) <- colnames(P)
  mat
}

# 稳态分布：π = πP，∑π=1
calc_markov_stationary <- function(P) {
  v <- validate_transition_matrix(P, "转移矩阵")
  if (!v$ok) stop(v$msg)

  n <- nrow(P)
  A <- t(P) - diag(n)
  A <- rbind(A, rep(1, n))
  b <- c(rep(0, n), 1)
  S <- tryCatch(qr.solve(A, b), error = function(e) rep(NA_real_, n))
  if (any(is.na(S))) {
    stop("稳态分布求解失败，可能是转移矩阵奇异或链不满足不可约/非周期条件。请检查输入。")
  }
  # 保证非负并归一化
  S <- pmax(S, 0)
  S <- S / sum(S)
  names(S) <- colnames(P)
  S
}

# 吸收链分析
calc_absorbing_chain <- function(P) {
  v <- validate_transition_matrix(P, "转移矩阵")
  if (!v$ok) stop(v$msg)

  n <- nrow(P)
  state_names <- colnames(P)
  if (is.null(state_names)) state_names <- paste0("状态", seq_len(n))

  # 严格吸收态：p_ii = 1 且该行其余元素为 0
  absorbing <- which(abs(diag(P) - 1) < 1e-6 & rowSums(P) - diag(P) < 1e-6)
  nonabs <- setdiff(seq_len(n), absorbing)

  if (length(absorbing) == 0) {
    return(list(has_absorbing = FALSE, msg = "该转移矩阵没有吸收态，无法进行标准吸收链分析。"))
  }
  if (length(nonabs) == 0) {
    return(list(has_absorbing = TRUE, has_transient = FALSE, msg = "所有状态均为吸收态。"))
  }

  ord <- c(nonabs, absorbing)
  P_reord <- P[ord, ord, drop = FALSE]
  m <- length(nonabs)
  Q <- P_reord[1:m, 1:m, drop = FALSE]
  R <- P_reord[1:m, (m + 1):n, drop = FALSE]

  N <- tryCatch(solve(diag(m) - Q), error = function(e) NULL)
  if (is.null(N)) {
    stop("基本矩阵 (I - Q) 不可逆，无法完成吸收链分析。请检查转移矩阵。")
  }
  B <- N %*% R
  t_vec <- as.vector(N %*% rep(1, m))

  rownames(Q) <- colnames(Q) <- state_names[nonabs]
  rownames(R) <- state_names[nonabs]
  colnames(R) <- state_names[absorbing]
  rownames(N) <- colnames(N) <- state_names[nonabs]
  rownames(B) <- state_names[nonabs]
  colnames(B) <- state_names[absorbing]
  names(t_vec) <- state_names[nonabs]

  list(
    has_absorbing = TRUE,
    has_transient = TRUE,
    absorbing_idx = absorbing,
    transient_idx = nonabs,
    absorbing_names = state_names[absorbing],
    transient_names = state_names[nonabs],
    P_reord = P_reord,
    Q = Q,
    R = R,
    N = N,
    B = B,
    t = t_vec
  )
}

# =========================
# 教师自测
# =========================

run_markov_self_tests <- function() {
  tests <- list()

  P <- matrix(c(0.9, 0.1, 0.5, 0.5), nrow = 2, byrow = TRUE,
              dimnames = list(c("S1", "S2"), c("S1", "S2")))
  pi0 <- c(0.5, 0.5)

  evol <- calc_markov_evolution(P, pi0, 2)
  expected_pi2 <- as.vector(pi0 %*% mat_power(P, 2))
  passed_evol <- all(abs(evol[3, ] - expected_pi2) < 1e-6)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "行随机 π_{t+1}=π_tP 演化",
    实际输出 = paste0("t=2: ", paste(round(evol[3, ], 4), collapse = ", ")),
    标准答案 = paste0("t=2: ", paste(round(expected_pi2, 4), collapse = ", ")),
    是否通过 = passed_evol,
    失败提示 = if (passed_evol) "" else "请检查是否使用行随机约定 π_{t+1}=π_tP，而非列随机。"
  )

  stat <- calc_markov_stationary(P)
  expected_stat <- c(5/6, 1/6)
  passed_stat <- all(abs(stat - expected_stat) < 1e-6)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "稳态分布 π=πP",
    实际输出 = paste(round(stat, 4), collapse = ", "),
    标准答案 = "0.8333, 0.1667",
    是否通过 = passed_stat,
    失败提示 = if (passed_stat) "" else "稳态分布应满足 π=πP 且元素和为 1。"
  )

  P_abs <- matrix(c(1, 0, 0,
                    0.2, 0.6, 0.2,
                    0, 0, 1),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(c("A", "T", "C"), c("A", "T", "C")))
  abs_res <- calc_absorbing_chain(P_abs)
  passed_abs <- abs_res$has_absorbing && abs_res$has_transient &&
    length(abs_res$absorbing_names) == 2 &&
    all(abs_res$absorbing_names == c("A", "C")) &&
    all(abs(abs_res$t - 2.5) < 1e-6) &&
    all(abs(abs_res$B - matrix(c(0.5, 0.5), nrow = 1)) < 1e-6)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "吸收链 Q/R/N/B/t",
    实际输出 = paste0("吸收态=", paste(abs_res$absorbing_names, collapse = ","),
                    "; t=", paste(round(abs_res$t, 4), collapse = ", "),
                    "; B=", paste(round(abs_res$B, 4), collapse = ", ")),
    标准答案 = "吸收态=A,C; t=2.5; B=0.5,0.5",
    是否通过 = passed_abs,
    失败提示 = if (passed_abs) "" else "请检查吸收态判定、状态重排后 Q/R 提取及 N=(I-Q)^{-1}、B=NR、t=N·1 的计算。"
  )

  err_cases <- list(
    list(name = "转移矩阵行和 ≠ 1", f = function() calc_markov_evolution(matrix(c(0.9, 0.2, 0.5, 0.5), nrow = 2, byrow = TRUE), c(0.5, 0.5), 1),
         hint = "每行概率之和必须等于 1。"),
    list(name = "转移矩阵含负数", f = function() calc_markov_evolution(matrix(c(-0.1, 1.1, 0.5, 0.5), nrow = 2, byrow = TRUE), c(0.5, 0.5), 1),
         hint = "转移概率必须非负。"),
    list(name = "初始分布和 ≠ 1", f = function() calc_markov_evolution(P, c(0.6, 0.5), 1),
         hint = "初始分布之和必须等于 1。"),
    list(name = "非方阵", f = function() calc_markov_stationary(matrix(c(0.5, 0.5, 0.3, 0.3, 0.4, 0.4), nrow = 2, byrow = TRUE)),
         hint = "转移矩阵必须是方阵。")
  )
  for (case in err_cases) {
    passed <- tryCatch({ case$f(); FALSE }, error = function(e) TRUE)
    tests[[length(tests) + 1]] <- list(
      测试名称 = paste0("错误输入：", case$name),
      实际输出 = if (passed) "正确报错" else "未报错",
      标准答案 = "正确报错",
      是否通过 = passed,
      失败提示 = if (passed) "" else case$hint
    )
  }

  df <- do.call(rbind, lapply(tests, as.data.frame, stringsAsFactors = FALSE))
  rownames(df) <- NULL
  df
}

# =========================
# 默认数据
# =========================
default_n_states <- 3
default_state_names <- c("状态1", "状态2", "状态3")
default_P <- matrix(
  c(0.7, 0.2, 0.1,
    0.3, 0.5, 0.2,
    0.1, 0.3, 0.6),
  nrow = 3, byrow = TRUE,
  dimnames = list(default_state_names, default_state_names)
)
default_pi0 <- c(1, 0, 0)

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
      .metric-value { font-size: 22px; font-weight: 700; color: #1f3b5b; }
      .formula-box {
        background: #f8fafc; border: 1px solid #d9e2ec;
        border-radius: 10px; padding: 14px 16px; line-height: 1.8;
      }
      .warning-box {
        background: #fff3cd; border: 1px solid #f0d98c; border-radius: 8px;
        padding: 12px 16px; margin-bottom: 14px; color: #7a4b00;
      }
      .small-note { color: #667085; font-size: 13px; }
      .pass { color: #1b7f3b; font-weight: bold; }
      .fail { color: #b42318; font-weight: bold; }
    "))
  ),

  titlePanel(div(class = "title-main", "马尔可夫链教学网页：状态演化、稳态与吸收链")),

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
      numericInput("n_states", "状态数量", value = default_n_states, min = 2, max = 15),
      actionButton("generate", "生成/重置矩阵", class = "btn-primary"),
      actionButton("reset_default", "恢复默认值", class = "btn-warning"),
      tags$hr(),

      h4("演化设置"),
      numericInput("n_steps", "演化步数", value = 10, min = 1, max = 100),
      tags$hr(),

      h4("操作"),
      actionButton("calculate", "计算演化 / 稳态 / 吸收链", class = "btn-success"),
      tags$hr(),

      checkboxInput("teacher_mode", "显示教师自测区域", value = FALSE),

      tags$hr(),
      helpText("输入说明："),
      tags$ul(
        tags$li("转移矩阵 P：行=当前状态，列=下一状态，每行之和必须等于 1。"),
        tags$li("本页面采用行随机约定：π_{t+1} = π_t P。"),
        tags$li("初始分布：各状态在 t=0 的概率，非负且和等于 1。"),
        tags$li("吸收态：p_{ii}=1 且该行其余元素为 0。")
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "案例说明",
          br(),
          div(
            class = "info-box",
            h4("方法背景"),
            p("马尔可夫链是一种无后效性的随机过程，未来状态只依赖于当前状态。本网页帮助理解状态概率演化、稳态分布以及吸收链分析。"),
            tags$details(
              tags$summary("查看计算说明"),
              br(),
              tags$ul(
                tags$li("行随机约定：π_{t+1} = π_t P，即 P 的每一行之和为 1。"),
                tags$li("n 步演化：π_t = π_0 P^t。"),
                tags$li("稳态分布：满足 π = πP 且 ∑π_i = 1 的概率向量。不可约、非周期链存在唯一稳态分布。"),
                tags$li("吸收链：将状态重排为（暂态，吸收态），P = [Q R; 0 I]，基本矩阵 N = (I - Q)^{-1}，吸收概率 B = NR，期望吸收时间 t = N·1。")
              )
            ),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("掌握行随机转移矩阵的构造与校验；"),
              tags$li("能够计算多步状态概率演化；"),
              tags$li("理解稳态分布的求解条件与计算方法；"),
              tags$li("掌握吸收链中 Q、R、N、B、t 的经济/物理含义。")
            )
          )
        ),

        tabPanel(
          "转移矩阵",
          br(),
          div(
            class = "info-box",
            h4("转移概率矩阵 P"),
            helpText("行=当前状态，列=下一状态，每行之和必须等于 1。"),
            rHandsontableOutput("P_hot")
          ),
          div(
            class = "info-box",
            h4("初始分布 π_0"),
            helpText("t=0 时各状态的概率，非负且和等于 1。"),
            rHandsontableOutput("pi0_hot")
          )
        ),

        tabPanel(
          "n 步演化",
          br(),
          div(
            class = "info-box",
            h4("状态概率演化表"),
            DTOutput("evolution_table")
          ),
          div(
            class = "info-box",
            h4("演化趋势图"),
            plotOutput("evolution_plot", height = "400px")
          )
        ),

        tabPanel(
          "稳态分布",
          br(),
          div(
            class = "info-box",
            h4("稳态分布 π"),
            DTOutput("stationary_table")
          ),
          br(),
          uiOutput("stationary_note")
        ),

        tabPanel(
          "吸收链",
          br(),
          uiOutput("absorbing_ui")
        ),

        tabPanel(
          "教师自测",
          br(),
          conditionalPanel(
            condition = "input.teacher_mode == true",
            div(class = "info-box",
                h4("教师自测"),
                actionButton("run_self_test", "运行自测", class = "btn-info"),
                br(), br(),
                DTOutput("self_test_table"))
          ),
          conditionalPanel(
            condition = "input.teacher_mode == false",
            div(class = "info-box", p("请在左侧勾选“显示教师自测区域”后运行自测。"))
          )
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
    P_df = as.data.frame(default_P),
    pi0_df = as.data.frame(matrix(default_pi0, nrow = 1)),
    state_names = default_state_names,
    n_states = default_n_states,
    calc_res = NULL
  )

  observe({
    colnames(rv$P_df) <- rv$state_names
    rownames(rv$P_df) <- rv$state_names
    colnames(rv$pi0_df) <- rv$state_names
    rownames(rv$pi0_df) <- "概率"
  })

  observeEvent(input$generate, {
    n <- input$n_states
    rv$n_states <- n
    rv$state_names <- paste0("状态", seq_len(n))
    rv$P_df <- as.data.frame(diag(n))
    rv$pi0_df <- as.data.frame(matrix(c(1, rep(0, n - 1)), nrow = 1))
    colnames(rv$P_df) <- rv$state_names
    rownames(rv$P_df) <- rv$state_names
    colnames(rv$pi0_df) <- rv$state_names
    rownames(rv$pi0_df) <- "概率"
    rv$calc_res <- NULL
  })

  observeEvent(input$reset_default, {
    rv$n_states <- default_n_states
    rv$state_names <- default_state_names
    rv$P_df <- as.data.frame(default_P)
    rv$pi0_df <- as.data.frame(matrix(default_pi0, nrow = 1))
    colnames(rv$P_df) <- rv$state_names
    rownames(rv$P_df) <- rv$state_names
    colnames(rv$pi0_df) <- rv$state_names
    rownames(rv$pi0_df) <- "概率"
    updateNumericInput(session, "n_states", value = default_n_states)
    updateNumericInput(session, "n_steps", value = 10)
    rv$calc_res <- NULL
  })

  output$P_hot <- renderRHandsontable({
    req(rv$P_df)
    rhandsontable(
      rv$P_df,
      rowHeaders = rownames(rv$P_df),
      stretchH = "all",
      height = 320,
      contextMenu = TRUE
    ) %>%
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.00") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }
      ")
  })

  output$pi0_hot <- renderRHandsontable({
    req(rv$pi0_df)
    rhandsontable(
      rv$pi0_df,
      rowHeaders = rownames(rv$pi0_df),
      stretchH = "all",
      height = 120,
      contextMenu = TRUE
    ) %>%
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.00") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }
      ")
  })

  calc <- eventReactive(input$calculate, {
    P_tbl <- hot_to_r(input$P_hot)
    pi0_tbl <- hot_to_r(input$pi0_hot)
    if (is.null(P_tbl) || is.null(pi0_tbl)) {
      showNotification("请先生成并填写转移矩阵与初始分布。", type = "error")
      return(NULL)
    }
    P <- suppressWarnings(as.matrix(P_tbl))
    pi0 <- suppressWarnings(as.numeric(pi0_tbl[1, ]))
    rownames(P) <- rv$state_names
    colnames(P) <- rv$state_names
    names(pi0) <- rv$state_names

    res <- tryCatch({
      evol <- calc_markov_evolution(P, pi0, input$n_steps)
      stat <- calc_markov_stationary(P)
      abs_res <- calc_absorbing_chain(P)
      list(evol = evol, stat = stat, abs = abs_res, P = P, pi0 = pi0)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    rv$calc_res <- res
    res
  })

  output$evolution_table <- renderDT({
    req(calc())
    df <- as.data.frame(round(calc()$evol, 4))
    df$t <- 0:(nrow(df) - 1)
    df <- df[, c("t", rv$state_names)]
    datatable(df, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE),
              caption = "状态概率 π_t = π_0 P^t 演化表")
  })

  output$evolution_plot <- renderPlot({
    req(calc())
    evol <- calc()$evol
    df <- as.data.frame(evol)
    df$t <- 0:(nrow(df) - 1)
    df_long <- pivot_longer(df, cols = -t, names_to = "状态", values_to = "概率")
    ggplot(df_long, aes(x = t, y = 概率, color = 状态)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(x = "步数 t", y = "状态概率", title = "状态概率随时间演化") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  output$stationary_table <- renderDT({
    req(calc())
    stat <- calc()$stat
    df <- data.frame(状态 = names(stat), 稳态概率 = round(stat, 4), stringsAsFactors = FALSE)
    datatable(df, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = "稳态分布 π = πP")
  })

  output$stationary_note <- renderUI({
    req(calc())
    stat <- calc()$stat
    P <- calc()$P
    check <- as.vector(stat %*% P)
    diff <- max(abs(check - stat))
    div(class = "info-box",
        h4("稳态校验"),
        p(sprintf("πP 与 π 的最大偏差为 %.6f（容差 1e-6）。", diff)),
        p("注意：若链不是不可约或非周期的，稳态分布可能不唯一或不存在；此时本页面给出的是线性方程组的一个非负归一化解。"))
  })

  output$absorbing_ui <- renderUI({
    req(calc())
    res <- calc()$abs
    if (!res$has_absorbing) {
      return(div(class = "info-box", p(res$msg)))
    }
    if (!res$has_transient) {
      return(div(class = "info-box", p(res$msg)))
    }
    tagList(
      div(class = "info-box",
          h4("吸收态与暂态"),
          p(paste0("吸收态：", paste(res$absorbing_names, collapse = "，"))),
          p(paste0("暂态：", paste(res$transient_names, collapse = "，")))),
      div(class = "info-box",
          h4("重排后的转移矩阵 [Q R; 0 I]"),
          DTOutput("abs_P_reord_table")),
      div(class = "info-box",
          h4("基本矩阵 N = (I - Q)^{-1}"),
          DTOutput("abs_N_table")),
      div(class = "info-box",
          h4("吸收概率 B = N R"),
          DTOutput("abs_B_table")),
      div(class = "info-box",
          h4("期望吸收时间 t = N · 1"),
          DTOutput("abs_t_table"))
    )
  })

  output$abs_P_reord_table <- renderDT({
    req(calc())
    res <- calc()$abs
    if (!res$has_absorbing || !res$has_transient) return(NULL)
    df <- as.data.frame(round(res$P_reord, 4))
    df$状态 <- c(res$transient_names, res$absorbing_names)
    df <- df[, c("状态", c(res$transient_names, res$absorbing_names))]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$abs_N_table <- renderDT({
    req(calc())
    res <- calc()$abs
    if (!res$has_absorbing || !res$has_transient) return(NULL)
    df <- as.data.frame(round(res$N, 4))
    df$暂态 <- rownames(res$N)
    df <- df[, c("暂态", colnames(res$N))]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$abs_B_table <- renderDT({
    req(calc())
    res <- calc()$abs
    if (!res$has_absorbing || !res$has_transient) return(NULL)
    df <- as.data.frame(round(res$B, 4))
    df$暂态 <- rownames(res$B)
    df <- df[, c("暂态", colnames(res$B))]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$abs_t_table <- renderDT({
    req(calc())
    res <- calc()$abs
    if (!res$has_absorbing || !res$has_transient) return(NULL)
    df <- data.frame(暂态 = names(res$t), 期望吸收时间 = round(res$t, 4), stringsAsFactors = FALSE)
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  self_test_res <- eventReactive(input$run_self_test, {
    run_markov_self_tests()
  })

  output$self_test_table <- renderDT({
    req(self_test_res())
    df <- self_test_res()
    df$是否通过 <- ifelse(df$是否通过,
                          "<span class='pass'>通过</span>",
                          "<span class='fail'>未通过</span>")
    datatable(df, rownames = FALSE, escape = FALSE,
              options = list(paging = FALSE, searching = FALSE, ordering = FALSE))
  })
}

shinyApp(ui, server)
