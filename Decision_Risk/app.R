# 依赖包：shiny, DT, ggplot2, rhandsontable, tidyr
# install.packages(c("shiny", "DT", "ggplot2", "rhandsontable", "tidyr"))

library(shiny)
library(DT)
library(ggplot2)
library(rhandsontable)
library(tidyr)

# =========================
# 工具函数
# =========================

# 概率校验：非负、和为 1
validate_probs <- function(p, name = "概率") {
  p <- suppressWarnings(as.numeric(p))
  if (any(is.na(p))) {
    return(list(valid = FALSE, msg = sprintf("%s 存在缺失或非数值，请检查输入。", name), p = p))
  }
  if (any(p < 0)) {
    p <- pmax(p, 0)
  }
  s <- sum(p)
  if (s == 0) {
    return(list(valid = FALSE, msg = sprintf("%s 之和不能为 0。", name), p = p))
  }
  if (abs(s - 1) > 1e-6) {
    p <- p / s
  }
  list(valid = TRUE, msg = NULL, p = p)
}

# 转移/似然矩阵校验：非负、每行和为 1
validate_likelihood <- function(M, name = "似然矩阵") {
  M <- suppressWarnings(as.numeric(M))
  if (any(is.na(M))) {
    return(list(valid = FALSE, msg = sprintf("%s 存在缺失或非数值。", name), M = M))
  }
  M <- matrix(pmax(M, 0), nrow = sqrt(length(M)))
  rs <- rowSums(M)
  if (any(rs == 0)) {
    return(list(valid = FALSE, msg = sprintf("%s 存在行和为 0 的行。", name), M = M))
  }
  if (any(abs(rs - 1) > 1e-6)) {
    M <- M / rs
  }
  list(valid = TRUE, msg = NULL, M = M)
}

# =========================
# 默认数据（教材第三章习题 2：产品包装改进）
# =========================
default_action_names <- c("大批量", "小批量")
default_state_names <- c("销路好", "销路差")
default_signal_names <- c("试销好", "试销差")

default_payoff <- matrix(
  c(800, -100,
    200, -20),
  nrow = 2, byrow = TRUE
)
default_prior <- c(0.6, 0.4)
default_likelihood <- matrix(
  c(0.80, 0.20,
    0.05, 0.95),
  nrow = 2, byrow = TRUE
)

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
    "))
  ),

  titlePanel(div(class = "title-main", "风险型决策分析教学网页：期望收益与完全信息价值")),

  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《风险型决策分析教学网页：期望收益与完全信息价值》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("问题类型"),
      radioButtons("problem_type", "收益型 / 成本型",
                   choices = c("收益型（最大化期望收益）" = "benefit",
                               "成本型（最小化期望成本）" = "cost"),
                   selected = "benefit"),
      helpText("收益型问题追求 EMV 最大；成本型问题追求 EC 最小。后悔值与 EVPI 的方向会随之改变。"),

      tags$hr(),
      h4("矩阵维度"),
      numericInput("n_actions", "决策方案数量", value = 2, min = 2, max = 20),
      numericInput("n_states", "自然状态数量", value = 2, min = 2, max = 20),
      actionButton("generate", "生成/重置矩阵", class = "btn-primary"),
      actionButton("reset_default", "恢复教材默认值", class = "btn-warning"),
      tags$hr(),

      h4("操作"),
      actionButton("calculate", "计算决策结果", class = "btn-success"),
      tags$hr(),

      helpText("输入说明："),
      tags$ul(
        tags$li("收益/成本矩阵：行=方案，列=自然状态，单位为万元。"),
        tags$li("先验概率：各自然状态的概率，必须非负且和为 1。"),
        tags$li("似然矩阵：行=真实状态，列=试销结果/信号，每行之和为 1。"),
        tags$li("可直接在表格中编辑，支持复制粘贴。")
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
            h4("问题背景"),
            p("风险型决策是指决策者知道各自然状态出现的概率，并据此计算期望指标进行决策。本网页以教材第三章“风险型决策分析”为背景，帮助理解期望准则、后悔值、完全情报价值（EVPI）与样本情报价值（EVSI）的计算与含义。"),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("掌握收益型与成本型问题的期望准则：收益型最大化 EMV，成本型最小化 EC；"),
              tags$li("理解完全情报价值 EVPI 的经济含义及其与最小期望机会损失（EOL）的关系；"),
              tags$li("掌握贝叶斯公式，由先验概率与似然矩阵计算后验概率；"),
              tags$li("理解 EVSI 与抽样/试销决策的经济权衡。")
            ),
            tags$hr(),
            h4("默认案例：产品包装改进"),
            p("某食品公司准备改进产品包装。大批量生产在销路好时获利 800 万元，销路差时损失 100 万元；小批量生产在销路好时获利 200 万元，销路差时损失 20 万元。销路好与差的先验概率分别为 0.6 与 0.4。公司可小批量试销后再决策。")
          )
        ),

        tabPanel(
          "输入数据",
          br(),
          div(
            class = "info-box",
            h4("收益/成本矩阵（万元）"),
            helpText("行=决策方案，列=自然状态。收益型填收益，成本型填成本。"),
            rHandsontableOutput("payoff_hot")
          ),
          div(
            class = "info-box",
            h4("先验概率"),
            helpText("各自然状态的概率，非负且和必须等于 1。"),
            rHandsontableOutput("prior_hot")
          ),
          div(
            class = "info-box",
            h4("似然矩阵 P(信号 | 真实状态)"),
            helpText("行=真实状态，列=试销/信号结果，每行概率之和必须等于 1。"),
            rHandsontableOutput("likelihood_hot")
          )
        ),

        tabPanel(
          "先验决策",
          br(),
          uiOutput("prior_metric_cards"),
          br(),
          div(
            class = "info-box",
            h4("各方案期望指标"),
            DTOutput("prior_table")
          ),
          div(
            class = "info-box",
            h4("机会损失矩阵与期望机会损失"),
            uiOutput("regret_note"),
            DTOutput("regret_table")
          ),
          div(
            class = "info-box",
            h4("完全情报价值 EVPI"),
            uiOutput("evpi_box")
          ),
          br(),
          uiOutput("prior_explain")
        ),

        tabPanel(
          "贝叶斯后验决策",
          br(),
          div(
            class = "info-box",
            h4("后验概率 P(真实状态 | 信号)"),
            DTOutput("posterior_table")
          ),
          div(
            class = "info-box",
            h4("各信号下的最优决策"),
            DTOutput("posterior_decision_table")
          ),
          div(
            class = "info-box",
            h4("样本情报价值 EVSI"),
            uiOutput("evsi_box")
          ),
          br(),
          uiOutput("posterior_explain")
        ),

        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("先验概率变化对最优方案的影响"),
            plotOutput("prior_sensitivity_plot", height = "380px"),
            tags$p(class = "small-note", "横轴为第一个自然状态的先验概率；曲线表示各方案的期望收益/成本。")
          ),
          div(
            class = "info-box",
            h4("后验概率分布"),
            plotOutput("posterior_plot", height = "360px")
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
    payoff_df = as.data.frame(default_payoff),
    prior_df = as.data.frame(matrix(default_prior, nrow = 1)),
    likelihood_df = as.data.frame(default_likelihood),
    action_names = default_action_names,
    state_names = default_state_names,
    signal_names = default_signal_names,
    res = NULL
  )

  # 初始化行/列名
  observe({
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
    colnames(rv$prior_df) <- rv$state_names
    colnames(rv$likelihood_df) <- rv$signal_names
    rownames(rv$likelihood_df) <- rv$state_names
  })

  observeEvent(input$generate, {
    n_a <- input$n_actions
    n_s <- input$n_states
    rv$action_names <- paste0("方案", seq_len(n_a))
    rv$state_names <- paste0("状态", seq_len(n_s))
    rv$signal_names <- paste0("信号", seq_len(n_s))
    rv$payoff_df <- as.data.frame(matrix(0, nrow = n_a, ncol = n_s))
    rv$prior_df <- as.data.frame(matrix(0, nrow = 1, ncol = n_s))
    rv$likelihood_df <- as.data.frame(diag(n_s))
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
    colnames(rv$prior_df) <- rv$state_names
    colnames(rv$likelihood_df) <- rv$signal_names
    rownames(rv$likelihood_df) <- rv$state_names
    rv$res <- NULL
  })

  observeEvent(input$reset_default, {
    rv$action_names <- default_action_names
    rv$state_names <- default_state_names
    rv$signal_names <- default_signal_names
    rv$payoff_df <- as.data.frame(default_payoff)
    rv$prior_df <- as.data.frame(matrix(default_prior, nrow = 1))
    rv$likelihood_df <- as.data.frame(default_likelihood)
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
    colnames(rv$prior_df) <- rv$state_names
    colnames(rv$likelihood_df) <- rv$signal_names
    rownames(rv$likelihood_df) <- rv$state_names
    updateRadioButtons(session, "problem_type", selected = "benefit")
    updateNumericInput(session, "n_actions", value = 2)
    updateNumericInput(session, "n_states", value = 2)
    rv$res <- NULL
  })

  output$payoff_hot <- renderRHandsontable({
    req(rv$payoff_df)
    rhandsontable(
      rv$payoff_df,
      rowHeaders = rownames(rv$payoff_df),
      stretchH = "all",
      height = 280,
      contextMenu = TRUE
    ) %>%
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }
      ")
  })

  output$prior_hot <- renderRHandsontable({
    req(rv$prior_df)
    rhandsontable(
      rv$prior_df,
      rowHeaders = c("概率"),
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

  output$likelihood_hot <- renderRHandsontable({
    req(rv$likelihood_df)
    rhandsontable(
      rv$likelihood_df,
      rowHeaders = rownames(rv$likelihood_df),
      stretchH = "all",
      height = 280,
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

  observeEvent(input$calculate, {
    # 读取表格
    payoff_tbl <- hot_to_r(input$payoff_hot)
    prior_tbl <- hot_to_r(input$prior_hot)
    likelihood_tbl <- hot_to_r(input$likelihood_hot)

    if (is.null(payoff_tbl) || is.null(prior_tbl) || is.null(likelihood_tbl)) {
      showNotification("请先生成并填写所有矩阵。", type = "error")
      return()
    }

    payoff <- suppressWarnings(as.matrix(payoff_tbl))
    prior <- suppressWarnings(as.numeric(prior_tbl[1, ]))
    likelihood <- suppressWarnings(as.matrix(likelihood_tbl))

    if (any(is.na(payoff)) || any(is.na(prior)) || any(is.na(likelihood))) {
      showNotification("输入存在缺失或非数值，请检查。", type = "error")
      return()
    }

    vp <- validate_probs(prior, "先验概率")
    if (!vp$valid) {
      showNotification(vp$msg, type = "error")
      return()
    }
    if (!is.null(vp$msg)) showNotification("先验概率已自动归一化。", type = "warning")
    prior <- vp$p

    vl <- validate_likelihood(likelihood, "似然矩阵")
    if (!vl$valid) {
      showNotification(vl$msg, type = "error")
      return()
    }
    if (!is.null(vl$msg)) showNotification("似然矩阵每行已自动归一化。", type = "warning")
    likelihood <- vl$M

    n_a <- nrow(payoff)
    n_s <- ncol(payoff)
    n_k <- ncol(likelihood)
    action_names <- rownames(payoff_tbl)
    state_names <- colnames(payoff_tbl)
    signal_names <- colnames(likelihood_tbl)

    is_benefit <- input$problem_type == "benefit"

    # 先验期望
    prior_mat <- matrix(prior, nrow = n_a, ncol = n_s, byrow = TRUE)
    expected <- rowSums(payoff * prior_mat)
    names(expected) <- action_names

    if (is_benefit) {
      best_val <- max(expected)
      best_idx <- which(expected == best_val)
      best_action <- paste(action_names[best_idx], collapse = ", ")
      perfect_info_value <- sum(prior * apply(payoff, 2, max))
      evpi <- perfect_info_value - best_val
      regret <- sweep(matrix(apply(payoff, 2, max), nrow = n_a, ncol = n_s, byrow = TRUE),
                      1:2, payoff, "-")
    } else {
      best_val <- min(expected)
      best_idx <- which(expected == best_val)
      best_action <- paste(action_names[best_idx], collapse = ", ")
      perfect_info_value <- sum(prior * apply(payoff, 2, min))
      evpi <- best_val - perfect_info_value
      regret <- sweep(payoff, 2, apply(payoff, 2, min), "-")
    }
    evpi <- max(evpi, 0)

    eol <- rowSums(regret * prior_mat)

    # 贝叶斯
    sample_margin <- as.vector(prior %*% likelihood)
    names(sample_margin) <- signal_names

    posterior <- sweep(likelihood, 2, sample_margin, "/")
    posterior <- apply(posterior, 2, function(x) {
      s <- sum(x)
      if (s == 0) x else x / s
    })
    rownames(posterior) <- state_names
    colnames(posterior) <- signal_names

    post_decisions <- lapply(seq_len(n_k), function(j) {
      post_prob <- posterior[, j]
      exp_post <- payoff %*% post_prob
      if (is_benefit) {
        best_post_val <- max(exp_post)
        best_post_idx <- which(exp_post == best_post_val)
      } else {
        best_post_val <- min(exp_post)
        best_post_idx <- which(exp_post == best_post_val)
      }
      data.frame(
        信号 = signal_names[j],
        边际概率 = round(sample_margin[j], 4),
        最优方案 = paste(action_names[best_post_idx], collapse = ", "),
        最优期望指标 = round(best_post_val, 4),
        stringsAsFactors = FALSE
      )
    })
    post_df <- do.call(rbind, post_decisions)

    expected_posterior_value <- sum(sample_margin * post_df$最优期望指标)
    if (is_benefit) {
      evsi <- expected_posterior_value - best_val
    } else {
      evsi <- best_val - expected_posterior_value
    }
    evsi <- max(evsi, 0)

    rv$res <- list(
      payoff = payoff,
      prior = prior,
      likelihood = likelihood,
      posterior = posterior,
      expected = expected,
      best_val = best_val,
      best_action = best_action,
      best_idx = best_idx,
      perfect_info_value = perfect_info_value,
      evpi = evpi,
      regret = regret,
      eol = eol,
      post_df = post_df,
      evsi = evsi,
      expected_posterior_value = expected_posterior_value,
      is_benefit = is_benefit,
      action_names = action_names,
      state_names = state_names,
      signal_names = signal_names
    )
  })

  output$prior_metric_cards <- renderUI({
    req(rv$res)
    label <- if (rv$res$is_benefit) "最大期望收益（EMV）" else "最小期望成本（EC）"
    fluidRow(
      column(4, div(class = "metric-card",
                    div(class = "metric-title", "先验最优方案"),
                    div(class = "metric-value", rv$res$best_action),
                    div(class = "metric-note", if (rv$res$is_benefit) "EMV 最大" else "EC 最小"))),
      column(4, div(class = "metric-card",
                    div(class = "metric-title", label),
                    div(class = "metric-value", sprintf("%.2f 万元", rv$res$best_val)))),
      column(4, div(class = "metric-card",
                    div(class = "metric-title", "EVPI"),
                    div(class = "metric-value", sprintf("%.2f 万元", rv$res$evpi)),
                    div(class = "metric-note", "完全情报价值")))
    )
  })

  output$prior_table <- renderDT({
    req(rv$res)
    res <- rv$res
    col_name <- if (res$is_benefit) "期望收益 EMV（万元）" else "期望成本 EC（万元）"
    df <- data.frame(
      方案 = res$action_names,
      期望指标 = round(res$expected, 4),
      是否最优 = ifelse(seq_along(res$action_names) %in% res$best_idx, "是", "否"),
      stringsAsFactors = FALSE
    )
    colnames(df)[2] <- col_name
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = if (res$is_benefit) "先验决策表（收益型）" else "先验决策表（成本型）")
  })

  output$regret_note <- renderUI({
    req(rv$res)
    if (rv$res$is_benefit) {
      p(class = "small-note", "收益型机会损失 = 该状态下最大收益 - 所选方案收益；最小期望机会损失应等于 EVPI。")
    } else {
      p(class = "small-note", "成本型机会损失 = 所选方案成本 - 该状态下最小成本；最小期望机会损失应等于 EVPI。")
    }
  })

  output$regret_table <- renderDT({
    req(rv$res)
    res <- rv$res
    df <- as.data.frame(round(res$regret, 4))
    df$方案 <- res$action_names
    df$期望机会损失_EOL <- round(res$eol, 4)
    df <- df[, c("方案", res$state_names, "期望机会损失_EOL")]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = "机会损失矩阵与期望机会损失")
  })

  output$evpi_box <- renderUI({
    req(rv$res)
    res <- rv$res
    if (res$is_benefit) {
      txt <- paste0(
        "EVPI = 完全情报期望收益 - 先验最优期望收益<br/>",
        "= ", round(res$perfect_info_value, 4), " - ", round(res$best_val, 4),
        " = <b>", round(res$evpi, 4), " 万元</b><br/><br/>",
        "含义：为获得完全情报所愿支付的最高价格；若情报费用低于 EVPI，则获取情报有利。"
      )
    } else {
      txt <- paste0(
        "EVPI = 先验最优期望成本 - 完全情报期望成本<br/>",
        "= ", round(res$best_val, 4), " - ", round(res$perfect_info_value, 4),
        " = <b>", round(res$evpi, 4), " 万元</b><br/><br/>",
        "含义：为获得完全情报所愿支付的最高价格；若情报费用低于 EVPI，则获取情报有利。"
      )
    }
    div(class = "formula-box", HTML(txt))
  })

  output$prior_explain <- renderUI({
    req(rv$res)
    res <- rv$res
    type_txt <- if (res$is_benefit) "期望收益最大" else "期望成本最小"
    explain <- sprintf("根据%s准则，推荐方案为 <b>%s</b>，其先验%s为 %.4f 万元。",
                       type_txt, res$best_action,
                       if (res$is_benefit) "EMV" else "EC",
                       res$best_val)
    eol_txt <- sprintf("最小期望机会损失为 %.4f 万元，与 EVPI（%.4f 万元）一致（允许四舍五入误差）。",
                       min(res$eol), res$evpi)
    div(class = "info-box",
        h4("决策解释"),
        p(HTML(explain)),
        p(HTML(eol_txt)),
        p("若多个方案并列最优，推荐结果中将同时列出。"))
  })

  output$posterior_table <- renderDT({
    req(rv$res)
    df <- as.data.frame(round(rv$res$posterior, 4))
    df$真实状态 <- rownames(rv$res$posterior)
    df <- df[, c("真实状态", rv$res$signal_names)]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = "贝叶斯后验概率表")
  })

  output$posterior_decision_table <- renderDT({
    req(rv$res)
    datatable(rv$res$post_df, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = if (rv$res$is_benefit) "各信号下的最优决策（收益型）" else "各信号下的最优决策（成本型）")
  })

  output$evsi_box <- renderUI({
    req(rv$res)
    res <- rv$res
    div(class = "formula-box",
        HTML(paste0(
          "EVSI = |后验最优期望指标 - 先验最优期望指标|<br/>",
          "= |", round(res$expected_posterior_value, 4), " - ", round(res$best_val, 4),
          "| = <b>", round(res$evsi, 4), " 万元</b><br/><br/>",
          "含义：若试销/调查费用低于 EVSI，则进行抽样在经济上是值得的。"
        )))
  })

  output$posterior_explain <- renderUI({
    req(rv$res)
    res <- rv$res
    txt <- sprintf("在各信号下分别按%s准则决策，再按信号边际概率加权，得到后验最优期望指标为 %.4f 万元。",
                   if (res$is_benefit) "期望收益最大" else "期望成本最小",
                   res$expected_posterior_value)
    div(class = "info-box",
        h4("决策解释"),
        p(txt),
        p(sprintf("EVSI = %.4f 万元。若试销费用低于该值，则进行试销有利。", res$evsi)))
  })

  output$prior_sensitivity_plot <- renderPlot({
    req(rv$res)
    res <- rv$res
    n_s <- length(res$state_names)
    if (n_s != 2) {
      # 多于两个状态时，固定其他状态等比例变化较复杂，此处提示
      return(
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = "仅当自然状态为 2 个时绘制此图") +
          theme_void()
      )
    }
    p_seq <- seq(0, 1, length.out = 101)
    df_list <- lapply(seq_along(res$action_names), function(i) {
      val <- p_seq * res$payoff[i, 1] + (1 - p_seq) * res$payoff[i, 2]
      data.frame(p = p_seq, 期望指标 = val, 方案 = res$action_names[i])
    })
    df <- do.call(rbind, df_list)
    y_label <- if (res$is_benefit) "期望收益（万元）" else "期望成本（万元）"
    ggplot(df, aes(x = p, y = 期望指标, color = 方案)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = res$prior[1], linetype = "dotted", color = "gray40") +
      labs(x = paste0(res$state_names[1], "的先验概率"), y = y_label,
           title = "先验概率变化对最优方案的影响") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  output$posterior_plot <- renderPlot({
    req(rv$res)
    res <- rv$res
    df <- as.data.frame(res$posterior)
    df$真实状态 <- rownames(res$posterior)
    df_long <- pivot_longer(df, cols = -真实状态, names_to = "信号", values_to = "概率")
    ggplot(df_long, aes(x = 信号, y = 概率, fill = 真实状态)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(概率, 3)),
                position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
      labs(x = NULL, y = "后验概率", fill = "真实状态", title = "后验概率分布") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
