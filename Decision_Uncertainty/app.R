# 依赖包：shiny, rhandsontable, DT, ggplot2
# install.packages(c("shiny", "rhandsontable", "DT", "ggplot2", "tidyr"))

library(shiny)
library(rhandsontable)
library(DT)
library(ggplot2)
library(tidyr)

# =========================
# 教材第四章习题 7 默认收益矩阵（钟表公司设计方案）
# 方案1：投资10万元，单位成本15元，售价25元，单位利润10元
# 方案2：投资16万元，单位成本12元，售价25元，单位利润13元
# 方案3：投资25万元，单位成本 8元，售价25元，单位利润17元
# 需求量：3万、12万、20万只
# =========================
default_payoff <- matrix(
  c(20, 110, 190,    # 方案1：3万*10-10万，12万*10-10万，20万*10-10万（单位：万元）
    23, 140, 244,    # 方案2：3万*13-16万，12万*13-16万，20万*13-16万
    26, 179, 315),   # 方案3：3万*17-25万，12万*17-25万，20万*17-25万
  nrow = 3, byrow = TRUE
)
rownames(default_payoff) <- c("方案1", "方案2", "方案3")
colnames(default_payoff) <- c("需求3万", "需求12万", "需求20万")

# =========================
# 工具函数
# =========================

# 不确定型决策核心计算
calc_uncertainty_decision <- function(payoff, alpha, is_benefit) {
  n_a <- nrow(payoff)
  n_s <- ncol(payoff)

  if (is_benefit) {
    maximax <- apply(payoff, 1, max)
    maximin <- apply(payoff, 1, min)
    laplace <- rowMeans(payoff)

    best_each_state <- apply(payoff, 2, max)
    regret <- sweep(
      matrix(best_each_state, nrow = n_a, ncol = n_s, byrow = TRUE),
      1:2, payoff, "-"
    )

    hurwicz <- alpha * apply(payoff, 1, max) + (1 - alpha) * apply(payoff, 1, min)

    result_df <- data.frame(
      决策方案 = rownames(payoff),
      Maximax_乐观 = round(maximax, 4),
      Maximin_悲观 = round(maximin, 4),
      Laplace_等可能 = round(laplace, 4),
      MinimaxRegret_最小最大后悔 = round(apply(regret, 1, max), 4),
      Hurwicz_折中 = round(hurwicz, 4),
      check.names = FALSE
    )
  } else {
    minimin <- apply(payoff, 1, min)
    minimax_cost <- apply(payoff, 1, max)
    laplace <- rowMeans(payoff)

    best_each_state <- apply(payoff, 2, min)
    regret <- sweep(payoff, 2, best_each_state, "-")

    hurwicz <- alpha * apply(payoff, 1, min) + (1 - alpha) * apply(payoff, 1, max)

    result_df <- data.frame(
      决策方案 = rownames(payoff),
      Minimin_乐观 = round(minimin, 4),
      Minimax_悲观 = round(minimax_cost, 4),
      Laplace_等可能 = round(laplace, 4),
      MinimaxRegret_最小最大后悔 = round(apply(regret, 1, max), 4),
      Hurwicz_折中 = round(hurwicz, 4),
      check.names = FALSE
    )
  }

  rownames(regret) <- rownames(payoff)
  colnames(regret) <- colnames(payoff)

  list(
    result_df = result_df,
    regret = regret,
    best_each_state = best_each_state,
    is_benefit = is_benefit
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

  titlePanel(div(class = "title-main", "不确定型决策分析教学网页：乐观、悲观、Hurwicz、Laplace 与 Savage 准则")),

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
      h4("问题类型"),
      radioButtons("problem_type", "收益型 / 成本型",
                   choices = c("收益型（收益越大越好）" = "benefit",
                               "成本型（成本越小越好）" = "cost"),
                   selected = "benefit"),
      helpText("收益型问题选“收益型”，成本型问题选“成本型”。各决策准则的方向会随之改变。"),
      tags$hr(),

      h4("矩阵维度"),
      numericInput("n_decisions", "决策方案数量", value = 3, min = 1, max = 20),
      numericInput("n_states", "自然状态数量", value = 3, min = 1, max = 20),
      actionButton("generate", "生成/重置矩阵", class = "btn-primary"),
      actionButton("reset_default", "恢复教材默认值", class = "btn-warning"),
      tags$hr(),

      h4("不确定型准则参数"),
      sliderInput("alpha", "Hurwicz 乐观系数 α", min = 0, max = 1, value = 0.5, step = 0.01),
      helpText("α 越接近 1 越乐观，越接近 0 越悲观。收益型：α×最好收益 + (1-α)×最差收益；成本型：α×最小成本 + (1-α)×最大成本。"),
      tags$hr(),

      actionButton("calculate", "计算决策结果", class = "btn-success"),
      tags$hr(),

      helpText("输入说明："),
      tags$ul(
        tags$li("先输入矩阵维度，再点击“生成/重置收益矩阵”"),
        tags$li("可像 Excel 一样直接在表格中填写"),
        tags$li("支持复制粘贴、Tab 键切换、方向键移动"),
        tags$li("收益/成本矩阵：行=方案，列=自然状态"),
        tags$li("不确定型决策不输入概率；若需要风险型分析，请使用“风险型决策”教学网页。")
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
            h4("教学目标"),
            tags$p("本章对应教材第四章“不确定型决策分析”。通过本网页，学生可以："),
            tags$ul(
              tags$li("理解 Maximax、Maximin、Laplace、Minimax Regret、Hurwicz 等不确定型决策准则；"),
              tags$li("比较不同准则下推荐方案可能不同，体会决策者的风险偏好对方案选择的影响；"),
              tags$li("区分收益型与成本型问题在准则计算和后悔值方向上的差异；"),
              tags$li("通过图形直观比较各方案在不同准则下的表现。")
            ),
            tags$details(
              tags$summary("查看计算说明"),
              br(),
              tags$ul(
                tags$li("收益型：Maximax（乐观）= 各行最大值再取最大；Maximin（悲观）= 各行最小值再取最大；Laplace = 行平均；Hurwicz = α×行最大 + (1-α)×行最小；Minimax Regret = 先按列求后悔值，再取各行最大后悔值中的最小。"),
                tags$li("成本型：Minimin（乐观）= 各行最小值再取最小；Minimax（悲观）= 各行最大值再取最小；Laplace = 行平均；Hurwicz = α×行最小 + (1-α)×行最大；Minimax Regret 同样按列求后悔值。"),
                tags$li("Laplace 准则是“等可能”假设，即认为各自然状态发生的机会相同，并不代表已知概率。"),
                tags$li("Savage 准则又称最小最大后悔准则，后悔值是在每个自然状态下“选错方案”所造成的损失。")
              )
            ),
            tags$hr(),
            h4("默认案例：教材第四章习题 7"),
            tags$p("某钟表公司计划通过销售网推销一种低价钟表，计划零售价每块25元。有三种设计方案："),
            tags$ul(
              tags$li("方案1：一次性投资10万元，投产后每块成本15元；"),
              tags$li("方案2：一次性投资16万元，投产后每块成本12元；"),
              tags$li("方案3：一次性投资25万元，投产后每块成本 8元。")
            ),
            tags$p("该钟表需求量不确切，估计有三种可能：3万、12万、20万只。请建立收益矩阵，并分别用乐观法、悲观法、等概率法、后悔值法决定应采用哪个设计方案。"),
            tags$hr(),
            h4("重要提示"),
            div(class = "warning-box",
                tags$p("不同决策准则可能给出不同推荐方案，这不是计算错误，而是反映了决策者风险偏好不同：乐观者偏好 Maximax/Minimin，稳健者偏好 Maximin/Minimax 或 Minimax Regret，等概率者采用 Laplace。"))
          )
        ),

        tabPanel(
          "收益矩阵输入",
          br(),
          div(
            class = "info-box",
            h4("收益/成本矩阵"),
            helpText("行表示决策方案，列表示自然状态。收益型问题填入收益，成本型问题填入成本。"),
            rHandsontableOutput("payoff_hot")
          )
        ),

        tabPanel(
          "决策结果",
          br(),
          uiOutput("problem_note"),
          DTOutput("result_table"),
          br(),
          uiOutput("best_summary")
        ),

        tabPanel(
          "后悔矩阵",
          br(),
          uiOutput("regret_note"),
          DTOutput("regret_table")
        ),

        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("各方案在不同准则下的评价值"),
            plotOutput("criteria_plot", height = "420px"),
            tags$p(class = "small-note", "注：Minimax Regret 为越小越好，其余收益型准则为越大越好；成本型准则方向相反。")
          ),
          div(
            class = "info-box",
            h4("各方案收益/成本分布"),
            plotOutput("payoff_plot", height = "360px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  create_matrix_df <- function(n_decisions, n_states, default_value = 0) {
    df <- as.data.frame(matrix(default_value, nrow = n_decisions, ncol = n_states))
    colnames(df) <- paste0("状态", seq_len(n_states))
    rownames(df) <- paste0("决策", seq_len(n_decisions))
    df
  }

  rv <- reactiveValues(
    payoff_df = as.data.frame(default_payoff),
    result = NULL
  )

  observeEvent(input$generate, {
    rv$payoff_df <- create_matrix_df(input$n_decisions, input$n_states)
    rv$result <- NULL
  })

  observeEvent(input$reset_default, {
    rv$payoff_df <- as.data.frame(default_payoff)
    updateNumericInput(session, "n_decisions", value = 3)
    updateNumericInput(session, "n_states", value = 3)
    updateSliderInput(session, "alpha", value = 0.5)
    updateRadioButtons(session, "problem_type", selected = "benefit")
    rv$result <- NULL
  })

  output$payoff_hot <- renderRHandsontable({
    req(rv$payoff_df)

    rhandsontable(
      rv$payoff_df,
      rowHeaders = rownames(rv$payoff_df),
      stretchH = "all",
      height = 320,
      contextMenu = TRUE
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
          td.style.verticalAlign = 'middle';
        }
      ") %>%
      hot_col(col = colnames(rv$payoff_df), type = "numeric", format = "0")
  })

  observeEvent(input$calculate, {
    tbl <- hot_to_r(input$payoff_hot)

    if (is.null(tbl)) {
      showNotification("请先生成并填写收益/成本矩阵。", type = "error")
      return()
    }

    tbl <- as.data.frame(tbl)
    rownames(tbl) <- rownames(rv$payoff_df)

    for (j in seq_along(tbl)) {
      tbl[[j]] <- suppressWarnings(as.numeric(tbl[[j]]))
    }

    rv$payoff_df <- tbl
    payoff <- as.matrix(tbl)

    if (any(is.na(payoff))) {
      showNotification("请先将收益/成本矩阵填写完整。", type = "error")
      return()
    }

    if (any(!is.finite(payoff))) {
      showNotification("收益/成本矩阵存在非有限数值，请检查。", type = "error")
      return()
    }

    is_benefit <- input$problem_type == "benefit"
    alpha <- input$alpha
    if (is.na(alpha) || alpha < 0 || alpha > 1) {
      showNotification("Hurwicz 系数 α 必须在 0 到 1 之间。", type = "error")
      return()
    }

    rv$result <- calc_uncertainty_decision(payoff, alpha, is_benefit)
  })

  output$problem_note <- renderUI({
    req(rv$result)
    if (rv$result$is_benefit) {
      div(class = "info-box",
          p(strong("当前为收益型问题。"), "各准则目标为收益最大化：Maximax、Maximin、Laplace、Hurwicz 越大越好；Minimax Regret 越小越好。"))
    } else {
      div(class = "info-box",
          p(strong("当前为成本型问题。"), "各准则目标为成本最小化：Minimin（乐观）= 各方案最小成本中的最小值，Minimax（悲观）= 各方案最大成本中的最小值；Laplace、Hurwicz 越小越好；Minimax Regret 越小越好。"))
    }
  })

  output$result_table <- renderDT({
    req(rv$result)
    datatable(
      rv$result$result_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = if (rv$result$is_benefit) "不确定型决策准则结果（收益型）" else "不确定型决策准则结果（成本型）"
    )
  })

  output$best_summary <- renderUI({
    req(rv$result)
    df <- rv$result$result_df
    is_benefit <- rv$result$is_benefit

    pick_best <- function(values, maximize) {
      idx <- if (maximize) which(abs(values - max(values)) < 1e-9) else which(abs(values - min(values)) < 1e-9)
      paste(df$决策方案[idx], collapse = ", ")
    }

    if (is_benefit) {
      best_maximax <- pick_best(df$Maximax_乐观, TRUE)
      best_maximin <- pick_best(df$Maximin_悲观, TRUE)
      best_laplace <- pick_best(df$Laplace_等可能, TRUE)
      best_hurwicz <- pick_best(df$Hurwicz_折中, TRUE)
    } else {
      best_maximax <- pick_best(df$Minimin_乐观, FALSE)
      best_maximin <- pick_best(df$Minimax_悲观, FALSE)
      best_laplace <- pick_best(df$Laplace_等可能, FALSE)
      best_hurwicz <- pick_best(df$Hurwicz_折中, FALSE)
    }
    best_minimax_regret <- pick_best(df$MinimaxRegret_最小最大后悔, FALSE)

    maximax_label <- if (is_benefit) "Maximax（乐观准则）" else "Minimin（乐观准则）"
    maximin_label <- if (is_benefit) "Maximin（悲观准则）" else "Minimax（悲观准则）"

    div(
      class = "info-box",
      h4("推荐方案汇总"),
      tags$ul(
        tags$li(sprintf("%s推荐：%s", maximax_label, best_maximax)),
        tags$li(sprintf("%s推荐：%s", maximin_label, best_maximin)),
        tags$li(sprintf("Laplace（等可能准则）推荐：%s", best_laplace)),
        tags$li(sprintf("Minimax Regret（最小最大后悔准则）推荐：%s", best_minimax_regret)),
        tags$li(sprintf("Hurwicz（折中准则，α=%.2f）推荐：%s", input$alpha, best_hurwicz))
      ),
      p("提示：不同准则可能推荐不同方案，这反映了决策者风险偏好的差异，并非计算错误。若某准则出现多个方案并列最优，上面会同时列出。")
    )
  })

  output$regret_note <- renderUI({
    req(rv$result)
    if (rv$result$is_benefit) {
      div(class = "info-box",
          p("收益型后悔值：regret_ij = max_i x_ij - x_ij，表示选择方案 i 而实际状态 j 发生时，与最优方案之间的收益差距。"))
    } else {
      div(class = "info-box",
          p("成本型后悔值：regret_ij = c_ij - min_i c_ij，表示选择方案 i 而实际状态 j 发生时，与最小成本方案之间的成本差距。"))
    }
  })

  output$regret_table <- renderDT({
    req(rv$result)
    df <- data.frame(
      决策方案 = rownames(rv$result$regret),
      as.data.frame(round(rv$result$regret, 4), check.names = FALSE),
      check.names = FALSE
    )
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "后悔矩阵"
    )
  })

  output$criteria_plot <- renderPlot({
    req(rv$result)
    df <- rv$result$result_df
    df_long <- tidyr::pivot_longer(
      df,
      cols = -决策方案,
      names_to = "准则",
      values_to = "评价值"
    )
    df_long$准则 <- factor(df_long$准则, levels = names(df)[-1])

    y_label <- if (rv$result$is_benefit) "评价值（万元）" else "评价值（万元）"

    ggplot(df_long, aes(x = 准则, y = 评价值, fill = 决策方案, group = 决策方案)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(评价值, 1)),
                position = position_dodge(width = 0.8),
                vjust = -0.5, size = 3.5) +
      labs(x = NULL, y = y_label, fill = "方案") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  output$payoff_plot <- renderPlot({
    req(rv$payoff_df)
    df <- rv$payoff_df
    df$方案 <- rownames(df)
    df_long <- tidyr::pivot_longer(
      df,
      cols = -方案,
      names_to = "状态",
      values_to = "值"
    )

    y_label <- if (rv$result$is_benefit) "收益（万元）" else "成本（万元）"

    ggplot(df_long, aes(x = 状态, y = 值, fill = 方案)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = 值),
                position = position_dodge(width = 0.8),
                vjust = -0.4, size = 3.5) +
      labs(x = NULL, y = y_label, fill = "方案") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
