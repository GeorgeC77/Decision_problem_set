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

default_probs <- c(1/3, 1/3, 1/3)
names(default_probs) <- colnames(default_payoff)

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
      actionButton("generate", "生成/重置收益矩阵", class = "btn-primary"),
      tags$hr(),
      
      h4("不确定型准则参数"),
      sliderInput("alpha", "Hurwicz 乐观系数 α", min = 0, max = 1, value = 0.5, step = 0.01),
      helpText("α 越接近 1 越乐观，越接近 0 越悲观。"),
      tags$hr(),
      
      h4("风险型决策参数"),
      helpText("在“概率输入”选项卡中可输入各状态概率，计算期望收益/成本、EVPI 等。概率之和必须等于 1。"),
      tags$hr(),
      
      actionButton("calculate", "计算决策结果", class = "btn-success"),
      tags$hr(),
      
      helpText("输入说明："),
      tags$ul(
        tags$li("先输入矩阵维度，再点击“生成/重置收益矩阵”"),
        tags$li("可像 Excel 一样直接在表格中填写"),
        tags$li("支持复制粘贴、Tab 键切换、方向键移动"),
        tags$li("收益矩阵：行=方案，列=自然状态"),
        tags$li("状态概率之和必须等于 1")
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
              tags$li("在已知状态概率时，计算风险型决策的期望收益/成本、完全情报价值 EVPI 等；"),
              tags$li("区分收益型与成本型问题在准则计算和后悔值方向上的差异；"),
              tags$li("通过图形直观比较各方案在不同准则下的表现。")
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
                tags$p("不同决策准则可能给出不同推荐方案，这不是计算错误，而是反映了决策者风险偏好不同：乐观者偏好 Maximax，稳健者偏好 Maximin 或 Minimax Regret，等概率者采用 Laplace。")
            )
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
          ),
          div(
            class = "info-box",
            h4("状态概率输入（仅用于风险型决策）"),
            helpText("不确定型决策准则不使用概率。若需要计算风险型指标（期望收益/成本、EVPI），请在下表填写概率；不填或填 0 表示只进行不确定型分析。概率之和必须等于 1。"),
            rHandsontableOutput("prob_hot")
          )
        ),
        
        tabPanel(
          "决策结果",
          br(),
          uiOutput("problem_note"),
          DTOutput("result_table"),
          br(),
          uiOutput("best_summary"),
          br(),
          DTOutput("risk_table"),
          br(),
          uiOutput("evpi_box")
        ),
        
        tabPanel(
          "遗憾矩阵",
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
  
  create_prob_df <- function(n_states) {
    df <- as.data.frame(matrix(0, nrow = 1, ncol = n_states))
    colnames(df) <- paste0("状态", seq_len(n_states))
    rownames(df) <- "概率"
    df
  }
  
  rv <- reactiveValues(
    payoff_df = as.data.frame(default_payoff),
    prob_df = as.data.frame(t(default_probs)),
    result_df = NULL,
    regret_df = NULL,
    risk_df = NULL,
    evpi = NULL,
    is_benefit = TRUE,
    prob_valid = FALSE
  )
  
  # 初始化时确保概率表列名一致
  observe({
    if (!identical(colnames(rv$prob_df), colnames(rv$payoff_df))) {
      n_states <- ncol(rv$payoff_df)
      rv$prob_df <- create_prob_df(n_states)
      colnames(rv$prob_df) <- colnames(rv$payoff_df)
    }
  })
  
  observeEvent(input$generate, {
    rv$payoff_df <- create_matrix_df(input$n_decisions, input$n_states)
    rv$prob_df <- create_prob_df(input$n_states)
    colnames(rv$prob_df) <- colnames(rv$payoff_df)
    rv$result_df <- NULL
    rv$regret_df <- NULL
    rv$risk_df <- NULL
    rv$evpi <- NULL
  })
  
  # 恢复默认案例
  observeEvent(input$problem_type, {
    # 切换问题时重置为默认矩阵（可选）
    # rv$payoff_df <- as.data.frame(default_payoff)
    # rv$prob_df <- as.data.frame(t(default_probs))
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
  
  output$prob_hot <- renderRHandsontable({
    req(rv$prob_df)
    
    rhandsontable(
      rv$prob_df,
      rowHeaders = rownames(rv$prob_df),
      stretchH = "all",
      height = 120,
      contextMenu = TRUE
    ) %>%
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
          td.style.verticalAlign = 'middle';
        }
      ") %>%
      hot_col(col = colnames(rv$prob_df), type = "numeric", format = "0.00")
  })
  
  observeEvent(input$calculate, {
    tbl <- hot_to_r(input$payoff_hot)
    prob_tbl <- hot_to_r(input$prob_hot)
    
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
    
    is_benefit <- input$problem_type == "benefit"
    rv$is_benefit <- is_benefit
    
    # 概率表处理
    probs <- NULL
    valid_probs <- FALSE
    prob_msg <- ""
    if (!is.null(prob_tbl)) {
      prob_tbl <- as.data.frame(prob_tbl)
      for (j in seq_along(prob_tbl)) {
        prob_tbl[[j]] <- suppressWarnings(as.numeric(prob_tbl[[j]]))
      }
      rv$prob_df <- prob_tbl
      probs <- as.numeric(prob_tbl[1, ])
      
      if (any(is.na(probs))) {
        prob_msg <- "状态概率存在缺失值，本次忽略概率进行风险型计算。"
      } else if (any(probs < 0)) {
        showNotification("状态概率不能为负，已自动截断为 0。", type = "warning")
        probs <- pmax(probs, 0)
        prob_msg <- "状态概率存在负值，已截断为 0。"
      } else {
        prob_sum <- sum(probs)
        if (prob_sum == 0) {
          prob_msg <- "状态概率之和为 0，本次忽略概率进行风险型计算。"
        } else if (abs(prob_sum - 1) > 1e-6) {
          showNotification(sprintf("状态概率之和为 %.4f，已自动归一化。", prob_sum), type = "warning")
          probs <- probs / prob_sum
          valid_probs <- TRUE
        } else {
          valid_probs <- TRUE
        }
      }
      
      if (!valid_probs && prob_msg != "") {
        showNotification(prob_msg, type = "warning")
      }
    }
    rv$prob_valid <- valid_probs
    
    # 不确定型准则
    if (is_benefit) {
      maximax <- apply(payoff, 1, max)
      maximin <- apply(payoff, 1, min)
      laplace <- rowMeans(payoff)
      
      best_each_state <- apply(payoff, 2, max)
      regret <- sweep(
        matrix(best_each_state, nrow = nrow(payoff), ncol = ncol(payoff), byrow = TRUE),
        1:2, payoff, "-"
      )
      
      hurwicz <- input$alpha * apply(payoff, 1, max) + (1 - input$alpha) * apply(payoff, 1, min)
      
      rv$result_df <- data.frame(
        决策方案 = rownames(payoff),
        Maximax_乐观 = round(maximax, 4),
        Maximin_悲观 = round(maximin, 4),
        Laplace_等概率 = round(laplace, 4),
        MinimaxRegret_最小最大后悔 = round(apply(regret, 1, max), 4),
        Hurwicz_折中 = round(hurwicz, 4),
        check.names = FALSE
      )
    } else {
      # 成本型：乐观=最小成本（Minimin），悲观=最大成本（Minimax）
      minimin <- apply(payoff, 1, min)
      minimax_cost <- apply(payoff, 1, max)
      laplace <- rowMeans(payoff)
      
      best_each_state <- apply(payoff, 2, min)
      regret <- sweep(payoff, 2, best_each_state, "-")
      
      # Hurwicz 成本型：α*min + (1-α)*max，值越小越好
      hurwicz <- input$alpha * apply(payoff, 1, min) + (1 - input$alpha) * apply(payoff, 1, max)
      
      rv$result_df <- data.frame(
        决策方案 = rownames(payoff),
        Minimin_乐观 = round(minimin, 4),
        Minimax_悲观 = round(minimax_cost, 4),
        Laplace_等概率 = round(laplace, 4),
        MinimaxRegret_最小最大后悔 = round(apply(regret, 1, max), 4),
        Hurwicz_折中 = round(hurwicz, 4),
        check.names = FALSE
      )
    }
    
    rownames(regret) <- rownames(payoff)
    colnames(regret) <- colnames(payoff)
    rv$regret_df <- data.frame(
      决策方案 = rownames(regret),
      as.data.frame(round(regret, 4), check.names = FALSE),
      check.names = FALSE
    )
    
    # 风险型决策
    if (valid_probs) {
      expected <- as.vector(payoff %*% probs)
      variance <- apply(payoff, 1, function(x) sum(probs * (x - sum(probs * x))^2))
      sd_value <- sqrt(variance)
      
      rv$risk_df <- data.frame(
        决策方案 = rownames(payoff),
        期望值 = round(expected, 4),
        方差 = round(variance, 4),
        标准差 = round(sd_value, 4),
        check.names = FALSE
      )
      
      # EVPI
      if (is_benefit) {
        evpi <- sum(probs * best_each_state) - max(expected)
      } else {
        evpi <- min(expected) - sum(probs * best_each_state)
      }
      rv$evpi <- max(evpi, 0)
    } else {
      rv$risk_df <- NULL
      rv$evpi <- NULL
    }
  })
  
  output$problem_note <- renderUI({
    if (rv$is_benefit) {
      div(class = "info-box",
          p(strong("当前为收益型问题。"), "各准则目标为收益最大化：Maximax、Maximin、Laplace、Hurwicz 越大越好；Minimax Regret 越小越好。"))
    } else {
      div(class = "info-box",
          p(strong("当前为成本型问题。"), "各准则目标为成本最小化：Minimin（乐观）= 各方案最小成本中的最小值，Minimax（悲观）= 各方案最大成本中的最小值；Laplace、Hurwicz 越小越好；Minimax Regret 越小越好。"))
    }
  })
  
  output$result_table <- renderDT({
    req(rv$result_df)
    datatable(
      rv$result_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = if (rv$is_benefit) "不确定型决策准则结果（收益型）" else "不确定型决策准则结果（成本型）"
    )
  })
  
  output$best_summary <- renderUI({
    req(rv$result_df)
    df <- rv$result_df
    is_benefit <- rv$is_benefit
    
    pick_best <- function(values, maximize) {
      if (maximize) {
        idx <- which(abs(values - max(values)) < 1e-9)
      } else {
        idx <- which(abs(values - min(values)) < 1e-9)
      }
      paste(df$决策方案[idx], collapse = ", ")
    }
    
    if (is_benefit) {
      best_maximax <- pick_best(df$Maximax_乐观, TRUE)
      best_maximin <- pick_best(df$Maximin_悲观, TRUE)
      best_laplace <- pick_best(df$Laplace_等概率, TRUE)
      best_hurwicz <- pick_best(df$Hurwicz_折中, TRUE)
    } else {
      best_maximax <- pick_best(df$Minimin_乐观, FALSE)
      best_maximin <- pick_best(df$Minimax_悲观, FALSE)
      best_laplace <- pick_best(df$Laplace_等概率, FALSE)
      best_hurwicz <- pick_best(df$Hurwicz_折中, FALSE)
    }
    best_minimax_regret <- pick_best(df$MinimaxRegret_最小最大后悔, FALSE)
    
    best_risk <- if (!is.null(rv$risk_df)) {
      if (is_benefit) pick_best(rv$risk_df$期望值, TRUE) else pick_best(rv$risk_df$期望值, FALSE)
    } else {
      "（未输入有效概率）"
    }
    
    maximax_label <- if (is_benefit) "Maximax（乐观准则）" else "Minimin（乐观准则）"
    maximin_label <- if (is_benefit) "Maximin（悲观准则）" else "Minimax（悲观准则）"
    
    div(
      class = "info-box",
      h4("推荐方案汇总"),
      tags$ul(
        tags$li(sprintf("%s推荐：%s", maximax_label, best_maximax)),
        tags$li(sprintf("%s推荐：%s", maximin_label, best_maximin)),
        tags$li(sprintf("Laplace（等概率准则）推荐：%s", best_laplace)),
        tags$li(sprintf("Minimax Regret（最小最大遗憾）推荐：%s", best_minimax_regret)),
        tags$li(sprintf("Hurwicz（折中准则，α=%.2f）推荐：%s", input$alpha, best_hurwicz)),
        tags$li(sprintf("期望准则（风险型）推荐：%s", best_risk))
      ),
      p("提示：不同准则可能推荐不同方案，这反映了决策者风险偏好的差异，并非计算错误。")
    )
  })
  
  output$risk_table <- renderDT({
    req(rv$risk_df)
    caption_txt <- if (rv$is_benefit) "风险型决策指标（期望收益）" else "风险型决策指标（期望成本）"
    datatable(
      rv$risk_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = caption_txt
    )
  })
  
  output$evpi_box <- renderUI({
    req(rv$evpi)
    div(
      class = "formula-box",
      HTML(paste0(
        "<b>完全情报价值 EVPI = </b>", round(rv$evpi, 4), "（万元）<br/>",
        "含义：为获得完全情报所愿支付的最高价格；",
        "若市场调查费用低于该值，则获取补充信息在经济上是有利的。"
      ))
    )
  })
  
  output$regret_note <- renderUI({
    if (rv$is_benefit) {
      div(class = "info-box",
          p("收益型后悔值：regret_ij = max_i x_ij - x_ij，表示选择方案 i 而实际状态 j 发生时，与最优方案之间的收益差距。"))
    } else {
      div(class = "info-box",
          p("成本型后悔值：regret_ij = c_ij - min_i c_ij，表示选择方案 i 而实际状态 j 发生时，与最小成本方案之间的成本差距。"))
    }
  })
  
  output$regret_table <- renderDT({
    req(rv$regret_df)
    datatable(
      rv$regret_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "遗憾矩阵"
    )
  })
  
  output$criteria_plot <- renderPlot({
    req(rv$result_df)
    df <- rv$result_df
    df_long <- tidyr::pivot_longer(
      df,
      cols = -决策方案,
      names_to = "准则",
      values_to = "评价值"
    )
    df_long$准则 <- factor(df_long$准则, levels = names(df)[-1])
    
    y_label <- if (rv$is_benefit) "评价值（万元）" else "评价值（万元）"
    
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
    
    y_label <- if (rv$is_benefit) "收益（万元）" else "成本（万元）"
    
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
