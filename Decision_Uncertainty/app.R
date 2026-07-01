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
    "))
  ),
  
  titlePanel(div(class = "title-main", "不确定型与风险型决策分析教学网页")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《不确定型与风险型决策分析教学网页》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("矩阵维度"),
      numericInput("n_decisions", "决策方案数量", value = 3, min = 1, max = 20),
      numericInput("n_states", "自然状态数量", value = 3, min = 1, max = 20),
      actionButton("generate", "生成/重置收益矩阵", class = "btn-primary"),
      tags$hr(),
      
      h4("不确定型准则参数"),
      sliderInput("alpha", "Hurwicz 乐观系数 α", min = 0, max = 1, value = 0.5, step = 0.01),
      tags$hr(),
      
      h4("风险型决策参数"),
      helpText("在“概率输入”选项卡中可输入各状态概率，计算期望收益、EVPI 等。"),
      tags$hr(),
      
      actionButton("calculate", "计算决策结果", class = "btn-success"),
      tags$hr(),
      
      helpText("输入说明："),
      tags$ul(
        tags$li("先输入矩阵维度，再点击“生成/重置收益矩阵”"),
        tags$li("可像 Excel 一样直接在表格中填写"),
        tags$li("支持复制粘贴、Tab 键切换、方向键移动"),
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
              tags$li("在已知状态概率时，计算风险型决策的期望收益、完全情报价值 EVPI 等；"),
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
            tags$p("该钟表需求量不确切，估计有三种可能：3万、12万、20万只。请建立收益矩阵，并分别用乐观法、悲观法、等概率法、后悔值法决定应采用哪个设计方案。")
          )
        ),
        
        tabPanel(
          "收益矩阵输入",
          br(),
          div(
            class = "info-box",
            h4("收益矩阵"),
            rHandsontableOutput("payoff_hot")
          ),
          div(
            class = "info-box",
            h4("状态概率输入（用于风险型决策）"),
            helpText("若已知各自然状态的概率，请在下表填写；不填或填 0 表示只进行不确定型分析。"),
            rHandsontableOutput("prob_hot")
          )
        ),
        
        tabPanel(
          "决策结果",
          br(),
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
          DTOutput("regret_table")
        ),
        
        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("各方案在不同准则下的评价值"),
            plotOutput("criteria_plot", height = "420px"),
            tags$p(class = "small-note", "注：Minimax Regret 为越小越好，其余准则为越大越好。")
          ),
          div(
            class = "info-box",
            h4("各方案收益分布"),
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
    evpi = NULL
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
      showNotification("请先生成并填写收益矩阵。", type = "error")
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
      showNotification("请先将收益矩阵填写完整。", type = "error")
      return()
    }
    
    # 概率表处理
    probs <- NULL
    valid_probs <- FALSE
    if (!is.null(prob_tbl)) {
      prob_tbl <- as.data.frame(prob_tbl)
      for (j in seq_along(prob_tbl)) {
        prob_tbl[[j]] <- suppressWarnings(as.numeric(prob_tbl[[j]]))
      }
      rv$prob_df <- prob_tbl
      probs <- as.numeric(prob_tbl[1, ])
      if (!any(is.na(probs)) && sum(probs) > 0.999 && sum(probs) < 1.001) {
        valid_probs <- TRUE
        if (abs(sum(probs) - 1) > 1e-6) {
          probs <- probs / sum(probs)
        }
      } else {
        showNotification("状态概率之和必须等于 1，本次忽略概率进行风险型计算。", type = "warning")
      }
    }
    
    # 不确定型准则
    maximax <- apply(payoff, 1, max)
    maximin <- apply(payoff, 1, min)
    laplace <- rowMeans(payoff)
    
    best_each_state <- apply(payoff, 2, max)
    regret <- sweep(
      matrix(best_each_state, nrow = nrow(payoff), ncol = ncol(payoff), byrow = TRUE),
      1:2, payoff, "-"
    )
    rownames(regret) <- rownames(payoff)
    colnames(regret) <- colnames(payoff)
    minimax_regret <- apply(regret, 1, max)
    
    alpha <- input$alpha
    hurwicz <- alpha * apply(payoff, 1, max) + (1 - alpha) * apply(payoff, 1, min)
    
    rv$result_df <- data.frame(
      决策方案 = rownames(payoff),
      Maximax_最大可能收益 = round(maximax, 4),
      Maximin_最大最小收益 = round(maximin, 4),
      Laplace_平均收益 = round(laplace, 4),
      MinimaxRegret_最大遗憾值 = round(minimax_regret, 4),
      Hurwicz = round(hurwicz, 4),
      check.names = FALSE
    )
    
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
        期望收益 = round(expected, 4),
        方差 = round(variance, 4),
        标准差 = round(sd_value, 4),
        check.names = FALSE
      )
      
      # EVPI = 完全情报期望收益 - 最大期望收益
      evpi <- sum(probs * best_each_state) - max(expected)
      rv$evpi <- evpi
    } else {
      rv$risk_df <- NULL
      rv$evpi <- NULL
    }
  })
  
  output$result_table <- renderDT({
    req(rv$result_df)
    datatable(
      rv$result_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "不确定型决策准则结果"
    ) %>%
      formatStyle(
        columns = names(rv$result_df)[-1],
        target = "cell",
        color = styleInterval(0, c("#b42318", "#1f3b5b")),
        fontWeight = styleEqual(c(0), c("normal"))
      )
  })
  
  output$best_summary <- renderUI({
    req(rv$result_df)
    df <- rv$result_df
    
    best_maximax <- df$决策方案[which.max(df$Maximax_最大可能收益)]
    best_maximin <- df$决策方案[which.max(df$Maximin_最大最小收益)]
    best_laplace <- df$决策方案[which.max(df$Laplace_平均收益)]
    best_minimax_regret <- df$决策方案[which.min(df$MinimaxRegret_最大遗憾值)]
    best_hurwicz <- df$决策方案[which.max(df$Hurwicz)]
    
    best_risk <- if (!is.null(rv$risk_df)) {
      rv$risk_df$决策方案[which.max(rv$risk_df$期望收益)]
    } else {
      "（未输入有效概率）"
    }
    
    div(
      class = "info-box",
      h4("推荐方案汇总"),
      tags$ul(
        tags$li(sprintf("Maximax（乐观准则）推荐：%s", best_maximax)),
        tags$li(sprintf("Maximin（悲观准则）推荐：%s", best_maximin)),
        tags$li(sprintf("Laplace（等概率准则）推荐：%s", best_laplace)),
        tags$li(sprintf("Minimax Regret（最小最大遗憾）推荐：%s", best_minimax_regret)),
        tags$li(sprintf("Hurwicz（折中准则，α=%.2f）推荐：%s", input$alpha, best_hurwicz)),
        tags$li(sprintf("期望收益准则（风险型）推荐：%s", best_risk))
      )
    )
  })
  
  output$risk_table <- renderDT({
    req(rv$risk_df)
    datatable(
      rv$risk_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "风险型决策指标"
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
    
    ggplot(df_long, aes(x = 准则, y = 评价值, fill = 决策方案, group = 决策方案)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(评价值, 1)),
                position = position_dodge(width = 0.8),
                vjust = -0.5, size = 3.5) +
      labs(x = NULL, y = "评价值", fill = "方案") +
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
      values_to = "收益"
    )
    
    ggplot(df_long, aes(x = 状态, y = 收益, fill = 方案)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = 收益),
                position = position_dodge(width = 0.8),
                vjust = -0.4, size = 3.5) +
      labs(x = NULL, y = "收益", fill = "方案") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
