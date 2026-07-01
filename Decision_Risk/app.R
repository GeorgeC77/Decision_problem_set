# 依赖包：shiny, DT, ggplot2
# install.packages(c("shiny", "DT", "ggplot2", "tidyr"))

library(shiny)
library(DT)
library(ggplot2)
library(tidyr)

# =========================
# 默认参数（教材第三章习题 2：产品包装改进决策）
# =========================
default_prior <- c(0.6, 0.4)          # 先验概率：销路好、销路差
default_payoff <- matrix(
  c(800, -100,    # 大批量生产销售
    200, -20),    # 小批量生产销售
  nrow = 2, byrow = TRUE
)
rownames(default_payoff) <- c("大批量", "小批量")
colnames(default_payoff) <- c("销路好", "销路差")

# 似然矩阵 P(试销结果 | 真实状态)
# 行：真实状态；列：试销结果
default_likelihood <- matrix(
  c(0.80, 0.20,
    0.05, 0.95),
  nrow = 2, byrow = TRUE
)
rownames(default_likelihood) <- c("销路好", "销路差")
colnames(default_likelihood) <- c("试销好", "试销差")

# 试销结果边际概率（与似然、先验应自洽；教材表 3-25 取 0.8, 0.2）
default_sample_margin <- c(0.8, 0.2)
names(default_sample_margin) <- c("试销好", "试销差")

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
    "))
  ),
  
  titlePanel(div(class = "title-main", "风险型决策分析教学网页：产品包装改进")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《风险型决策分析教学网页：产品包装改进》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("先验概率"),
      numericInput("p_good", "销路好概率", value = default_prior[1], min = 0, max = 1, step = 0.01),
      numericInput("p_bad", "销路差概率", value = default_prior[2], min = 0, max = 1, step = 0.01),
      tags$hr(),
      
      h4("收益矩阵（万元）"),
      numericInput("big_good", "大批量 × 销路好", value = default_payoff[1, 1], step = 10),
      numericInput("big_bad", "大批量 × 销路差", value = default_payoff[1, 2], step = 10),
      numericInput("small_good", "小批量 × 销路好", value = default_payoff[2, 1], step = 10),
      numericInput("small_bad", "小批量 × 销路差", value = default_payoff[2, 2], step = 10),
      tags$hr(),
      
      h4("试销似然 P(试销结果 | 真实状态)"),
      numericInput("lh_good_good", "真实好 → 试销好", value = default_likelihood[1, 1], min = 0, max = 1, step = 0.01),
      numericInput("lh_good_bad", "真实好 → 试销差", value = default_likelihood[1, 2], min = 0, max = 1, step = 0.01),
      numericInput("lh_bad_good", "真实差 → 试销好", value = default_likelihood[2, 1], min = 0, max = 1, step = 0.01),
      numericInput("lh_bad_bad", "真实差 → 试销差", value = default_likelihood[2, 2], min = 0, max = 1, step = 0.01),
      tags$hr(),
      
      actionButton("reset_btn", "恢复教材默认值", class = "btn-warning"),
      helpText("注：当“试销结果边际概率”由先验与似然自动计算时，若与教材表 3-25 不一致，可视为教学演示允许学生自行探索参数。")
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
            tags$p("某食品公司准备改进产品包装。公司面临两种生产方案："),
            tags$ul(
              tags$li("大批量生产销售：若销路好可获利 800 万元；若销路差将损失 100 万元。"),
              tags$li("小批量生产销售：若销路好可获利 200 万元；若销路差仅损失 20 万元。")
            ),
            tags$p("公司决定先小批量试销，根据试销结果再决定是否转为大批量生产。"),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("理解先验概率、收益矩阵与期望收益准则；"),
              tags$li("掌握贝叶斯公式：由似然和先验计算后验概率；"),
              tags$li("理解完全情报价值 EVPI 与样本情报价值 EVSI 的经济含义；"),
              tags$li("体会“先抽样、后决策”的风险型决策流程。")
            )
          )
        ),
        
        tabPanel(
          "先验决策",
          br(),
          uiOutput("prior_metric_cards"),
          br(),
          div(
            class = "info-box",
            h4("先验概率下各方案期望收益"),
            DTOutput("prior_table")
          ),
          div(
            class = "info-box",
            h4("完全情报价值 EVPI"),
            uiOutput("evpi_box")
          )
        ),
        
        tabPanel(
          "贝叶斯后验决策",
          br(),
          div(
            class = "info-box",
            h4("后验概率 P(真实状态 | 试销结果)"),
            DTOutput("posterior_table")
          ),
          div(
            class = "info-box",
            h4("各试销结果下的最优决策"),
            DTOutput("posterior_decision_table")
          ),
          div(
            class = "info-box",
            h4("样本情报价值 EVSI"),
            uiOutput("evsi_box")
          )
        ),
        
        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("先验概率变化对最优方案的影响"),
            plotOutput("prior_sensitivity_plot", height = "360px"),
            tags$p(class = "small-note", "横轴为“销路好”的先验概率；两条曲线分别表示大批量和小批量方案的期望收益。")
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

server <- function(input, output, session) {
  
  observeEvent(input$reset_btn, {
    updateNumericInput(session, "p_good", value = default_prior[1])
    updateNumericInput(session, "p_bad", value = default_prior[2])
    updateNumericInput(session, "big_good", value = default_payoff[1, 1])
    updateNumericInput(session, "big_bad", value = default_payoff[1, 2])
    updateNumericInput(session, "small_good", value = default_payoff[2, 1])
    updateNumericInput(session, "small_bad", value = default_payoff[2, 2])
    updateNumericInput(session, "lh_good_good", value = default_likelihood[1, 1])
    updateNumericInput(session, "lh_good_bad", value = default_likelihood[1, 2])
    updateNumericInput(session, "lh_bad_good", value = default_likelihood[2, 1])
    updateNumericInput(session, "lh_bad_bad", value = default_likelihood[2, 2])
  })
  
  calc <- reactive({
    prior <- c(input$p_good, input$p_bad)
    prior <- prior / sum(prior)  # 归一化
    
    payoff <- matrix(
      c(input$big_good, input$big_bad,
        input$small_good, input$small_bad),
      nrow = 2, byrow = TRUE
    )
    rownames(payoff) <- c("大批量", "小批量")
    colnames(payoff) <- c("销路好", "销路差")
    
    likelihood <- matrix(
      c(input$lh_good_good, input$lh_good_bad,
        input$lh_bad_good, input$lh_bad_bad),
      nrow = 2, byrow = TRUE
    )
    rownames(likelihood) <- c("销路好", "销路差")
    colnames(likelihood) <- c("试销好", "试销差")
    
    # 归一化似然行
    likelihood <- likelihood / rowSums(likelihood)
    
    # 试销结果边际概率
    sample_margin <- as.vector(prior %*% likelihood)
    names(sample_margin) <- colnames(likelihood)
    
    # 后验概率 P(真实状态 | 试销结果)
    posterior <- sweep(likelihood, 2, sample_margin, "/")
    posterior <- apply(posterior, 2, function(x) x / sum(x))  # 列归一化防误差
    rownames(posterior) <- rownames(likelihood)
    colnames(posterior) <- colnames(likelihood)
    
    # 先验期望收益
    prior_expected <- rowSums(payoff * matrix(prior, nrow = 2, ncol = 2, byrow = TRUE))
    best_prior_value <- max(prior_expected)
    best_prior_action <- names(prior_expected)[which.max(prior_expected)]
    
    # EVPI
    perfect_info_value <- sum(prior * apply(payoff, 2, max))
    evpi <- perfect_info_value - best_prior_value
    
    # 后验决策
    posterior_decisions <- list()
    posterior_values <- numeric(ncol(posterior))
    names(posterior_values) <- colnames(posterior)
    
    for (j in seq_len(ncol(posterior))) {
      post_prob <- posterior[, j]
      exp_values <- rowSums(payoff * matrix(post_prob, nrow = 2, ncol = 2, byrow = TRUE))
      best <- names(exp_values)[which.max(exp_values)]
      posterior_decisions[[j]] <- list(
        试销结果 = colnames(posterior)[j],
        后验概率_销路好 = post_prob[1],
        后验概率_销路差 = post_prob[2],
        大批量期望收益 = exp_values["大批量"],
        小批量期望收益 = exp_values["小批量"],
        最优方案 = best,
        最优期望收益 = max(exp_values)
      )
    }
    
    posterior_df <- do.call(rbind, lapply(posterior_decisions, function(x) {
      data.frame(
        试销结果 = x$试销结果,
        后验销路好 = round(x$后验概率_销路好, 4),
        后验销路差 = round(x$后验概率_销路差, 4),
        大批量期望收益 = round(x$大批量期望收益, 2),
        小批量期望收益 = round(x$小批量期望收益, 2),
        最优方案 = x$最优方案,
        最优期望收益 = round(x$最优期望收益, 2),
        stringsAsFactors = FALSE
      )
    }))
    
    # EVSI
    expected_posterior_value <- sum(sample_margin * posterior_df$最优期望收益)
    evsi <- expected_posterior_value - best_prior_value
    
    list(
      prior = prior,
      payoff = payoff,
      likelihood = likelihood,
      sample_margin = sample_margin,
      posterior = posterior,
      prior_expected = prior_expected,
      best_prior_value = best_prior_value,
      best_prior_action = best_prior_action,
      evpi = evpi,
      posterior_df = posterior_df,
      evsi = evsi,
      perfect_info_value = perfect_info_value
    )
  })
  
  output$prior_metric_cards <- renderUI({
    res <- calc()
    fluidRow(
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "先验最优方案"),
          div(class = "metric-value", res$best_prior_action),
          div(class = "metric-note", "按先验概率计算的期望收益最大方案")
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "先验最大期望收益"),
          div(class = "metric-value", paste0(round(res$best_prior_value, 2), " 万元")),
          div(class = "metric-note", "先验决策下的期望收益")
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "EVPI"),
          div(class = "metric-value", paste0(round(res$evpi, 2), " 万元")),
          div(class = "metric-note", "完全情报价值")
        )
      )
    )
  })
  
  output$prior_table <- renderDT({
    res <- calc()
    df <- data.frame(
      方案 = names(res$prior_expected),
      期望收益 = round(res$prior_expected, 2),
      stringsAsFactors = FALSE
    )
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "先验决策表"
    )
  })
  
  output$evpi_box <- renderUI({
    res <- calc()
    div(
      class = "formula-box",
      HTML(paste0(
        "<b>完全情报价值 EVPI</b> = 完全情报期望收益 - 先验最优期望收益<br/>",
        "= ", round(res$perfect_info_value, 2), " - ", round(res$best_prior_value, 2),
        " = <b>", round(res$evpi, 2), " 万元</b><br/><br/>",
        "含义：若获取完全情报的费用低于 EVPI，则获取情报在经济上是值得的。"
      ))
    )
  })
  
  output$posterior_table <- renderDT({
    res <- calc()
    df <- as.data.frame(round(res$posterior, 4))
    df$真实状态 <- rownames(res$posterior)
    df <- df[, c("真实状态", colnames(res$posterior))]
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "贝叶斯后验概率表"
    )
  })
  
  output$posterior_decision_table <- renderDT({
    res <- calc()
    datatable(
      res$posterior_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "基于后验概率的最优决策"
    )
  })
  
  output$evsi_box <- renderUI({
    res <- calc()
    post_val <- sum(res$sample_margin * res$posterior_df$最优期望收益)
    div(
      class = "formula-box",
      HTML(paste0(
        "<b>样本情报价值 EVSI</b> = 后验期望收益 - 先验最优期望收益<br/>",
        "= ", round(post_val, 2), " - ", round(res$best_prior_value, 2),
        " = <b>", round(res$evsi, 2), " 万元</b><br/><br/>",
        "含义：若试销费用低于 EVSI，则进行试销在经济上是值得的。"
      ))
    )
  })
  
  output$prior_sensitivity_plot <- renderPlot({
    res <- calc()
    p_seq <- seq(0, 1, length.out = 101)
    big_exp <- p_seq * res$payoff["大批量", "销路好"] +
      (1 - p_seq) * res$payoff["大批量", "销路差"]
    small_exp <- p_seq * res$payoff["小批量", "销路好"] +
      (1 - p_seq) * res$payoff["小批量", "销路差"]
    
    df <- data.frame(
      p_good = rep(p_seq, 2),
      期望收益 = c(big_exp, small_exp),
      方案 = rep(c("大批量", "小批量"), each = length(p_seq))
    )
    
    ggplot(df, aes(x = p_good, y = 期望收益, color = 方案)) +
      geom_line(linewidth = 1.2) +
      geom_point(aes(x = res$prior[1], y = res$prior_expected["大批量"]),
                 color = "#1b9e77", size = 3) +
      geom_point(aes(x = res$prior[1], y = res$prior_expected["小批量"]),
                 color = "#d95f02", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(x = "销路好先验概率", y = "期望收益（万元）") +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("大批量" = "#1b9e77", "小批量" = "#d95f02"))
  })
  
  output$posterior_plot <- renderPlot({
    res <- calc()
    df <- as.data.frame(res$posterior)
    df$真实状态 <- rownames(res$posterior)
    df_long <- tidyr::pivot_longer(df, cols = -真实状态, names_to = "试销结果", values_to = "概率")
    
    ggplot(df_long, aes(x = 试销结果, y = 概率, fill = 真实状态)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(概率, 3)),
                position = position_dodge(width = 0.8),
                vjust = -0.5, size = 4) +
      labs(x = NULL, y = "后验概率", fill = "真实状态") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
