# 依赖包：shiny, DT, rhandsontable, ggplot2
# install.packages(c("shiny", "DT", "rhandsontable", "ggplot2", "tidyr"))

library(shiny)
library(DT)
library(rhandsontable)
library(ggplot2)
library(tidyr)

# =========================
# 博弈论辅助函数
# =========================

# 寻找纯策略纳什均衡（返回玩家1和玩家2的收益）
find_pure_nash <- function(A, B) {
  n <- nrow(A)
  m <- ncol(A)
  
  best_response_p2 <- matrix(FALSE, n, m)  # 给定列，哪些行是玩家1的最优反应
  for (j in seq_len(m)) {
    best_val <- max(A[, j])
    best_response_p2[, j] <- A[, j] == best_val
  }
  
  best_response_p1 <- matrix(FALSE, n, m)  # 给定行，哪些列是玩家2的最优反应
  for (i in seq_len(n)) {
    best_val <- max(B[i, ])
    best_response_p1[i, ] <- B[i, ] == best_val
  }
  
  nash_idx <- which(best_response_p2 & best_response_p1, arr.ind = TRUE)
  if (nrow(nash_idx) == 0) return(NULL)
  
  data.frame(
    行策略 = rownames(A)[nash_idx[, 1]],
    列策略 = colnames(A)[nash_idx[, 2]],
    玩家1收益 = A[nash_idx],
    玩家2收益 = B[nash_idx],
    stringsAsFactors = FALSE
  )
}

# 2x2 混合策略纳什均衡
solve_mixed_2x2 <- function(A, B) {
  # A: 玩家1收益矩阵（玩家1选行，玩家2选列）
  # B: 玩家2收益矩阵
  a11 <- A[1, 1]; a12 <- A[1, 2]
  a21 <- A[2, 1]; a22 <- A[2, 2]
  b11 <- B[1, 1]; b12 <- B[1, 2]
  b21 <- B[2, 1]; b22 <- B[2, 2]
  
  denom_q <- a11 - a12 - a21 + a22
  denom_p <- b11 - b12 - b21 + b22
  
  eq <- list()
  
  if (abs(denom_q) > 1e-10) {
    q <- (a22 - a12) / denom_q
    if (q >= 0 && q <= 1) {
      eq$p1_mix <- c(q, 1 - q)
    }
  }
  if (abs(denom_p) > 1e-10) {
    p <- (b22 - b21) / denom_p
    if (p >= 0 && p <= 1) {
      eq$p2_mix <- c(p, 1 - p)
    }
  }
  
  if (length(eq) == 2) {
    exp1 <- eq$p1_mix[1] * (eq$p2_mix[1] * a11 + eq$p2_mix[2] * a12) +
            eq$p1_mix[2] * (eq$p2_mix[1] * a21 + eq$p2_mix[2] * a22)
    exp2 <- eq$p1_mix[1] * (eq$p2_mix[1] * b11 + eq$p2_mix[2] * b12) +
            eq$p1_mix[2] * (eq$p2_mix[1] * b21 + eq$p2_mix[2] * b22)
    eq$expected_payoff <- c(exp1, exp2)
  }
  
  eq
}

# 教材第七章习题 4 默认双矩阵（声明博弈）
# 玩家1（声明方）有三种类型 θ1, θ2, θ3；玩家2（行为方）有三种行为 α1, α2, α3
# 每个数组 (a, b)：a 为声明方收益，b 为行为方收益
default_A <- matrix(
  c(0, 1, 0,
    0, 1, 0,
    0, 1, 2),
  nrow = 3, byrow = TRUE
)
default_B <- matrix(
  c(1, 0, 0,
    0, 2, 0,
    0, 0, 1),
  nrow = 3, byrow = TRUE
)
rownames(default_A) <- rownames(default_B) <- c("θ1", "θ2", "θ3")
colnames(default_A) <- colnames(default_B) <- c("α1", "α2", "α3")

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
      }
      .metric-title { color: #5b7083; font-size: 13px; margin-bottom: 8px; }
      .metric-value { font-size: 22px; font-weight: 700; color: #1f3b5b; }
      .formula-box {
        background: #f8fafc; border: 1px solid #d9e2ec;
        border-radius: 10px; padding: 14px 16px; line-height: 1.8;
      }
    "))
  ),
  
  titlePanel(div(class = "title-main", "竞争型决策分析教学网页：纳什均衡求解器")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《竞争型决策分析教学网页：纳什均衡求解器》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("矩阵维度"),
      numericInput("n_rows", "玩家 1 策略数", value = 3, min = 2, max = 5),
      numericInput("n_cols", "玩家 2 策略数", value = 3, min = 2, max = 5),
      actionButton("generate", "生成/重置矩阵", class = "btn-primary"),
      tags$hr(),
      actionButton("solve", "求解纳什均衡", class = "btn-success"),
      tags$hr(),
      helpText("说明："),
      tags$ul(
        tags$li("每个单元格为 (玩家1收益, 玩家2收益)。"),
        tags$li("2×2 博弈会自动计算混合策略均衡。"),
        tags$li("默认数据为教材第七章习题 4 的声明博弈。")
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
            tags$p("本章对应教材第七章“竞争型决策分析——博弈论”。通过本网页，学生可以："),
            tags$ul(
              tags$li("理解纳什均衡的概念：双方都在给定对方策略时选择最优反应；"),
              tags$li("掌握纯策略纳什均衡的判定方法；"),
              tags$li("对 2×2 博弈，掌握混合策略纳什均衡的计算；"),
              tags$li("通过修改收益矩阵，观察均衡如何变化。")
            ),
            tags$hr(),
            h4("默认案例"),
            tags$p("默认载入教材第七章习题 4 的声明博弈：声明方有三种类型 θ1, θ2, θ3，行为方有三种行为 α1, α2, α3。")
          )
        ),
        
        tabPanel(
          "收益矩阵输入",
          br(),
          div(
            class = "info-box",
            h4("玩家 1 收益矩阵 A"),
            rHandsontableOutput("A_hot")
          ),
          div(
            class = "info-box",
            h4("玩家 2 收益矩阵 B"),
            rHandsontableOutput("B_hot")
          )
        ),
        
        tabPanel(
          "均衡结果",
          br(),
          div(
            class = "info-box",
            h4("纯策略纳什均衡"),
            DTOutput("pure_nash_table")
          ),
          div(
            class = "info-box",
            h4("2×2 混合策略纳什均衡"),
            uiOutput("mixed_nash_box")
          )
        ),
        
        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("玩家 1 收益热力图"),
            plotOutput("A_heatmap", height = "360px")
          ),
          div(
            class = "info-box",
            h4("玩家 2 收益热力图"),
            plotOutput("B_heatmap", height = "360px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    A = default_A,
    B = default_B,
    pure_nash = NULL,
    mixed_nash = NULL
  )
  
  make_empty_mat <- function(n, m, prefix = "策略") {
    mat <- matrix(0, n, m)
    rownames(mat) <- paste0("R", seq_len(n))
    colnames(mat) <- paste0("C", seq_len(m))
    mat
  }
  
  observeEvent(input$generate, {
    rv$A <- make_empty_mat(input$n_rows, input$n_cols, "行")
    rv$B <- make_empty_mat(input$n_rows, input$n_cols, "列")
    rv$pure_nash <- NULL
    rv$mixed_nash <- NULL
  })
  
  output$A_hot <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$A),
      rowHeaders = rownames(rv$A),
      stretchH = "all",
      height = 320
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0")
  })
  
  output$B_hot <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$B),
      rowHeaders = rownames(rv$B),
      stretchH = "all",
      height = 320
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0")
  })
  
  observeEvent(input$solve, {
    A_tbl <- hot_to_r(input$A_hot)
    B_tbl <- hot_to_r(input$B_hot)
    
    A <- as.matrix(A_tbl)
    B <- as.matrix(B_tbl)
    mode(A) <- "numeric"
    mode(B) <- "numeric"
    rownames(A) <- rownames(B) <- rownames(A_tbl)
    colnames(A) <- colnames(B) <- colnames(A_tbl)
    
    rv$A <- A
    rv$B <- B
    
    rv$pure_nash <- find_pure_nash(A, B)
    
    if (nrow(A) == 2 && ncol(A) == 2) {
      rv$mixed_nash <- solve_mixed_2x2(A, B)
    } else {
      rv$mixed_nash <- NULL
    }
  })
  
  output$pure_nash_table <- renderDT({
    if (is.null(rv$pure_nash)) {
      df <- data.frame(结果 = "未找到纯策略纳什均衡", stringsAsFactors = FALSE)
    } else {
      df <- rv$pure_nash
    }
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  output$mixed_nash_box <- renderUI({
    if (nrow(rv$A) != 2 || ncol(rv$A) != 2) {
      return(div(class = "formula-box", "混合策略均衡仅对 2×2 博弈自动计算。"))
    }
    if (is.null(rv$mixed_nash) || length(rv$mixed_nash) < 3) {
      return(div(class = "formula-box", "未找到有效的 2×2 混合策略均衡，或请检查收益矩阵。"))
    }
    eq <- rv$mixed_nash
    rnames <- rownames(rv$A)
    cnames <- colnames(rv$A)
    
    div(
      class = "formula-box",
      HTML(paste0(
        "<b>玩家 1 的混合策略：</b><br/>",
        rnames[1], " 概率 = ", round(eq$p1_mix[1], 4), "；",
        rnames[2], " 概率 = ", round(eq$p1_mix[2], 4), "<br/><br/>",
        "<b>玩家 2 的混合策略：</b><br/>",
        cnames[1], " 概率 = ", round(eq$p2_mix[1], 4), "；",
        cnames[2], " 概率 = ", round(eq$p2_mix[2], 4), "<br/><br/>",
        "<b>期望收益：</b>玩家 1 = ", round(eq$expected_payoff[1], 4),
        "，玩家 2 = ", round(eq$expected_payoff[2], 4)
      ))
    )
  })
  
  output$A_heatmap <- renderPlot({
    req(rv$A)
    df <- as.data.frame(rv$A)
    df$玩家1策略 <- rownames(rv$A)
    df_long <- tidyr::pivot_longer(df, cols = -玩家1策略, names_to = "玩家2策略", values_to = "收益")
    
    ggplot(df_long, aes(x = 玩家2策略, y = 玩家1策略, fill = 收益)) +
      geom_tile(color = "white") +
      geom_text(aes(label = 收益), color = "black", size = 5) +
      scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850") +
      labs(title = "玩家 1 收益", x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })
  
  output$B_heatmap <- renderPlot({
    req(rv$B)
    df <- as.data.frame(rv$B)
    df$玩家1策略 <- rownames(rv$B)
    df_long <- tidyr::pivot_longer(df, cols = -玩家1策略, names_to = "玩家2策略", values_to = "收益")
    
    ggplot(df_long, aes(x = 玩家2策略, y = 玩家1策略, fill = 收益)) +
      geom_tile(color = "white") +
      geom_text(aes(label = 收益), color = "black", size = 5) +
      scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850") +
      labs(title = "玩家 2 收益", x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)
