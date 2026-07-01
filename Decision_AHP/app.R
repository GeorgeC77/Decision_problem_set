# 依赖包：shiny, DT, rhandsontable, ggplot2
# install.packages(c("shiny", "DT", "rhandsontable", "ggplot2"))

library(shiny)
library(DT)
library(rhandsontable)
library(ggplot2)

# =========================
# AHP 辅助函数
# =========================

# 1-9 标度的随机一致性指标 RI
ri_table <- c(
  "1" = 0, "2" = 0, "3" = 0.58, "4" = 0.90, "5" = 1.12,
  "6" = 1.24, "7" = 1.32, "8" = 1.41, "9" = 1.45
)

# 计算判断矩阵权重、λmax、CI、CR
ahp_weights <- function(mat) {
  n <- nrow(mat)
  if (n != ncol(mat) || any(is.na(mat))) {
    return(list(weights = rep(NA, n), lambda = NA, ci = NA, cr = NA))
  }
  
  # 几何平均法求权重
  gm <- apply(mat, 1, function(x) prod(x)^(1 / length(x)))
  w <- gm / sum(gm)
  
  # 计算 λmax
  aw <- mat %*% w
  lambda <- mean(aw / w)
  ci <- (lambda - n) / (n - 1)
  ri <- ri_table[as.character(n)]
  cr <- if (is.na(ri) || ri == 0) 0 else ci / ri
  
  list(weights = w, lambda = lambda, ci = ci, cr = cr)
}

# 根据判断矩阵上三角自动生成完整矩阵（含倒数）
fill_pairwise_matrix <- function(upper) {
  n <- nrow(upper)
  mat <- matrix(1, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i < j) {
        v <- suppressWarnings(as.numeric(upper[i, j]))
        if (is.na(v)) v <- 1
        mat[i, j] <- v
        mat[j, i] <- 1 / v
      }
    }
  }
  mat
}

# 教材第五章习题 2 默认数据
criteria_names <- c("调动积极性 C1", "提高企业水平 C2", "改善福利 C3")
alt_names <- c("发奖金 a1", "集体福利 a2", "办职校 a3", "建俱乐部 a4", "引进设备 a5")

# 准则层判断矩阵（完整）
default_criteria_mat <- matrix(
  c(1, 1/4, 1/5,
    4, 1, 2,
    5, 1/2, 1),
  nrow = 3, byrow = TRUE
)
rownames(default_criteria_mat) <- criteria_names
colnames(default_criteria_mat) <- criteria_names

# 三个准则下的方案判断矩阵（完整方阵，教材中 C2 不含 a1、C3 不含 a5，用中性值 1 占位，
# 用户可在实际教学中根据题意手动将无关方案对应行/列调整为 1）
default_alt_mats <- list(
  "调动积极性 C1" = matrix(
    c(1, 3, 4, 5, 6,
      1/3, 1, 2, 4, 3,
      1/4, 1/2, 1, 3, 2,
      1/5, 1/4, 1/3, 1, 3,
      1/6, 1/3, 1/2, 1/3, 1),
    nrow = 5, byrow = TRUE
  ),
  "提高企业水平 C2" = matrix(
    c(1, 1, 1, 1, 1,
      1, 1, 1/5, 1/4, 1/6,
      1, 5, 1, 4, 2,
      1, 4, 1/4, 1, 1/2,
      1, 6, 1/2, 2, 1),
    nrow = 5, byrow = TRUE
  ),
  "改善福利 C3" = matrix(
    c(1, 3, 2, 3, 1,
      1/3, 1, 2, 2, 1,
      1/2, 1/2, 1, 1, 1,
      1/3, 1/2, 1, 1, 1,
      1, 1, 1, 1, 1),
    nrow = 5, byrow = TRUE
  )
)
for (nm in names(default_alt_mats)) {
  rownames(default_alt_mats[[nm]]) <- alt_names
  colnames(default_alt_mats[[nm]]) <- alt_names
}

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
      .small-note { color: #667085; font-size: 13px; }
    "))
  ),
  
  titlePanel(div(class = "title-main", "层次分析法教学网页：判断矩阵、权重计算与一致性检验")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《层次分析法教学网页：判断矩阵、权重计算与一致性检验》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("操作"),
      actionButton("reset_btn", "恢复教材默认值", class = "btn-warning"),
      actionButton("calculate", "计算 AHP 结果", class = "btn-success"),
      tags$hr(),
      h4("使用说明"),
      tags$ul(
        tags$li("本网页权重采用几何平均近似法（即判断矩阵各行几何平均值归一化）计算。"),
        tags$li("左侧输入判断矩阵；上三角数值改变后，下三角会自动补为倒数。"),
        tags$li("一致性比率 CR < 0.1 时认为判断矩阵具有满意一致性；CR ≥ 0.1 时结果仅供参考，建议调整判断矩阵。"),
        tags$li("1–9 标度含义：1 表示同等重要，3 表示稍重要，5 表示明显重要，7 表示强烈重要，9 表示极端重要；2、4、6、8 为相邻标度中间值。"),
        tags$li("教材中 C2 不含 a1、C3 不含 a5，网页默认用中性值占位，教师可引导学生讨论其合理性。")
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
            tags$p("某企业需要合理使用利润 G，考虑三个准则：调动积极性 C1、提高企业水平 C2、改善福利 C3。"),
            tags$p("备选方案包括：发奖金 a1、集体福利 a2、办职校 a3、建俱乐部 a4、引进设备 a5。"),
            tags$p("本网页用 AHP 层次分析法计算各准则权重和各方案相对排序。"),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("理解 1-9 标度与判断矩阵的构造；"),
              tags$li("掌握几何平均近似法求权重；"),
              tags$li("掌握一致性检验：CI、RI、CR 的计算与判断；"),
              tags$li("掌握层次总排序及其一致性检验。")
            )
          )
        ),
        
        tabPanel(
          "准则层判断矩阵",
          br(),
          div(
            class = "info-box",
            h4("目标 G → 准则 C1、C2、C3"),
            rHandsontableOutput("criteria_hot")
          )
        ),
        
        tabPanel(
          "方案层判断矩阵",
          br(),
          tabsetPanel(
            tabPanel("C1 调动积极性", br(), rHandsontableOutput("alt_hot_C1")),
            tabPanel("C2 提高企业水平", br(), rHandsontableOutput("alt_hot_C2")),
            tabPanel("C3 改善福利", br(), rHandsontableOutput("alt_hot_C3"))
          )
        ),
        
        tabPanel(
          "计算结果",
          br(),
          uiOutput("cr_warning"),
          br(),
          uiOutput("cr_cards"),
          br(),
          div(
            class = "info-box",
            h4("各判断矩阵一致性检验汇总"),
            DTOutput("cr_detail_table")
          ),
          br(),
          div(
            class = "info-box",
            h4("准则层权重"),
            DTOutput("criteria_result_table")
          ),
          div(
            class = "info-box",
            h4("方案层局部权重"),
            DTOutput("alt_weight_table")
          ),
          div(
            class = "info-box",
            h4("层次总排序"),
            DTOutput("total_rank_table")
          ),
          div(
            class = "info-box",
            h4("总排序一致性检验"),
            uiOutput("total_cr_box")
          )
        ),
        
        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("层次总排序柱状图"),
            plotOutput("rank_plot", height = "400px")
          ),
          div(
            class = "info-box",
            h4("准则层权重饼图"),
            plotOutput("criteria_pie", height = "360px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    criteria_mat = default_criteria_mat,
    alt_mats = default_alt_mats,
    criteria_res = NULL,
    alt_res = NULL,
    total_rank = NULL,
    total_cr = NULL
  )
  
  observeEvent(input$reset_btn, {
    rv$criteria_mat <- default_criteria_mat
    rv$alt_mats <- default_alt_mats
    rv$criteria_res <- NULL
    rv$alt_res <- NULL
    rv$total_rank <- NULL
    rv$total_cr <- NULL
  })
  
  output$criteria_hot <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$criteria_mat),
      rowHeaders = rownames(rv$criteria_mat),
      stretchH = "all",
      height = 280
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0000")
  })
  
  output$alt_hot_C1 <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$alt_mats[["调动积极性 C1"]]),
      rowHeaders = rownames(rv$alt_mats[["调动积极性 C1"]]),
      stretchH = "all",
      height = 360
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0000")
  })
  
  output$alt_hot_C2 <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$alt_mats[["提高企业水平 C2"]]),
      rowHeaders = rownames(rv$alt_mats[["提高企业水平 C2"]]),
      stretchH = "all",
      height = 360
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0000")
  })
  
  output$alt_hot_C3 <- renderRHandsontable({
    rhandsontable(
      as.data.frame(rv$alt_mats[["改善福利 C3"]]),
      rowHeaders = rownames(rv$alt_mats[["改善福利 C3"]]),
      stretchH = "all",
      height = 360
    ) %>%
      hot_table(manualColumnResize = TRUE, manualRowResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0000")
  })
  
  observeEvent(input$calculate, {
    # 读取准则矩阵
    crit_tbl <- hot_to_r(input$criteria_hot)
    crit_upper <- as.matrix(crit_tbl)
    crit_mat <- fill_pairwise_matrix(crit_upper)
    rownames(crit_mat) <- rownames(crit_upper)
    colnames(crit_mat) <- colnames(crit_upper)
    rv$criteria_mat <- crit_mat
    rv$criteria_res <- ahp_weights(crit_mat)
    
    # 读取方案矩阵
    alt_names_map <- criteria_names
    alt_tbls <- list(
      "调动积极性 C1" = hot_to_r(input$alt_hot_C1),
      "提高企业水平 C2" = hot_to_r(input$alt_hot_C2),
      "改善福利 C3" = hot_to_r(input$alt_hot_C3)
    )
    
    rv$alt_res <- lapply(names(alt_tbls), function(nm) {
      upper <- as.matrix(alt_tbls[[nm]])
      mat <- fill_pairwise_matrix(upper)
      rownames(mat) <- rownames(upper)
      colnames(mat) <- colnames(upper)
      rv$alt_mats[[nm]] <- mat
      res <- ahp_weights(mat)
      list(name = nm, res = res, mat = mat)
    })
    names(rv$alt_res) <- names(alt_tbls)
    
    # 层次总排序
    criteria_w <- rv$criteria_res$weights
    alt_names <- rownames(rv$alt_mats[[1]])
    n_alt <- length(alt_names)
    alt_weight_matrix <- sapply(rv$alt_res, function(x) x$res$weights[1:n_alt])
    if (is.null(dim(alt_weight_matrix))) {
      alt_weight_matrix <- matrix(alt_weight_matrix, nrow = n_alt)
    }
    total_scores <- as.vector(alt_weight_matrix %*% criteria_w)
    names(total_scores) <- alt_names
    
    rv$total_rank <- data.frame(
      方案 = alt_names,
      总排序权重 = round(total_scores, 4),
      排序 = rank(-total_scores, ties.method = "min"),
      stringsAsFactors = FALSE
    )
    rv$total_rank <- rv$total_rank[order(rv$total_rank$排序), ]
    
    # 总排序一致性
    cis <- sapply(rv$alt_res, function(x) x$res$ci)
    ris <- sapply(rv$alt_res, function(x) {
      n <- nrow(x$mat)
      ri_table[as.character(n)]
    })
    total_ci <- sum(criteria_w * cis)
    total_ri <- sum(criteria_w * ris)
    total_cr <- if (total_ri == 0) 0 else total_ci / total_ri
    rv$total_cr <- list(ci = total_ci, ri = total_ri, cr = total_cr)
  })
  
  output$cr_warning <- renderUI({
    req(rv$criteria_res)
    cr <- rv$criteria_res$cr
    alt_crs <- sapply(rv$alt_res, function(x) x$res$cr)
    any_fail <- is.na(cr) || cr >= 0.1 || any(!is.na(alt_crs) & alt_crs >= 0.1)
    if (any_fail) {
      div(
        class = "warning-box",
        HTML("<b>一致性检验未全部通过：</b>部分判断矩阵的 CR ≥ 0.10。建议返回检查并调整相应矩阵。未通过一致性检验时，AHP 排序结果仅供参考。")
      )
    } else {
      div(class = "info-box", p("所有判断矩阵一致性检验通过（CR < 0.10），结果可信。"))
    }
  })
  
  output$cr_cards <- renderUI({
    req(rv$criteria_res)
    cr <- rv$criteria_res$cr
    pass <- if (is.na(cr)) FALSE else cr < 0.1
    fluidRow(
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "准则层 λmax"),
          div(class = "metric-value", round(rv$criteria_res$lambda, 4))
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "准则层 CI"),
          div(class = "metric-value", round(rv$criteria_res$ci, 4))
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-title", "准则层 CR"),
          div(class = "metric-value", style = if (pass) "color:#1b7f3b" else "color:#b42318",
              ifelse(is.na(cr), "—", round(cr, 4))),
          div(class = "metric-note", if (pass) "通过一致性检验" else "未通过一致性检验")
        )
      )
    )
  })
  
  output$criteria_result_table <- renderDT({
    req(rv$criteria_res)
    df <- data.frame(
      准则 = criteria_names,
      权重 = round(rv$criteria_res$weights, 4),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  output$alt_weight_table <- renderDT({
    req(rv$alt_res)
    df <- data.frame(
      方案 = rownames(rv$alt_mats[[1]]),
      stringsAsFactors = FALSE
    )
    for (nm in names(rv$alt_res)) {
      df[[nm]] <- round(rv$alt_res[[nm]]$res$weights, 4)
    }
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  output$total_rank_table <- renderDT({
    req(rv$total_rank)
    datatable(
      rv$total_rank,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "层次总排序"
    )
  })
  
  output$cr_detail_table <- renderDT({
    req(rv$criteria_res, rv$alt_res)
    rows <- data.frame(
      矩阵 = "准则层",
      λmax = round(rv$criteria_res$lambda, 4),
      CI = round(rv$criteria_res$ci, 4),
      CR = round(rv$criteria_res$cr, 4),
      是否通过 = ifelse(rv$criteria_res$cr < 0.1, "是", "否"),
      stringsAsFactors = FALSE
    )
    for (nm in names(rv$alt_res)) {
      r <- rv$alt_res[[nm]]$res
      rows <- rbind(rows, data.frame(
        矩阵 = nm,
        λmax = round(r$lambda, 4),
        CI = round(r$ci, 4),
        CR = round(r$cr, 4),
        是否通过 = ifelse(r$cr < 0.1, "是", "否"),
        stringsAsFactors = FALSE
      ))
    }
    datatable(rows, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = "各判断矩阵一致性检验（CR < 0.10 为通过）")
  })
  
  output$total_cr_box <- renderUI({
    req(rv$total_cr)
    pass <- rv$total_cr$cr < 0.1
    div(
      class = "formula-box",
      HTML(paste0(
        "层次总排序公式：W_total = V · W_criteria，其中 V 的每一列为某准则下各方案局部权重，W_criteria 为准则层权重向量。<br/><br/>",
        "CI_total = Σ w_k · CI_k = ", round(rv$total_cr$ci, 4), "<br/>",
        "RI_total = Σ w_k · RI_k = ", round(rv$total_cr$ri, 4), "<br/>",
        "CR_total = CI_total / RI_total = ", round(rv$total_cr$cr, 4),
        ifelse(pass, " < 0.1，通过总排序一致性检验。", " ≥ 0.1，未通过总排序一致性检验，建议调整判断矩阵。")
      ))
    )
  })
  
  output$rank_plot <- renderPlot({
    req(rv$total_rank)
    df <- rv$total_rank
    df$方案 <- factor(df$方案, levels = df$方案[order(df$总排序权重)])
    
    ggplot(df, aes(x = 方案, y = 总排序权重, fill = 排序 == 1)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = round(总排序权重, 4)), hjust = -0.2, size = 4) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#4c78a8")) +
      labs(x = NULL, y = "总排序权重", fill = "是否最优") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$criteria_pie <- renderPlot({
    req(rv$criteria_res)
    df <- data.frame(
      准则 = criteria_names,
      权重 = rv$criteria_res$weights
    )
    ggplot(df, aes(x = "", y = 权重, fill = 准则)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y") +
      geom_text(aes(label = paste0(准则, "\n", round(权重, 3))),
                position = position_stack(vjust = 0.5), size = 4) +
      theme_void(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
