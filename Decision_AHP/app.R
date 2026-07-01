# 依赖包：shiny, DT
# install.packages(c("shiny", "DT"))

library(shiny)
library(DT)

# =========================
# 核心计算函数
# =========================

ri_table <- c("1" = 0, "2" = 0, "3" = 0.58, "4" = 0.90, "5" = 1.12,
              "6" = 1.24, "7" = 1.32, "8" = 1.41, "9" = 1.45, "10" = 1.49)

# 由上三角构建互反矩阵
build_pairwise_matrix <- function(upper_vec, names = NULL) {
  n <- (1 + sqrt(1 + 8 * length(upper_vec))) / 2
  if (abs(n - round(n)) > 1e-9 || n < 2) {
    stop("上三角元素数量不正确，无法构造方阵。")
  }
  n <- as.integer(round(n))
  A <- diag(n)
  k <- 1
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      val <- upper_vec[k]
      if (is.na(val) || val <= 0) {
        stop("判断矩阵上三角元素必须为正数，请检查输入。")
      }
      A[i, j] <- val
      A[j, i] <- 1 / val
      k <- k + 1
    }
  }
  if (!is.null(names)) {
    rownames(A) <- colnames(A) <- names[seq_len(n)]
  }
  A
}

validate_pairwise_matrix <- function(A, name = "判断矩阵") {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) {
    return(list(ok = FALSE, msg = sprintf("%s 不是方阵（%d 行 × %d 列），请检查输入。", name, nrow(A), ncol(A))))
  }
  if (any(is.na(A))) {
    return(list(ok = FALSE, msg = sprintf("%s 存在缺失值，请检查输入。", name)))
  }
  if (any(A <= 0)) {
    return(list(ok = FALSE, msg = sprintf("%s 所有元素必须为正数，请检查输入。", name)))
  }
  n <- nrow(A)
  # 互反性：A[i,j] * A[j,i] 应接近 1
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (abs(A[i, j] * A[j, i] - 1) > 1e-5) {
        return(list(ok = FALSE, msg = sprintf("%s 不满足互反性：A[%d,%d]=%.4f 与 A[%d,%d]=%.4f 的乘积不等于 1。",
                                              name, i, j, A[i, j], j, i, A[j, i])))
      }
    }
  }
  list(ok = TRUE, msg = "")
}

# 最大特征向量法求权重
calc_ahp_weights <- function(A) {
  A <- as.matrix(A)
  v <- validate_pairwise_matrix(A, name = "判断矩阵")
  if (!v$ok) stop(v$msg)

  n <- nrow(A)
  ev <- eigen(A, symmetric = FALSE)
  idx <- which.max(Re(ev$values))
  lambda_max <- Re(ev$values[idx])
  w <- Re(ev$vectors[, idx])
  w <- abs(w) / sum(abs(w))

  ci <- if (n <= 1) 0 else (lambda_max - n) / (n - 1)
  ri <- ri_table[as.character(n)]
  if (is.na(ri) || ri == 0) {
    cr <- 0
  } else {
    cr <- ci / ri
  }

  list(
    weights = as.vector(w),
    lambda_max = as.vector(lambda_max),
    ci = ci,
    ri = ri,
    cr = cr,
    consistent = cr < 0.1
  )
}

# 层次总排序
calc_ahp_scores <- function(criteria_weight, local_weight_matrix) {
  criteria_weight <- as.vector(criteria_weight)
  local_weight_matrix <- as.matrix(local_weight_matrix)

  if (length(criteria_weight) != ncol(local_weight_matrix)) {
    stop(sprintf("准则权重长度（%d）与方案层局部权重矩阵列数（%d）不一致，请检查输入。",
                 length(criteria_weight), ncol(local_weight_matrix)))
  }
  if (any(is.na(criteria_weight)) || any(is.na(local_weight_matrix))) {
    stop("权重存在缺失值，请检查输入。")
  }
  if (abs(sum(criteria_weight) - 1) > 1e-6) {
    stop(sprintf("准则权重之和为 %.6f，应等于 1。", sum(criteria_weight)))
  }
  if (any(criteria_weight < 0)) stop("准则权重不能为负。")

  scores <- as.vector(local_weight_matrix %*% criteria_weight)
  names(scores) <- rownames(local_weight_matrix)
  scores
}

# =========================
# 教师自测
# =========================

run_ahp_self_tests <- function() {
  tests <- list()

  # 完全一致矩阵
  upper_c <- c(2, 3, 2)
  A_c <- build_pairwise_matrix(upper_c, c("C1", "C2", "C3"))
  w_c <- calc_ahp_weights(A_c)
  expected_w_c <- eigen(A_c)$vectors[, which.max(Re(eigen(A_c)$values))]
  expected_w_c <- abs(Re(expected_w_c)) / sum(abs(Re(expected_w_c)))
  passed_consistent <- w_c$cr < 0.1 && all(abs(w_c$weights - expected_w_c) < 1e-6)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "完全一致矩阵 CR < 0.1",
    实际输出 = paste0("权重=", paste(round(w_c$weights, 4), collapse = ", "),
                    ", CR=", round(w_c$cr, 4)),
    标准答案 = paste0("CR < 0.1，权重≈", paste(round(expected_w_c, 4), collapse = ", ")),
    是否通过 = passed_consistent,
    失败提示 = if (passed_consistent) "" else "完全一致或近似一致矩阵的 CR 应小于 0.1，权重为归一化的主特征向量。"
  )

  # 不一致矩阵
  upper_inc <- c(2, 3, 0.5)
  A_inc <- build_pairwise_matrix(upper_inc, c("C1", "C2", "C3"))
  w_inc <- calc_ahp_weights(A_inc)
  passed_inconsistent <- w_inc$cr > 0.1
  tests[[length(tests) + 1]] <- list(
    测试名称 = "不一致矩阵 CR > 0.1",
    实际输出 = paste0("CR=", round(w_inc$cr, 4)),
    标准答案 = "CR > 0.1",
    是否通过 = passed_inconsistent,
    失败提示 = if (passed_inconsistent) "" else "该矩阵存在明显逻辑矛盾，CR 应大于 0.1。请检查 CR 计算是否误用 CI，或 RI 取值是否正确。"
  )

  # 总排序方向 A > B > C
  criteria_w <- c(0.5, 0.3, 0.2)
  local <- matrix(c(0.6, 0.3, 0.1,
                    0.3, 0.4, 0.3,
                    0.1, 0.3, 0.6),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(c("A", "B", "C"), c("C1", "C2", "C3")))
  scores <- calc_ahp_scores(criteria_w, local)
  rank_order <- names(sort(scores, decreasing = TRUE))
  passed_rank <- identical(rank_order, c("A", "B", "C"))
  tests[[length(tests) + 1]] <- list(
    测试名称 = "总排序方向 A>B>C",
    实际输出 = paste(names(scores), round(scores, 4), sep = ":", collapse = "； "),
    标准答案 = "A > B > C",
    是否通过 = passed_rank,
    失败提示 = if (passed_rank) "" else "总得分 = 方案局部权重矩阵 %*% 准则权重，矩阵方向不要弄反。"
  )

  # 错误输入
  err_cases <- list(
    list(name = "上三角含负数", f = function() build_pairwise_matrix(c(2, -1, 2)),
         hint = "判断矩阵元素必须为正数。"),
    list(name = "上三角元素数量错误", f = function() build_pairwise_matrix(c(2, 3)),
         hint = "元素数量应满足 n(n-1)/2。"),
    list(name = "非方阵", f = function() calc_ahp_weights(matrix(1:6, nrow = 2)),
         hint = "应检查方阵。"),
    list(name = "互反性不满足", f = function() {
      A <- matrix(c(1, 2, 3, 0.4, 1, 2, 1/3, 1/2, 1), nrow = 3, byrow = TRUE)
      calc_ahp_weights(A)
    }, hint = "A[i,j]*A[j,i] 应等于 1。")
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

  do.call(rbind, lapply(tests, as.data.frame, stringsAsFactors = FALSE))
}

# =========================
# 辅助 UI：生成上三角输入
# =========================

generate_pair_inputs <- function(id_prefix, n, labels, default_values = NULL) {
  if (is.null(default_values)) default_values <- rep(1, n * (n - 1) / 2)
  inputs <- list()
  k <- 1
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      input_id <- paste0(id_prefix, "_", i, "_", j)
      inputs[[length(inputs) + 1]] <-
        numericInput(input_id, paste0(labels[i], " / ", labels[j], " 重要性之比"),
                     value = default_values[k], min = 0.01, max = 100, step = 0.1)
      k <- k + 1
    }
  }
  do.call(tagList, inputs)
}

read_pair_vector <- function(id_prefix, n, input) {
  vals <- c()
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      vals <- c(vals, input[[paste0(id_prefix, "_", i, "_", j)]])
    }
  }
  vals
}

# =========================
# 默认数据
# =========================

default_n_criteria <- 3
default_n_alternatives <- 3

default_criteria_labels <- c("功能", "价格", "外观")
default_alt_labels <- c("方案A", "方案B", "方案C")

# 准则层默认上三角：功能:价格=2，功能:外观=3，价格:外观=2
default_criteria_upper <- c(2, 3, 2)
# 方案层局部判断默认（每个准则下 A/B/C 的比较）
default_alt_upper <- list(
  c(2, 3, 2),  # 功能
  c(0.5, 0.33, 0.5),  # 价格：B 比 A 便宜等
  c(3, 2, 0.5)   # 外观
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
      .pass { color: #1b7f3b; font-weight: bold; }
      .fail { color: #b42318; font-weight: bold; }
    "))
  ),

  titlePanel(div(class = "title-main", "层次分析法教学网页：特征向量法与一致性检验")),

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
      h4("维度设置"),
      numericInput("n_criteria", "准则数量", value = default_n_criteria, min = 2, max = 10),
      numericInput("n_alternatives", "方案数量", value = default_n_alternatives, min = 2, max = 10),
      actionButton("generate", "生成输入面板", class = "btn-primary"),
      actionButton("reset_default", "恢复默认值", class = "btn-warning"),
      tags$hr(),
      h4("操作"),
      actionButton("calculate", "计算 AHP 权重与总排序", class = "btn-success"),
      tags$hr(),
      checkboxInput("teacher_mode", "显示教师自测区域", value = FALSE),
      tags$hr(),
      helpText("输入说明："),
      tags$ul(
        tags$li("比值 > 1 表示行因素比列因素更重要；< 1 表示行因素不如列因素重要。"),
        tags$li("判断矩阵应满足正数、互反性（Aij = 1/Aji）。"),
        tags$li("本页面使用最大特征向量法求权重，并用 CR 进行一致性检验；λ_max 为判断矩阵的最大特征值。"),
        tags$li("教材中部分准则下未直接评价某些方案。网页为保证判断矩阵完整，默认采用中性值（重要性相等）占位。这是一种教学处理和建模假设，不代表真实偏好；教师可引导学生讨论是否应采用不完全 AHP 或重新定义方案—准则关系。")
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
            p("层次分析法（AHP）将复杂决策问题分解为目标层、准则层和方案层，通过两两比较构建判断矩阵，再计算各层权重并合成总排序。"),
            tags$details(
              tags$summary("查看计算说明"),
              br(),
              tags$ul(
                tags$li("判断矩阵必须为正互反矩阵：A_{ij} > 0，且 A_{ij} = 1/A_{ji}。"),
                tags$li("本页面使用最大特征向量法（主特征向量）求局部权重；λ_max 为判断矩阵的最大特征值，CI/CR 据此严格计算。"),
                tags$li("一致性指标 CI = (λ_max - n)/(n - 1)；一致性比率 CR = CI/RI。"),
                tags$li("CR < 0.1 认为判断矩阵具有可接受的一致性；CR ≥ 0.1 时建议重新调整判断。"),
                tags$li("总排序得分 = 方案局部权重矩阵（行=方案，列=准则）× 准则权重向量。")
              )
            ),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("掌握正互反矩阵的构造与校验方法；"),
              tags$li("理解最大特征向量法与几何平均法的区别；"),
              tags$li("能够正确计算 CI、CR 并进行一致性判断；"),
              tags$li("掌握层次总排序的矩阵方向，避免总得分计算错误；"),
              tags$li("理解：一致性通过仅说明矩阵内部矛盾较小，不代表偏好本身一定正确。")
            )
          )
        ),

        tabPanel(
          "准则层判断",
          br(),
          div(class = "info-box",
              h4("准则两两比较"),
              helpText("输入准则 i 相对于准则 j 的重要性之比。"),
              uiOutput("criteria_inputs"))
        ),

        tabPanel(
          "方案层判断",
          br(),
          lapply(seq_len(default_n_criteria), function(k) {
            div(class = "info-box",
                h4(paste0("准则 ", k, "：", default_criteria_labels[k], " 下方案两两比较")),
                uiOutput(paste0("alt_inputs_", k)))
          })
        ),

        tabPanel(
          "计算结果",
          br(),
          div(class = "info-box",
              h4("准则层权重与一致性"),
              DTOutput("criteria_weight_table")),
          div(class = "info-box",
              h4("方案层局部权重与一致性"),
              DTOutput("alt_weight_table")),
          div(class = "info-box",
              h4("层次总排序"),
              DTOutput("total_score_table")),
          br(),
          uiOutput("consistency_warning")
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
    n_criteria = default_n_criteria,
    n_alternatives = default_n_alternatives,
    criteria_labels = default_criteria_labels,
    alt_labels = default_alt_labels
  )

  output$criteria_inputs <- renderUI({
    generate_pair_inputs("crit", rv$n_criteria, rv$criteria_labels, default_criteria_upper)
  })

  observe({
    for (k in seq_len(rv$n_criteria)) {
      local({
        kk <- k
        output[[paste0("alt_inputs_", kk)]] <- renderUI({
          vals <- if (kk <= length(default_alt_upper)) default_alt_upper[[kk]] else NULL
          generate_pair_inputs(paste0("alt", kk), rv$n_alternatives, rv$alt_labels, vals)
        })
      })
    }
  })

  observeEvent(input$generate, {
    rv$n_criteria <- input$n_criteria
    rv$n_alternatives <- input$n_alternatives
    rv$criteria_labels <- paste0("准则", seq_len(rv$n_criteria))
    rv$alt_labels <- paste0("方案", seq_len(rv$n_alternatives))
  })

  observeEvent(input$reset_default, {
    rv$n_criteria <- default_n_criteria
    rv$n_alternatives <- default_n_alternatives
    rv$criteria_labels <- default_criteria_labels
    rv$alt_labels <- default_alt_labels
    updateNumericInput(session, "n_criteria", value = default_n_criteria)
    updateNumericInput(session, "n_alternatives", value = default_n_alternatives)
  })

  calc <- eventReactive(input$calculate, {
    # 读取准则层
    crit_upper <- read_pair_vector("crit", rv$n_criteria, input)
    crit_names <- rv$criteria_labels
    tryCatch({
      A_crit <- build_pairwise_matrix(crit_upper, crit_names)
      crit_res <- calc_ahp_weights(A_crit)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      return(NULL)
    })

    # 读取方案层
    alt_local_matrix <- matrix(NA, nrow = rv$n_alternatives, ncol = rv$n_criteria)
    rownames(alt_local_matrix) <- rv$alt_labels
    colnames(alt_local_matrix) <- crit_names
    alt_res_list <- list()
    consistency_issues <- c()

    for (k in seq_len(rv$n_criteria)) {
      alt_upper <- read_pair_vector(paste0("alt", k), rv$n_alternatives, input)
      tryCatch({
        A_alt <- build_pairwise_matrix(alt_upper, rv$alt_labels)
        w_alt <- calc_ahp_weights(A_alt)
        alt_local_matrix[, k] <- w_alt$weights
        alt_res_list[[k]] <- data.frame(
          准则 = crit_names[k],
          最大特征值 = round(w_alt$lambda_max, 4),
          CI = round(w_alt$ci, 4),
          RI = round(w_alt$ri, 4),
          CR = round(w_alt$cr, 4),
          一致性 = if (w_alt$consistent) "通过" else "未通过",
          stringsAsFactors = FALSE
        )
        if (!w_alt$consistent) {
          consistency_issues <- c(consistency_issues, paste0("准则 ", crit_names[k], " 下方案层判断"))
        }
      }, error = function(e) {
        showNotification(paste0("准则 ", crit_names[k], "：", conditionMessage(e)), type = "error")
        return(NULL)
      })
    }

    if (is.null(crit_res) || length(alt_res_list) != rv$n_criteria) {
      return(NULL)
    }

    scores <- calc_ahp_scores(crit_res$weights, alt_local_matrix)
    if (!crit_res$consistent) consistency_issues <- c("准则层判断", consistency_issues)

    list(
      criteria = crit_res,
      alt_local_matrix = alt_local_matrix,
      alt_res = do.call(rbind, alt_res_list),
      scores = scores,
      consistency_issues = consistency_issues
    )
  })

  output$criteria_weight_table <- renderDT({
    req(calc())
    res <- calc()
    df <- data.frame(
      准则 = rv$criteria_labels,
      权重 = round(res$criteria$weights, 4),
      最大特征值 = round(res$criteria$lambda_max, 4),
      CI = round(res$criteria$ci, 4),
      RI = round(res$criteria$ri, 4),
      CR = round(res$criteria$cr, 4),
      一致性 = if (res$criteria$consistent) "通过" else "未通过",
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$alt_weight_table <- renderDT({
    req(calc())
    res <- calc()
    datatable(res$alt_res, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$total_score_table <- renderDT({
    req(calc())
    res <- calc()
    df <- data.frame(
      方案 = names(res$scores),
      总得分 = round(res$scores, 4),
      排序 = rank(-res$scores, ties.method = "min"),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$排序), ]
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = "层次总排序结果（总得分 = 方案局部权重矩阵 × 准则权重）")
  })

  output$consistency_warning <- renderUI({
    req(calc())
    res <- calc()
    if (length(res$consistency_issues) > 0) {
      div(class = "warning-box",
          HTML(paste0(
            "<b>一致性警告：</b>以下判断矩阵的 CR ≥ 0.1，建议重新调整两两比较：<br/>",
            paste(res$consistency_issues, collapse = "、"),
            "<br/>CR ≥ 0.1 并不意味着结果完全不可用，但说明判断逻辑存在一定程度的不一致，教学中应引导学生检查判断。",
            "<br/>一致性通过仅说明矩阵内部矛盾较小，不代表偏好本身一定正确。"
          )))
    } else {
      div(class = "info-box",
          p("所有判断矩阵的 CR 均小于 0.1，单层一致性检验通过。"),
          p("一致性通过仅说明矩阵内部矛盾较小，不代表偏好本身一定正确。"))
    }
  })

  self_test_res <- eventReactive(input$run_self_test, {
    run_ahp_self_tests()
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
