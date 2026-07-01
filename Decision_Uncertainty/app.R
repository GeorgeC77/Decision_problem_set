# 依赖包：shiny, DT, rhandsontable, ggplot2
# install.packages(c("shiny", "DT", "rhandsontable", "ggplot2"))

library(shiny)
library(DT)
library(rhandsontable)
library(ggplot2)

# =========================
# 核心计算函数
# =========================

calc_uncertainty_decision <- function(payoff_matrix, alpha = 0.5, is_benefit = TRUE) {
  payoff_matrix <- as.matrix(payoff_matrix)

  if (any(is.na(payoff_matrix))) {
    stop("收益/成本矩阵存在缺失值，请检查输入。")
  }
  if (is.na(alpha) || length(alpha) != 1) {
    stop("乐观系数 alpha 必须是一个 0~1 之间的单一数值。")
  }
  if (alpha < 0 || alpha > 1) {
    stop("乐观系数 alpha 必须在 0 与 1 之间。alpha=1 表示完全乐观，alpha=0 表示完全悲观。")
  }

  n_a <- nrow(payoff_matrix)
  action_names <- rownames(payoff_matrix)
  if (is.null(action_names)) action_names <- paste0("方案", seq_len(n_a))
  state_names <- colnames(payoff_matrix)
  if (is.null(state_names)) state_names <- paste0("状态", seq_len(ncol(payoff_matrix)))

  row_max <- apply(payoff_matrix, 1, max)
  row_min <- apply(payoff_matrix, 1, min)
  laplace <- rowMeans(payoff_matrix)

  if (is_benefit) {
    maximax_val <- max(row_max)
    maximax_idx <- which(abs(row_max - maximax_val) < 1e-9)

    maximin_val <- max(row_min)
    maximin_idx <- which(abs(row_min - maximin_val) < 1e-9)

    hurwicz <- alpha * row_max + (1 - alpha) * row_min
    hurwicz_val <- max(hurwicz)
    hurwicz_idx <- which(abs(hurwicz - hurwicz_val) < 1e-9)

    regret <- sweep(
      matrix(apply(payoff_matrix, 2, max), nrow = n_a, ncol = ncol(payoff_matrix), byrow = TRUE),
      1:2, payoff_matrix, "-"
    )
    row_max_regret <- apply(regret, 1, max)
    savage_val <- min(row_max_regret)
    savage_idx <- which(abs(row_max_regret - savage_val) < 1e-9)

    laplace_val <- max(laplace)
    laplace_idx <- which(abs(laplace - laplace_val) < 1e-9)

    summary <- data.frame(
      决策准则 = c("Maximax（乐观）", "Maximin（悲观）", "Laplace（等可能）",
                  "Hurwicz（折中）", "Savage（最小最大后悔值）"),
      指标值 = c(maximax_val, maximin_val, laplace_val, hurwicz_val, savage_val),
      推荐方案 = c(
        paste(action_names[maximax_idx], collapse = ", "),
        paste(action_names[maximin_idx], collapse = ", "),
        paste(action_names[laplace_idx], collapse = ", "),
        paste(action_names[hurwicz_idx], collapse = ", "),
        paste(action_names[savage_idx], collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
  } else {
    minimin_val <- min(row_min)
    minimin_idx <- which(abs(row_min - minimin_val) < 1e-9)

    minimax_cost_val <- min(row_max)
    minimax_cost_idx <- which(abs(row_max - minimax_cost_val) < 1e-9)

    laplace_val <- min(laplace)
    laplace_idx <- which(abs(laplace - laplace_val) < 1e-9)

    hurwicz <- alpha * row_min + (1 - alpha) * row_max
    hurwicz_val <- min(hurwicz)
    hurwicz_idx <- which(abs(hurwicz - hurwicz_val) < 1e-9)

    regret <- sweep(payoff_matrix, 2, apply(payoff_matrix, 2, min), "-")
    row_max_regret <- apply(regret, 1, max)
    savage_val <- min(row_max_regret)
    savage_idx <- which(abs(row_max_regret - savage_val) < 1e-9)

    summary <- data.frame(
      决策准则 = c("Minimin（乐观）", "Minimax（悲观）", "Laplace（等可能）",
                  "Hurwicz（折中）", "Savage（最小最大后悔值）"),
      指标值 = c(minimin_val, minimax_cost_val, laplace_val, hurwicz_val, savage_val),
      推荐方案 = c(
        paste(action_names[minimin_idx], collapse = ", "),
        paste(action_names[minimax_cost_idx], collapse = ", "),
        paste(action_names[laplace_idx], collapse = ", "),
        paste(action_names[hurwicz_idx], collapse = ", "),
        paste(action_names[savage_idx], collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
  }

  list(
    payoff = payoff_matrix,
    alpha = alpha,
    is_benefit = is_benefit,
    action_names = action_names,
    state_names = state_names,
    maximax = if (is_benefit) row_max else NULL,
    minimin = if (!is_benefit) row_min else NULL,
    maximin = if (is_benefit) row_min else NULL,
    minimax_cost = if (!is_benefit) row_max else NULL,
    laplace = laplace,
    hurwicz = hurwicz,
    regret = regret,
    row_max_regret = row_max_regret,
    summary = summary
  )
}

# =========================
# 教师自测
# =========================

run_uncertainty_self_tests <- function() {
  tests <- list()

  payoff_b <- matrix(c(800, 400, -200, 600, 300, 100, 200, 150, 300),
                     nrow = 3, byrow = TRUE,
                     dimnames = list(c("大批量", "中批量", "小批量"),
                                     c("畅销", "一般", "滞销")))
  alpha <- 0.7
  res_b <- calc_uncertainty_decision(payoff_b, alpha, TRUE)

  # 收益型手工核对
  row_max <- apply(payoff_b, 1, max)
  row_min <- apply(payoff_b, 1, min)
  laplace_b <- rowMeans(payoff_b)
  hurwicz_b <- alpha * row_max + (1 - alpha) * row_min
  regret_b <- sweep(matrix(apply(payoff_b, 2, max), nrow = 3, ncol = 3, byrow = TRUE), 1:2, payoff_b, "-")
  max_regret_b <- apply(regret_b, 1, max)

  passed_b <- all(abs(res_b$maximax - row_max) < 1e-6) &&
    all(abs(res_b$maximin - row_min) < 1e-6) &&
    all(abs(res_b$laplace - laplace_b) < 1e-6) &&
    all(abs(res_b$hurwicz - hurwicz_b) < 1e-6) &&
    all(abs(res_b$row_max_regret - max_regret_b) < 1e-6) &&
    res_b$summary$推荐方案[res_b$summary$决策准则 == "Maximax（乐观）"] == "大批量"

  tests[[length(tests) + 1]] <- list(
    测试名称 = "收益型准则方向",
    实际输出 = paste(res_b$summary$决策准则, res_b$summary$推荐方案, sep = ":", collapse = "； "),
    标准答案 = "Maximax应选大批量，Maximin应选小批量，Savage应选中批量",
    是否通过 = passed_b,
    失败提示 = if (passed_b) "" else "收益型 Maximax 取行最大最大，Maximin 取行最小最大，Savage 取最小最大后悔值。"
  )

  # Hurwicz alpha=1 vs alpha=0
  res_opt <- calc_uncertainty_decision(payoff_b, 1, TRUE)
  res_pes <- calc_uncertainty_decision(payoff_b, 0, TRUE)
  passed_hurwicz <- res_opt$summary$推荐方案[res_opt$summary$决策准则 == "Hurwicz（折中）"] == "大批量" &&
    res_pes$summary$推荐方案[res_pes$summary$决策准则 == "Hurwicz（折中）"] == "小批量"
  tests[[length(tests) + 1]] <- list(
    测试名称 = "Hurwicz 乐观系数边界",
    实际输出 = paste0("alpha=1: ", res_opt$summary$推荐方案[res_opt$summary$决策准则 == "Hurwicz（折中）"],
                    "； alpha=0: ", res_pes$summary$推荐方案[res_pes$summary$决策准则 == "Hurwicz（折中）"]),
    标准答案 = "alpha=1: 大批量； alpha=0: 小批量",
    是否通过 = passed_hurwicz,
    失败提示 = if (passed_hurwicz) "" else "Hurwicz 中 alpha=1 应退化为乐观准则，alpha=0 应退化为悲观准则。"
  )

  # 成本型
  payoff_c <- -payoff_b
  res_c <- calc_uncertainty_decision(payoff_c, alpha, FALSE)
  row_min_c <- apply(payoff_c, 1, min)
  row_max_c <- apply(payoff_c, 1, max)
  laplace_c <- rowMeans(payoff_c)
  hurwicz_c <- alpha * row_min_c + (1 - alpha) * row_max_c
  regret_c <- sweep(payoff_c, 2, apply(payoff_c, 2, min), "-")
  max_regret_c <- apply(regret_c, 1, max)

  passed_c <- all(abs(res_c$minimin - row_min_c) < 1e-6) &&
    all(abs(res_c$minimax_cost - row_max_c) < 1e-6) &&
    all(abs(res_c$laplace - laplace_c) < 1e-6) &&
    all(abs(res_c$hurwicz - hurwicz_c) < 1e-6) &&
    all(abs(res_c$row_max_regret - max_regret_c) < 1e-6)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "成本型准则方向",
    实际输出 = paste(res_c$summary$决策准则, res_c$summary$推荐方案, sep = ":", collapse = "； "),
    标准答案 = "成本型 Minimin 取行最小最小，Minimax 取行最大最小",
    是否通过 = passed_c,
    失败提示 = if (passed_c) "" else "成本型 Minimin 取行最小最小，Minimax（成本）取行最大最小，不要与收益型混淆。"
  )

  # Savage regret 非负且行和？这里不检查行和
  tests[[length(tests) + 1]] <- list(
    测试名称 = "Savage 后悔值非负",
    实际输出 = paste0("收益型最小后悔值=", min(res_b$row_max_regret),
                    "；成本型最小后悔值=", min(res_c$row_max_regret)),
    标准答案 = "均 >= 0",
    是否通过 = all(res_b$regret >= -1e-9) && all(res_c$regret >= -1e-9),
    失败提示 = if (all(res_b$regret >= -1e-9) && all(res_c$regret >= -1e-9)) "" else "后悔值不应为负，收益型后悔值=列最大-元素，成本型后悔值=元素-列最小。"
  )

  # 并列最优
  tie_mat <- matrix(c(10, 10, 10, 10), nrow = 2, byrow = TRUE,
                    dimnames = list(c("A", "B"), c("s1", "s2")))
  res_tie <- calc_uncertainty_decision(tie_mat, 0.5, TRUE)
  tie_ok <- grepl("A", res_tie$summary$推荐方案[1]) && grepl("B", res_tie$summary$推荐方案[1])
  tests[[length(tests) + 1]] <- list(
    测试名称 = "并列最优显示",
    实际输出 = res_tie$summary$推荐方案[1],
    标准答案 = "A, B",
    是否通过 = tie_ok,
    失败提示 = if (tie_ok) "" else "并列最优时应同时显示所有推荐方案。"
  )

  # 错误输入
  err_cases <- list(
    list(name = "alpha 超出 [0,1]", f = function() calc_uncertainty_decision(payoff_b, 1.2, TRUE),
         hint = "alpha 应在 0~1 之间，越界时报错。"),
    list(name = "payoff 含 NA", f = function() calc_uncertainty_decision(matrix(c(10, NA, 5, 6), nrow = 2, byrow = TRUE), 0.5, TRUE),
         hint = "应检测到缺失值并报错。")
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
# 默认数据
# =========================
default_action_names <- c("大批量", "中批量", "小批量")
default_state_names <- c("畅销", "一般", "滞销")
default_payoff <- matrix(
  c(800, 400, -200,
    600, 300, 100,
    200, 150, 300),
  nrow = 3, byrow = TRUE,
  dimnames = list(default_action_names, default_state_names)
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
      .small-note { color: #667085; font-size: 13px; }
      .pass { color: #1b7f3b; font-weight: bold; }
      .fail { color: #b42318; font-weight: bold; }
    "))
  ),

  titlePanel(div(class = "title-main", "不确定型决策分析教学网页：乐观、悲观与后悔值")),

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
                   choices = c("收益型" = "benefit", "成本型" = "cost"),
                   selected = "benefit"),
      helpText("收益型追求收益越大越好；成本型追求成本越低越好。后悔值方向会随之改变。"),

      tags$hr(),
      h4("矩阵维度"),
      numericInput("n_actions", "方案数量", value = 3, min = 2, max = 20),
      numericInput("n_states", "自然状态数量", value = 3, min = 2, max = 20),
      actionButton("generate", "生成/重置收益/成本矩阵", class = "btn-primary"),
      actionButton("reset_default", "恢复教材默认值", class = "btn-warning"),

      tags$hr(),
      h4("Hurwicz 乐观系数"),
      sliderInput("alpha", "alpha（0=完全悲观，1=完全乐观）",
                  min = 0, max = 1, value = 0.7, step = 0.05),
      helpText("alpha 越大越乐观。收益型 Hurwicz = alpha×最大收益 + (1-alpha)×最小收益；成本型 Hurwicz = alpha×最小成本 + (1-alpha)×最大成本。"),

      tags$hr(),
      h4("操作"),
      actionButton("calculate", "计算决策结果", class = "btn-success"),
      tags$hr(),

      checkboxInput("teacher_mode", "显示教师自测区域", value = FALSE),

      tags$hr(),
      helpText("输入说明："),
      tags$ul(
        tags$li("收益/成本矩阵：行=方案，列=自然状态，不需要输入概率。"),
        tags$li("可直接在表格中编辑。"),
        tags$li("所有准则同时计算，方便比较不同决策者的风险偏好。")
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
            p("不确定型决策是指决策者不知道各自然状态出现的概率，只能依靠方案在不同状态下的结果选择。常用的准则有："),
            tags$ul(
              tags$li(tags$b("Maximax（乐观）/ Minimin（成本型乐观）"), "：假设出现最好状态，选该状态下结果最好的方案。"),
              tags$li(tags$b("Maximin（收益型悲观）/ Minimax（成本型悲观）"), "：假设出现最差状态，选该状态下结果最好的方案（或成本最小）。"),
              tags$li(tags$b("Laplace（等可能）"), "：假设各状态概率相等，选择期望指标最优的方案。"),
              tags$li(tags$b("Hurwicz（折中）"), "：用乐观系数 alpha 对最好与最坏结果加权。"),
              tags$li(tags$b("Savage（最小最大后悔值）"), "：构造后悔值矩阵，选择最大后悔值最小的方案。")
            ),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("区分收益型与成本型各准则的优化方向，避免方向错误；"),
              tags$li("理解 Hurwicz 乐观系数 alpha 的含义与边界行为；"),
              tags$li("正确构造后悔值矩阵并应用 Savage 准则；"),
              tags$li("识别并列最优，避免只返回单个方案造成误导。"),
              tags$li("注意：若某个方案在所有自然状态下都严格优于其他方案，则各准则很可能给出相同推荐，这是由数据本身的严格优势导致的，而非准则失效。")
            )
          )
        ),

        tabPanel(
          "输入数据",
          br(),
          div(
            class = "info-box",
            h4("收益/成本矩阵（万元）"),
            helpText("行=方案，列=自然状态。收益型填收益，成本型填成本。"),
            rHandsontableOutput("payoff_hot")
          )
        ),

        tabPanel(
          "计算结果",
          br(),
          div(
            class = "info-box",
            h4("各准则决策结果"),
            helpText("Minimax Regret 始终越小越好；除后悔值准则外，收益型评价值越大越好，成本型评价值越小越好。"),
            DTOutput("summary_table")
          ),
          div(
            class = "info-box",
            h4("后悔值矩阵"),
            helpText("收益型 regret_{ij} = max_i(x_{ij}) - x_{ij}；成本型 regret_{ij} = c_{ij} - min_i(c_{ij})。Savage 准则选择最大后悔值最小的方案，因此最大后悔值越小越好。"),
            DTOutput("regret_table")
          ),
          br(),
          uiOutput("explain_box")
        ),

        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("各方案行最大/最小值对比"),
            plotOutput("range_plot", height = "360px"),
            tags$p(class = "small-note", "左端点为各方案最差结果，右端点为最好结果。红色竖线表示当前 alpha 下 Hurwicz 折中值。")
          )
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
    payoff_df = as.data.frame(default_payoff),
    action_names = default_action_names,
    state_names = default_state_names
  )

  observe({
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
  })

  observeEvent(input$generate, {
    n_a <- input$n_actions
    n_s <- input$n_states
    rv$action_names <- paste0("方案", seq_len(n_a))
    rv$state_names <- paste0("状态", seq_len(n_s))
    rv$payoff_df <- as.data.frame(matrix(0, nrow = n_a, ncol = n_s))
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
  })

  observeEvent(input$reset_default, {
    rv$action_names <- default_action_names
    rv$state_names <- default_state_names
    rv$payoff_df <- as.data.frame(default_payoff)
    colnames(rv$payoff_df) <- rv$state_names
    rownames(rv$payoff_df) <- rv$action_names
    updateRadioButtons(session, "problem_type", selected = "benefit")
    updateNumericInput(session, "n_actions", value = 3)
    updateNumericInput(session, "n_states", value = 3)
    updateSliderInput(session, "alpha", value = 0.7)
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
      hot_table(manualColumnResize = TRUE) %>%
      hot_cols(type = "numeric", format = "0") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }
      ")
  })

  calc <- eventReactive(input$calculate, {
    tbl <- hot_to_r(input$payoff_hot)
    if (is.null(tbl)) {
      showNotification("请先生成并填写矩阵。", type = "error")
      return(NULL)
    }
    payoff <- suppressWarnings(as.matrix(tbl))
    rownames(payoff) <- rv$action_names
    colnames(payoff) <- rv$state_names
    is_benefit <- input$problem_type == "benefit"
    alpha <- input$alpha
    tryCatch({
      calc_uncertainty_decision(payoff, alpha, is_benefit)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
  })

  output$summary_table <- renderDT({
    req(calc())
    datatable(calc()$summary, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = if (calc()$is_benefit) "收益型各准则决策结果" else "成本型各准则决策结果") %>%
      formatRound(columns = "指标值", digits = 4)
  })

  output$regret_table <- renderDT({
    req(calc())
    res <- calc()
    df <- as.data.frame(round(res$regret, 4))
    df$方案 <- res$action_names
    df$最大后悔值 <- round(res$row_max_regret, 4)
    df <- df[, c("方案", res$state_names, "最大后悔值")]
    datatable(df, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = if (res$is_benefit) "收益型后悔值矩阵" else "成本型后悔值矩阵")
  })

  output$explain_box <- renderUI({
    req(calc())
    res <- calc()
    if (res$is_benefit) {
      txt <- sprintf(
        "当前为<b>收益型</b>问题，Hurwicz 乐观系数 alpha=%.2f。%s 收益型问题中，alpha=1 退化为 Maximax，alpha=0 退化为 Maximin；Savage 准则选择最大后悔值最小的方案。",
        res$alpha,
        if (res$alpha == 1) "alpha=1 表示完全乐观，" else if (res$alpha == 0) "alpha=0 表示完全悲观，" else ""
      )
    } else {
      txt <- sprintf(
        "当前为<b>成本型</b>问题，Hurwicz 乐观系数 alpha=%.2f。%s 成本型问题中，alpha=1 退化为 Minimin（乐观），alpha=0 退化为 Minimax（悲观）；Savage 准则选择最大后悔值最小的方案。",
        res$alpha,
        if (res$alpha == 1) "alpha=1 表示完全乐观，" else if (res$alpha == 0) "alpha=0 表示完全悲观，" else ""
      )
    }
    div(class = "info-box", h4("决策解释"), p(HTML(txt)),
        p("若某准则出现多个并列最优方案，推荐栏会同时列出，避免遗漏。"))
  })

  output$range_plot <- renderPlot({
    req(calc())
    res <- calc()
    df <- data.frame(
      方案 = rep(res$action_names, each = 2),
      端点 = rep(c("最差", "最好"), length(res$action_names)),
      值 = c(rbind(if (res$is_benefit) res$maximin else res$minimax_cost,
                   if (res$is_benefit) res$maximax else res$minimin))
    )
    hurwicz_df <- data.frame(方案 = res$action_names, Hurwicz = res$hurwicz)
    p <- ggplot(df, aes(x = 值, y = 方案, group = 方案)) +
      geom_line(linewidth = 1.2, color = "#5b7083") +
      geom_point(aes(color = 端点), size = 3) +
      geom_point(data = hurwicz_df, aes(x = Hurwicz, y = 方案),
                 shape = 23, fill = "#b42318", color = "#b42318", size = 4) +
      scale_color_manual(values = c("最差" = "#b42318", "最好" = "#1b7f3b")) +
      labs(title = "各方案最好-最差结果范围", x = "金额（万元）", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    p
  })

  self_test_res <- eventReactive(input$run_self_test, {
    run_uncertainty_self_tests()
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
