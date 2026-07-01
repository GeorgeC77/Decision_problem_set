# 依赖包：shiny, DT, ggplot2, rhandsontable, tidyr
# install.packages(c("shiny", "DT", "ggplot2", "rhandsontable", "tidyr"))

library(shiny)
library(DT)
library(ggplot2)
library(rhandsontable)
library(tidyr)

# =========================
# 核心计算函数
# =========================

# 风险型决策核心计算（不含贝叶斯后验/EVSI）
calc_risk_decision <- function(payoff_matrix, prob, type = c("benefit", "cost")) {
  type <- match.arg(type)
  payoff_matrix <- as.matrix(payoff_matrix)
  prob <- as.numeric(prob)

  if (any(is.na(payoff_matrix))) {
    stop("收益/成本矩阵存在缺失值，请检查输入。")
  }
  if (any(is.na(prob))) {
    stop("概率存在缺失值，请检查输入。")
  }
  if (length(prob) != ncol(payoff_matrix)) {
    stop(sprintf("概率长度（%d）与矩阵列数（%d）不一致，请检查输入。", length(prob), ncol(payoff_matrix)))
  }
  if (any(prob < 0)) {
    stop("概率存在负值，概率必须非负。")
  }
  if (abs(sum(prob) - 1) > 1e-8) {
    stop(sprintf("概率之和为 %.8f，与 1 的偏差超过 1e-8，请修改输入。", sum(prob)))
  }
  if (any(payoff_matrix < 0) && type == "benefit") {
    # 收益型允许负收益，不报错；成本型允许正成本，也不报错
  }

  prob_mat <- matrix(prob, nrow = nrow(payoff_matrix), ncol = ncol(payoff_matrix), byrow = TRUE)
  expected <- as.vector(rowSums(payoff_matrix * prob_mat))

  if (type == "benefit") {
    best_val <- max(expected)
    best_idx <- which(abs(expected - best_val) < 1e-9)
    evwpi <- sum(prob * apply(payoff_matrix, 2, max))
    evpi <- evwpi - best_val
    regret <- sweep(
      matrix(apply(payoff_matrix, 2, max), nrow = nrow(payoff_matrix), ncol = ncol(payoff_matrix), byrow = TRUE),
      1:2, payoff_matrix, "-"
    )
    label <- "EMV"
  } else {
    best_val <- min(expected)
    best_idx <- which(abs(expected - best_val) < 1e-9)
    ecwpi <- sum(prob * apply(payoff_matrix, 2, min))
    evpi <- best_val - ecwpi
    regret <- sweep(payoff_matrix, 2, apply(payoff_matrix, 2, min), "-")
    label <- "EC"
  }

  if (evpi < -1e-8) {
    stop("EVPI 计算为负值，请检查收益/成本类型或矩阵方向是否设置正确。")
  }
  evpi <- max(evpi, 0)

  eol <- as.vector(rowSums(regret * prob_mat))
  if (abs(min(eol) - evpi) > 1e-6) {
    stop(sprintf("机会损失矩阵或 EVPI 计算方向可能错误：min(EOL)=%.6f，EVPI=%.6f。", min(eol), evpi))
  }

  list(
    payoff_matrix = payoff_matrix,
    prob = prob,
    type = type,
    expected = expected,
    best_val = best_val,
    best_idx = best_idx,
    evwpi = if (type == "benefit") evwpi else NULL,
    ecwpi = if (type == "cost") ecwpi else NULL,
    evpi = evpi,
    regret = regret,
    eol = eol,
    label = label
  )
}

# 贝叶斯后验与 EVSI 计算
calc_bayesian_evsi <- function(payoff_matrix, prior, likelihood, type = c("benefit", "cost")) {
  type <- match.arg(type)
  n_k <- ncol(likelihood)
  signal_names <- colnames(likelihood)
  action_names <- rownames(payoff_matrix)
  state_names <- colnames(payoff_matrix)

  sample_margin <- as.vector(prior %*% likelihood)
  names(sample_margin) <- signal_names

  posterior <- matrix(NA_real_, nrow = length(state_names), ncol = n_k)
  rownames(posterior) <- state_names
  colnames(posterior) <- signal_names

  for (j in seq_len(n_k)) {
    if (sample_margin[j] > 1e-12) {
      posterior[, j] <- (likelihood[, j] * prior) / sample_margin[j]
      # 归一化，防止数值误差
      s <- sum(posterior[, j])
      if (s > 0) posterior[, j] <- posterior[, j] / s
    }
  }

  post_decisions <- lapply(seq_len(n_k), function(j) {
    if (sample_margin[j] <= 1e-12) {
      return(data.frame(
        信号 = signal_names[j],
        边际概率 = round(sample_margin[j], 4),
        最优方案 = "该信号发生概率为 0，无法计算后验概率",
        最优期望指标 = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    post_prob <- posterior[, j]
    exp_post <- payoff_matrix %*% post_prob
    if (type == "benefit") {
      best_post_val <- max(exp_post)
      best_post_idx <- which(abs(exp_post - best_post_val) < 1e-9)
    } else {
      best_post_val <- min(exp_post)
      best_post_idx <- which(abs(exp_post - best_post_val) < 1e-9)
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

  # 仅边际概率大于 0 的信号参与期望后验指标计算
  valid_idx <- sample_margin > 1e-12
  expected_posterior_value <- sum(sample_margin[valid_idx] * post_df$最优期望指标[valid_idx], na.rm = TRUE)
  prior_res <- calc_risk_decision(payoff_matrix, prior, type)
  if (type == "benefit") {
    evsi <- expected_posterior_value - prior_res$best_val
  } else {
    evsi <- prior_res$best_val - expected_posterior_value
  }
  if (evsi < -1e-8) {
    stop("EVSI 计算为负值，请检查输入数据。")
  }
  evsi <- max(evsi, 0)
  # EVSI 理论上不超过 EVPI
  evpi <- prior_res$evpi
  if (evsi > evpi + 1e-8) {
    evsi <- evpi
  }

  list(posterior = posterior, post_df = post_df, evsi = evsi,
       expected_posterior_value = expected_posterior_value)
}

# =========================
# 教师自测
# =========================

run_risk_self_tests <- function() {
  tests <- list()

  payoff_b <- matrix(c(800, -100, 200, -20), nrow = 2, byrow = TRUE,
                     dimnames = list(c("大批量", "小批量"), c("销路好", "销路差")))
  prob <- c(0.6, 0.4)

  # 收益型测试
  res_b <- calc_risk_decision(payoff_b, prob, "benefit")
  expected_emv <- c(0.6 * 800 + 0.4 * (-100), 0.6 * 200 + 0.4 * (-20))
  expected_evpi <- sum(prob * apply(payoff_b, 2, max)) - max(expected_emv)
  tests[[length(tests) + 1]] <- list(
    测试名称 = "收益型 EMV 与 EVPI",
    实际输出 = paste0("EMV=", paste(round(res_b$expected, 4), collapse = ", "),
                    ", 最优=", paste(rownames(payoff_b)[res_b$best_idx], collapse = ", "),
                    ", EVPI=", round(res_b$evpi, 4)),
    标准答案 = paste0("EMV=", paste(round(expected_emv, 4), collapse = ", "),
                    ", 最优=大批量, EVPI=", round(expected_evpi, 4)),
    是否通过 = all(abs(res_b$expected - expected_emv) < 1e-6) &&
      res_b$best_idx == 1 && abs(res_b$evpi - expected_evpi) < 1e-6,
    失败提示 = if (all(abs(res_b$expected - expected_emv) < 1e-6) && res_b$best_idx == 1 && abs(res_b$evpi - expected_evpi) < 1e-6) "" else "收益型应计算 EMV 并选最大，EVPI = EVwPI - max(EMV)。"
  )

  # EOL = EVPI 自检
  tests[[length(tests) + 1]] <- list(
    测试名称 = "收益型 min(EOL) = EVPI",
    实际输出 = paste0("min(EOL)=", round(min(res_b$eol), 6), ", EVPI=", round(res_b$evpi, 6)),
    标准答案 = "二者相等",
    是否通过 = abs(min(res_b$eol) - res_b$evpi) < 1e-6,
    失败提示 = if (abs(min(res_b$eol) - res_b$evpi) < 1e-6) "" else "后悔值应按列最大收益计算，或 EVPI 方向错误。"
  )

  # 成本型测试
  payoff_c <- -payoff_b
  res_c <- calc_risk_decision(payoff_c, prob, "cost")
  expected_ec <- c(0.6 * (-800) + 0.4 * 100, 0.6 * (-200) + 0.4 * 20)
  expected_evpi_c <- min(expected_ec) - sum(prob * apply(payoff_c, 2, min))
  tests[[length(tests) + 1]] <- list(
    测试名称 = "成本型 EC 与 EVPI",
    实际输出 = paste0("EC=", paste(round(res_c$expected, 4), collapse = ", "),
                    ", 最优=", paste(rownames(payoff_c)[res_c$best_idx], collapse = ", "),
                    ", EVPI=", round(res_c$evpi, 4)),
    标准答案 = paste0("EC=", paste(round(expected_ec, 4), collapse = ", "),
                    ", 最优=大批量, EVPI=", round(expected_evpi_c, 4)),
    是否通过 = all(abs(res_c$expected - expected_ec) < 1e-6) &&
      res_c$best_idx == 1 && abs(res_c$evpi - expected_evpi_c) < 1e-6,
    失败提示 = if (all(abs(res_c$expected - expected_ec) < 1e-6) && res_c$best_idx == 1 && abs(res_c$evpi - expected_evpi_c) < 1e-6) "" else "成本型应计算 EC 并选最小，EVPI = min(EC) - ECwPI。"
  )

  # 并列最优
  payoff_tie <- matrix(c(100, 100, 100, 100), nrow = 2, byrow = TRUE,
                       dimnames = list(c("A", "B"), c("s1", "s2")))
  res_tie <- calc_risk_decision(payoff_tie, c(0.5, 0.5), "benefit")
  tests[[length(tests) + 1]] <- list(
    测试名称 = "并列最优",
    实际输出 = paste(rownames(payoff_tie)[res_tie$best_idx], collapse = ", "),
    标准答案 = "A, B",
    是否通过 = length(res_tie$best_idx) == 2,
    失败提示 = if (length(res_tie$best_idx) == 2) "" else "并列最优时应返回所有最优方案，不要只取 which.max 的第一个。"
  )

  # 错误输入测试
  err_cases <- list(
    list(name = "概率和 = 0.9", f = function() calc_risk_decision(payoff_b, c(0.6, 0.3), "benefit"), hint = "概率之和必须严格等于 1。"),
    list(name = "概率存在负数", f = function() calc_risk_decision(payoff_b, c(0.6, -0.2), "benefit"), hint = "概率必须非负。"),
    list(name = "payoff 含 NA", f = function() calc_risk_decision(matrix(c(800, NA, 200, -20), nrow = 2, byrow = TRUE), prob, "benefit"), hint = "应检测到缺失值并报错。"),
    list(name = "矩阵列数与概率长度不一致", f = function() calc_risk_decision(payoff_b, c(0.5, 0.3, 0.2), "benefit"), hint = "应检查维度一致性。")
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
# 默认数据（教材第三章习题 2：产品包装改进）
# =========================
default_action_names <- c("大批量", "小批量")
default_state_names <- c("销路好", "销路差")
default_signal_names <- c("试销好", "试销差")

default_payoff <- matrix(
  c(800, -100,
    200, -20),
  nrow = 2, byrow = TRUE,
  dimnames = list(default_action_names, default_state_names)
)
default_prior <- c(0.6, 0.4)
default_likelihood <- matrix(
  c(0.80, 0.20,
    0.05, 0.95),
  nrow = 2, byrow = TRUE,
  dimnames = list(default_state_names, default_signal_names)
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

  titlePanel(div(class = "title-main", "风险型决策分析教学网页：期望收益与完全信息价值")),

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
      h4("试销/调查成本"),
      numericInput("sample_cost", "试销或调查成本 Cs（万元）", value = 0, min = 0, step = 1),
      helpText("EVSI 是样本信息价值，尚未扣除试销成本。净样本信息价值 = EVSI - Cs，仅当该值 > 0 时试销在经济上才有利。"),
      tags$hr(),

      checkboxInput("teacher_mode", "显示教师自测区域", value = FALSE),

      tags$hr(),
      helpText("输入说明："),
      tags$ul(
        tags$li("收益/成本矩阵：行=方案，列=自然状态，单位为万元。"),
        tags$li("先验概率：各自然状态的概率，必须非负且和为 1（允许 1e-8 数值误差）。"),
        tags$li("似然矩阵：行=真实状态，列=试销结果/信号，每行之和为 1（允许 1e-8 数值误差）。"),
        tags$li("若概率或行和不等于 1，页面会提示修改，不会自动归一化。"),
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
            p("风险型决策是指决策者知道各自然状态出现的概率（或可估计概率），并据此计算期望指标进行决策。本网页以教材第三章“风险型决策分析”为背景，帮助理解期望准则、后悔值、完全信息价值（EVPI）与样本信息价值（EVSI）的计算与含义。"),
            tags$details(
              tags$summary("查看计算说明"),
              br(),
              tags$ul(
                tags$li("收益型：EMV_i = Σ_j p_j · x_{ij}，选择 EMV 最大的方案；EVPI = Σ_j p_j · max_i(x_{ij}) - max_i(EMV_i)。"),
                tags$li("成本型：EC_i = Σ_j p_j · c_{ij}，选择 EC 最小的方案；EVPI = min_i(EC_i) - Σ_j p_j · min_i(c_{ij})。"),
                tags$li("后悔值按每个自然状态分别计算：收益型 regret_{ij}=max_i(x_{ij})-x_{ij}；成本型 regret_{ij}=c_{ij}-min_i(c_{ij})。"),
                tags$li("后验概率由贝叶斯公式给出：P(状态|信号) ∝ P(信号|状态)·P(状态)。")
              )
            ),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("掌握收益型与成本型问题的期望准则：收益型最大化 EMV，成本型最小化 EC；"),
              tags$li("理解完全信息价值 EVPI 的经济含义及其与最小期望机会损失（EOL）的关系；"),
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
            helpText("各自然状态的概率，非负且和必须严格等于 1。"),
            rHandsontableOutput("prior_hot")
          ),
          div(
            class = "info-box",
            h4("似然矩阵 P(信号 | 真实状态)"),
            helpText("行=真实状态，列=试销/信号结果，每行概率之和必须严格等于 1。"),
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
            h4("完全信息价值 EVPI"),
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
            h4("样本信息价值 EVSI"),
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
            tags$p(class = "small-note", "仅当自然状态数为 2 时绘制此图；横轴为第一个自然状态的先验概率，第二个状态概率为 1-p。若状态数大于 2，图中会给出说明。")
          ),
          div(
            class = "info-box",
            h4("后验概率分布"),
            plotOutput("posterior_plot", height = "360px")
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
                DTOutput("self_test_table")
            )
          ),
          conditionalPanel(
            condition = "input.teacher_mode == false",
            div(class = "info-box",
                p("请在左侧勾选“显示教师自测区域”后运行自测。"))
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
    payoff_tbl <- hot_to_r(input$payoff_hot)
    prior_tbl <- hot_to_r(input$prior_hot)
    likelihood_tbl <- hot_to_r(input$likelihood_hot)

    if (is.null(payoff_tbl) || is.null(prior_tbl) || is.null(likelihood_tbl)) {
      showNotification("请先生成并填写所有矩阵。", type = "error")
      return()
    }

    payoff <- suppressWarnings(as.matrix(payoff_tbl))
    rownames(payoff) <- rv$action_names
    colnames(payoff) <- rv$state_names
    prior <- suppressWarnings(as.numeric(prior_tbl[1, ]))
    likelihood <- suppressWarnings(as.matrix(likelihood_tbl))
    rownames(likelihood) <- rv$state_names
    colnames(likelihood) <- rv$signal_names

    if (any(is.na(likelihood))) {
      showNotification("似然矩阵存在缺失或非数值，请检查。", type = "error")
      return()
    }
    # 似然矩阵校验
    likelihood <- matrix(pmax(likelihood, 0), nrow = sqrt(length(likelihood)))
    rs <- rowSums(likelihood)
    if (any(rs == 0)) {
      showNotification("似然矩阵存在行和为 0 的行。", type = "error")
      return()
    }
    if (any(abs(rs - 1) > 1e-8)) {
      showNotification("似然矩阵某些行之和与 1 的偏差超过 1e-8，请修改输入。", type = "error")
      return()
    }

    is_benefit <- input$problem_type == "benefit"
    type <- if (is_benefit) "benefit" else "cost"

    res <- tryCatch({
      r <- calc_risk_decision(payoff, prior, type)
      bayes <- calc_bayesian_evsi(payoff, prior, likelihood, type)
      c(r, bayes)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })

    if (!is.null(res)) {
      res$action_names <- rv$action_names
      res$state_names <- rv$state_names
      res$signal_names <- rv$signal_names
    }
    rv$res <- res
  })

  output$prior_metric_cards <- renderUI({
    req(rv$res)
    label <- if (rv$res$type == "benefit") "最大期望收益（EMV）" else "最小期望成本（EC）"
    best_action <- paste(rv$res$action_names[rv$res$best_idx], collapse = ", ")
    fluidRow(
      column(4, div(class = "metric-card",
                    div(class = "metric-title", "先验最优方案"),
                    div(class = "metric-value", best_action),
                    div(class = "metric-note", if (rv$res$type == "benefit") "EMV 最大" else "EC 最小"))),
      column(4, div(class = "metric-card",
                    div(class = "metric-title", label),
                    div(class = "metric-value", sprintf("%.2f 万元", rv$res$best_val)))),
      column(4, div(class = "metric-card",
                    div(class = "metric-title", "EVPI"),
                    div(class = "metric-value", sprintf("%.2f 万元", rv$res$evpi)),
                    div(class = "metric-note", "完全信息价值")))
    )
  })

  output$prior_table <- renderDT({
    req(rv$res)
    res <- rv$res
    col_name <- if (res$type == "benefit") "期望收益 EMV（万元）" else "期望成本 EC（万元）"
    df <- data.frame(
      方案 = res$action_names,
      期望指标 = round(res$expected, 4),
      是否最优 = ifelse(seq_along(res$action_names) %in% res$best_idx, "是", "否"),
      stringsAsFactors = FALSE
    )
    colnames(df)[2] <- col_name
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
              caption = if (res$type == "benefit") "先验决策表（收益型）" else "先验决策表（成本型）")
  })

  output$regret_note <- renderUI({
    req(rv$res)
    if (rv$res$type == "benefit") {
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
    if (res$type == "benefit") {
      txt <- paste0(
        "EVPI = 完全信息期望收益 - 先验最优期望收益<br/>",
        "= ", round(res$evwpi, 4), " - ", round(res$best_val, 4),
        " = <b>", round(res$evpi, 4), " 万元</b><br/><br/>",
        "含义：为获得完全信息所愿支付的最高价格；若信息费用低于 EVPI，则获取信息有利。"
      )
    } else {
      txt <- paste0(
        "EVPI = 先验最优期望成本 - 完全信息期望成本<br/>",
        "= ", round(res$best_val, 4), " - ", round(res$ecwpi, 4),
        " = <b>", round(res$evpi, 4), " 万元</b><br/><br/>",
        "含义：为获得完全信息所愿支付的最高价格；若信息费用低于 EVPI，则获取信息有利。"
      )
    }
    div(class = "formula-box", HTML(txt))
  })

  output$prior_explain <- renderUI({
    req(rv$res)
    res <- rv$res
    type_txt <- if (res$type == "benefit") "期望收益最大" else "期望成本最小"
    best_action <- paste(res$action_names[res$best_idx], collapse = ", ")
    explain <- sprintf("根据%s准则，推荐方案为 <b>%s</b>，其先验%s为 %.4f 万元。",
                       type_txt, best_action, res$label, res$best_val)
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
              caption = if (rv$res$type == "benefit") "各信号下的最优决策（收益型）" else "各信号下的最优决策（成本型）")
  })

  output$evsi_box <- renderUI({
    req(rv$res)
    res <- rv$res
    cs <- input$sample_cost
    net_evsi <- res$evsi - cs
    decision_txt <- if (net_evsi > 0) {
      sprintf("净样本信息价值 EVSI - Cs = %.4f - %.4f = %.4f 万元 > 0，进行试销/调查在经济上值得。", res$evsi, cs, net_evsi)
    } else {
      sprintf("净样本信息价值 EVSI - Cs = %.4f - %.4f = %.4f 万元 ≤ 0，进行试销/调查在经济上不值得。", res$evsi, cs, net_evsi)
    }
    div(class = "formula-box",
        HTML(paste0(
          "EVSI = |后验最优期望指标 - 先验最优期望指标|<br/>",
          "= |", round(res$expected_posterior_value, 4), " - ", round(res$best_val, 4),
          "| = <b>", round(res$evsi, 4), " 万元</b><br/><br/>",
          "试销/调查成本 Cs = ", cs, " 万元<br/>",
          decision_txt, "<br/><br/>",
          "注意：EVSI 是样本信息价值，尚未扣除试销成本；只有 EVSI > Cs 时试销才具有经济价值。"
        )))
  })

  output$posterior_explain <- renderUI({
    req(rv$res)
    res <- rv$res
    txt <- sprintf("在各信号下分别按%s准则决策，再按信号边际概率加权，得到后验最优期望指标为 %.4f 万元。",
                   if (res$type == "benefit") "期望收益最大" else "期望成本最小",
                   res$expected_posterior_value)
    div(class = "info-box",
        h4("决策解释"),
        p(txt),
        p(sprintf("EVSI = %.4f 万元，试销成本 Cs = %.4f 万元。净样本信息价值 = %.4f 万元。", res$evsi, input$sample_cost, res$evsi - input$sample_cost)))
  })

  output$prior_sensitivity_plot <- renderPlot({
    req(rv$res)
    res <- rv$res
    n_s <- length(res$state_names)
    if (n_s != 2) {
      return(
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = "仅当自然状态为 2 个时绘制此图；\n状态数大于 2 时，其余状态概率需按比例调整。") +
          theme_void()
      )
    }
    p_seq <- seq(0, 1, length.out = 101)
    df_list <- lapply(seq_along(res$action_names), function(i) {
      val <- p_seq * res$payoff_matrix[i, 1] + (1 - p_seq) * res$payoff_matrix[i, 2]
      data.frame(p = p_seq, 期望指标 = val, 方案 = res$action_names[i])
    })
    df <- do.call(rbind, df_list)
    y_label <- if (res$type == "benefit") "期望收益（万元）" else "期望成本（万元）"
    ggplot(df, aes(x = p, y = 期望指标, color = 方案)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = res$prob[1], linetype = "dotted", color = "gray40") +
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

  self_test_res <- eventReactive(input$run_self_test, {
    run_risk_self_tests()
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
