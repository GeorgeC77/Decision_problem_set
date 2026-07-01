# 依赖包：shiny, DT, visNetwork, ggplot2
# install.packages(c("shiny", "DT", "visNetwork", "ggplot2"))

library(shiny)
library(DT)
library(visNetwork)
library(ggplot2)

addResourcePath("assets", normalizePath("."))

# =========================
# 1. 基本设定（教材第六章马尔可夫决策思想 + Q-learning 教学示例）
# =========================
states <- 0:5
goal_state <- 5

neighbors <- list(
  "0" = c(2),
  "1" = c(3, 5),
  "2" = c(0, 3, 4),
  "3" = c(1, 2, 4),
  "4" = c(2, 3, 5),
  "5" = c(1, 4)
)

build_R <- function(goal_state = 5) {
  R <- matrix(-1, nrow = 6, ncol = 6)
  
  for (s in 0:5) {
    R[s + 1, s + 1] <- ifelse(s == goal_state, 100, 0)
    for (nxt in neighbors[[as.character(s)]]) {
      R[s + 1, nxt + 1] <- ifelse(nxt == goal_state, 100, 0)
    }
  }
  
  rownames(R) <- paste0("S", 0:5)
  colnames(R) <- paste0("S", 0:5)
  R
}

valid_actions <- function(state, R) {
  which(R[state + 1, ] >= 0) - 1
}

make_Q <- function() {
  Q <- matrix(0, nrow = 6, ncol = 6)
  rownames(Q) <- paste0("S", 0:5)
  colnames(Q) <- paste0("S", 0:5)
  Q
}

# 提取当前最优策略（贪心）
extract_policy <- function(Q, R) {
  policy <- character(6)
  for (s in 0:5) {
    acts <- valid_actions(s, R)
    if (length(acts) > 0) {
      best <- acts[which.max(Q[s + 1, acts + 1])]
      policy[s + 1] <- paste0("S", best)
    } else {
      policy[s + 1] <- "—"
    }
  }
  data.frame(
    状态 = paste0("S", 0:5),
    推荐动作 = policy,
    stringsAsFactors = FALSE
  )
}

# =========================
# 2. 动态图布局
# =========================
node_positions <- data.frame(
  id = 0:5,
  label = paste0("状态 ", 0:5),
  x = c(80, 260, 180, 400, 620, 720),
  y = c(280, 280, 130, 130, 130, 280)
)

graph_edges_base <- data.frame(
  from = c(0, 1, 1, 2, 3, 4, 2),
  to   = c(2, 3, 5, 3, 4, 5, 4),
  stringsAsFactors = FALSE
)

is_last_edge <- function(a, b, last_from, last_to) {
  if (is.null(last_from) || is.null(last_to)) return(FALSE)
  (a == last_from && b == last_to) || (a == last_to && b == last_from)
}

build_graph_edges <- function(last_from = NULL, last_to = NULL) {
  edges <- graph_edges_base
  
  edges$arrows <- "to, from"
  edges$width <- 2.5
  edges$color.color <- "#7f8c8d"
  edges$color.highlight <- "#34495e"
  edges$shadow.enabled <- FALSE
  
  edges$smooth.enabled <- FALSE
  edges$smooth.type <- NA
  edges$smooth.roundness <- NA
  
  idx_24 <- which(edges$from == 2 & edges$to == 4)
  if (length(idx_24) > 0) {
    edges$smooth.enabled[idx_24] <- TRUE
    edges$smooth.type[idx_24] <- "curvedCW"
    edges$smooth.roundness[idx_24] <- 0.42
  }
  
  if (!is.null(last_from) && !is.null(last_to) && last_from != last_to) {
    for (k in seq_len(nrow(edges))) {
      a <- edges$from[k]
      b <- edges$to[k]
      if (is_last_edge(a, b, last_from, last_to)) {
        edges$width[k] <- 5
        edges$color.color[k] <- "#d62728"
        edges$color.highlight[k] <- "#d62728"
        edges$shadow.enabled[k] <- TRUE
      }
    }
  }
  
  edges
}

# =========================
# 3. UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .top-row {
        display: flex;
        flex-wrap: wrap;
        gap: 16px;
        width: 100%;
        margin-bottom: 20px;
        align-items: stretch;
      }
      .top-left {
        flex: 0 0 32%;
        min-width: 280px;
        box-sizing: border-box;
        background: #f8f9fa;
        border: 1px solid #d9d9d9;
        border-radius: 8px;
        padding: 18px 20px;
        line-height: 1.9;
        min-height: 380px;
      }
      .top-right {
        flex: 1 1 0;
        min-width: 320px;
        box-sizing: border-box;
        border: 1px solid #d9d9d9;
        background: white;
        padding: 8px;
        min-height: 380px;
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
      }
      .top-right img {
        display: block;
        max-width: 100%;
        max-height: 320px;
        width: auto;
        height: auto;
      }
      .network-row {
        margin-top: 16px;
        background: white;
        border: 1px solid #d9d9d9;
        border-radius: 8px;
        padding: 12px;
        overflow-x: auto;
        overflow-y: hidden;
      }
      .network-box {
        width: 800px;
        min-width: 800px;
        height: 320px;
        margin: 0 auto;
      }
      .path-row { margin-top: 16px; }
      @media (max-width: 900px) {
        .top-left, .top-right { flex: 0 0 100%; min-height: auto; }
        .top-right img { max-height: 300px; }
      }
      .metric-box {
        background: #f8f9fa; border: 1px solid #d9d9d9;
        border-radius: 8px; padding: 12px; margin-bottom: 10px;
      }
    "))
  ),
  
  titlePanel("Q-learning 教学互动网页：房间脱出问题"),
  
  div(
    style = "
      background: #fff3cd;
      border: 1px solid #f0d98c;
      border-radius: 8px;
      padding: 12px 16px;
      margin: 6px 0 16px 0;
      font-size: 14px;
      line-height: 1.7;
      color: #7a4b00;
    ",
    HTML("
    <b>版权声明：</b><br/>
    《Q-learning 教学互动网页：房间脱出问题》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("gamma", "折扣因子 γ", min = 0, max = 0.99, value = 0.80, step = 0.01),
      sliderInput("alpha", "学习率 α", min = 0.01, max = 1, value = 1.00, step = 0.01),
      sliderInput("epsilon", "探索率 ε", min = 0, max = 1, value = 0.30, step = 0.01),
      helpText("ε=1 表示完全随机探索；ε=0 表示完全按当前 Q 值选择动作（贪心）。"),
      tags$hr(),
      
      actionButton("random_start", "随机选择一个初始状态", class = "btn-primary"),
      actionButton("step_once", "单步迭代"),
      actionButton("auto_episode", "自动完成当前回合"),
      actionButton("train_100", "额外训练100回合", class = "btn-info"),
      actionButton("reset_q", "重置Q矩阵", class = "btn-warning"),
      
      tags$hr(),
      h4("规则说明"),
      tags$ul(
        tags$li("每个状态只能转移到相邻房间，或者停留在本房间。"),
        tags$li("无效动作的奖励为 -1，且不会被选中。"),
        tags$li("进入目标状态 5 的奖励为 100。"),
        tags$li("在目标状态 5 停留的奖励也为 100。"),
        tags$li("其他合法动作奖励为 0。")
      ),
      
      tags$hr(),
      h4("当前状态"),
      verbatimTextOutput("status_text"),
      
      tags$hr(),
      h4("当前可行动作"),
      verbatimTextOutput("valid_actions_text"),
      
      tags$hr(),
      h4("本次更新公式"),
      htmlOutput("formula_text")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "示意图",
          br(),
          
          div(
            class = "top-row",
            div(
              class = "top-left",
              h4("问题说明"),
              tags$p("这是一个经典的房间脱出（Room Escape）Q-learning 教学例子，对应教材第六章“序贯决策分析”中马尔可夫决策与强化学习的思想。"),
              tags$p("图中的每个房间对应一个状态，智能体可以从当前房间移动到相邻房间，或者停留在当前房间。"),
              tags$p("目标是从任意随机初始状态出发，学习如何尽快到达目标房间（状态 5）。"),
              tags$p("右侧静态图展示房间布局；下方动态图展示状态转移关系和当前迭代过程。"),
              tags$p(tags$b("目标状态：5"))
            ),
            div(
              class = "top-right",
              tags$img(src = "assets/Layout.svg", alt = "房间布局示意图")
            )
          ),
          
          div(
            class = "network-row",
            div(class = "network-box", visNetworkOutput("state_graph", width = "800px", height = "380px"))
          ),
          
          div(
            class = "path-row",
            h4("当前回合路径"),
            verbatimTextOutput("path_text")
          )
        ),
        
        tabPanel(
          "矩阵",
          br(),
          fluidRow(
            column(
              6,
              h4("奖励矩阵 R"),
              DTOutput("r_table")
            ),
            column(
              6,
              h4("Q 矩阵"),
              DTOutput("q_table")
            )
          ),
          br(),
          div(
            class = "info-box",
            h4("当前最优策略（贪心）"),
            DTOutput("policy_table")
          )
        ),
        
        tabPanel(
          "逐步迭代日志",
          br(),
          DTOutput("log_table")
        ),
        
        tabPanel(
          "学习曲线",
          br(),
          div(
            class = "info-box",
            h4("Q 矩阵最大值随训练回合的变化"),
            plotOutput("learning_curve", height = "360px"),
            tags$p(class = "small-note", "说明：曲线趋于平稳表示 Q 矩阵已收敛。")
          ),
          div(
            class = "info-box",
            h4("到达目标所需的平均步数"),
            plotOutput("steps_curve", height = "360px")
          )
        )
      )
    )
  )
)

# =========================
# 4. Server
# =========================
server <- function(input, output, session) {
  R_mat <- build_R(goal_state = goal_state)
  
  rv <- reactiveValues(
    Q = make_Q(),
    start_state = NULL,
    current_state = NULL,
    episode_path = integer(0),
    step_count = 0,
    episode_done = TRUE,
    auto_running = FALSE,
    last_update = NULL,
    logs = data.frame(
      步数 = integer(),
      当前状态 = character(),
      选择动作 = character(),
      奖励 = numeric(),
      下一状态最大Q = numeric(),
      旧Q值 = numeric(),
      目标值 = numeric(),
      新Q值 = numeric(),
      策略类型 = character(),
      stringsAsFactors = FALSE
    ),
    max_q_history = numeric(),
    steps_history = integer(),
    episode_count = 0
  )
  
  reset_episode_only <- function() {
    rv$start_state <- NULL
    rv$current_state <- NULL
    rv$episode_path <- integer(0)
    rv$step_count <- 0
    rv$episode_done <- TRUE
    rv$auto_running <- FALSE
    rv$last_update <- NULL
    rv$logs <- rv$logs[0, ]
  }
  
  # epsilon-greedy 动作选择
  choose_action <- function(state) {
    acts <- valid_actions(state, R_mat)
    if (length(acts) == 0) return(NULL)
    if (runif(1) < input$epsilon) {
      list(action = sample(acts, 1), type = "探索（随机）")
    } else {
      best_acts <- acts[which.max(rv$Q[state + 1, acts + 1])]
      # 若有多个相同最大 Q，随机选一个
      list(action = sample(best_acts, 1), type = "利用（贪心）")
    }
  }
  
  do_one_step <- function(log_it = TRUE) {
    if (is.null(rv$current_state) || rv$episode_done) return(invisible(NULL))
    
    s <- rv$current_state
    chosen <- choose_action(s)
    if (is.null(chosen)) return(invisible(NULL))
    a <- chosen$action
    s_next <- a
    
    old_q <- rv$Q[s + 1, a + 1]
    
    next_acts <- valid_actions(s_next, R_mat)
    max_next_q <- if (length(next_acts) > 0) {
      max(rv$Q[s_next + 1, next_acts + 1])
    } else {
      0
    }
    
    reward <- R_mat[s + 1, a + 1]
    target <- reward + input$gamma * max_next_q
    new_q <- old_q + input$alpha * (target - old_q)
    
    rv$Q[s + 1, a + 1] <- new_q
    rv$step_count <- rv$step_count + 1
    rv$current_state <- s_next
    rv$episode_path <- c(rv$episode_path, s_next)
    
    rv$last_update <- list(
      s = s,
      a = a,
      s_next = s_next,
      reward = reward,
      old_q = old_q,
      max_next_q = max_next_q,
      target = target,
      new_q = new_q,
      type = chosen$type
    )
    
    if (log_it) {
      rv$logs <- rbind(
        rv$logs,
        data.frame(
          步数 = rv$step_count,
          当前状态 = paste0("S", s),
          选择动作 = paste0("S", a),
          奖励 = reward,
          下一状态最大Q = round(max_next_q, 4),
          旧Q值 = round(old_q, 4),
          目标值 = round(target, 4),
          新Q值 = round(new_q, 4),
          策略类型 = chosen$type,
          stringsAsFactors = FALSE
        )
      )
    }
    
    if (s_next == goal_state || rv$step_count >= 50) {
      rv$episode_done <- TRUE
      rv$auto_running <- FALSE
    }
  }
  
  train_many_episodes <- function(n_episodes = 100) {
    steps_vec <- integer(n_episodes)
    for (ep in seq_len(n_episodes)) {
      s <- sample(0:4, 1)
      step_guard <- 0
      
      while (s != goal_state && step_guard < 100) {
        acts <- valid_actions(s, R_mat)
        if (length(acts) == 0) break
        
        if (runif(1) < input$epsilon) {
          a <- sample(acts, 1)
        } else {
          best_acts <- acts[which.max(rv$Q[s + 1, acts + 1])]
          a <- sample(best_acts, 1)
        }
        s_next <- a
        
        old_q <- rv$Q[s + 1, a + 1]
        next_acts <- valid_actions(s_next, R_mat)
        max_next_q <- if (length(next_acts) > 0) max(rv$Q[s_next + 1, next_acts + 1]) else 0
        
        reward <- R_mat[s + 1, a + 1]
        target <- reward + input$gamma * max_next_q
        new_q <- old_q + input$alpha * (target - old_q)
        rv$Q[s + 1, a + 1] <- new_q
        
        s <- s_next
        step_guard <- step_guard + 1
      }
      
      rv$episode_count <- rv$episode_count + 1
      rv$max_q_history <- c(rv$max_q_history, max(rv$Q))
      steps_vec[ep] <- step_guard
    }
    rv$steps_history <- c(rv$steps_history, steps_vec)
  }
  
  observeEvent(input$random_start, {
    rv$auto_running <- FALSE
    rv$start_state <- sample(0:4, 1)
    rv$current_state <- rv$start_state
    rv$episode_path <- rv$start_state
    rv$step_count <- 0
    rv$episode_done <- FALSE
    rv$last_update <- NULL
    rv$logs <- rv$logs[0, ]
  })
  
  observeEvent(input$step_once, {
    if (is.null(rv$current_state)) {
      showNotification("请先点击“随机选择一个初始状态”。", type = "warning")
      return()
    }
    if (!rv$episode_done) do_one_step(log_it = TRUE)
  })
  
  observeEvent(input$auto_episode, {
    if (is.null(rv$current_state)) {
      showNotification("请先点击“随机选择一个初始状态”。", type = "warning")
      return()
    }
    if (!rv$episode_done) rv$auto_running <- TRUE
  })
  
  observe({
    if (rv$auto_running) {
      invalidateLater(700, session)
      if (!rv$episode_done) {
        do_one_step(log_it = TRUE)
      } else {
        rv$auto_running <- FALSE
      }
    }
  })
  
  observeEvent(input$train_100, {
    train_many_episodes(100)
    showNotification("已额外训练 100 回合。", type = "message")
  })
  
  observeEvent(input$reset_q, {
    rv$Q <- make_Q()
    reset_episode_only()
    rv$max_q_history <- numeric()
    rv$steps_history <- integer()
    rv$episode_count <- 0
    showNotification("Q矩阵已重置。", type = "message")
  })
  
  output$status_text <- renderText({
    start_txt <- if (is.null(rv$start_state)) "尚未选择" else paste0("S", rv$start_state)
    curr_txt  <- if (is.null(rv$current_state)) "尚未开始" else paste0("S", rv$current_state)
    done_txt  <- if (rv$episode_done) "是" else "否"
    
    paste0(
      "目标状态：S", goal_state, "\n",
      "本回合初始状态：", start_txt, "\n",
      "当前所在状态：", curr_txt, "\n",
      "已迭代步数：", rv$step_count, "\n",
      "当前回合是否结束：", done_txt, "\n",
      "已完成训练回合：", rv$episode_count
    )
  })
  
  output$valid_actions_text <- renderText({
    if (is.null(rv$current_state)) {
      return("尚未开始，请先随机选择一个初始状态。")
    }
    acts <- valid_actions(rv$current_state, R_mat)
    paste0(
      "当前状态：S", rv$current_state, "\n",
      "可行动作集合：{ ",
      paste(paste0("S", acts), collapse = ", "),
      " }"
    )
  })
  
  output$formula_text <- renderUI({
    if (is.null(rv$last_update)) {
      HTML("尚未进行迭代。请先随机选择初始状态。")
    } else {
      u <- rv$last_update
      HTML(paste0(
        "<b>更新位置：</b> Q(S", u$s, ", S", u$a, ")<br/>",
        "<b>动作选择：</b>", u$type, "<br/>",
        "<b>公式：</b> Q(s,a) ← Q(s,a) + α [ R(s,a) + γ max Q(s',·) - Q(s,a) ]<br/><br/>",
        "<b>代入数值：</b><br/>",
        "Q(S", u$s, ", S", u$a, ") ← ", round(u$old_q, 4),
        " + ", input$alpha,
        " × [ ", u$reward,
        " + ", input$gamma,
        " × ", round(u$max_next_q, 4),
        " - ", round(u$old_q, 4),
        " ]<br/>",
        "= <b>", round(u$new_q, 4), "</b>"
      ))
    }
  })
  
  output$path_text <- renderText({
    if (length(rv$episode_path) == 0) {
      "尚未开始。"
    } else {
      paste(paste0("S", rv$episode_path), collapse = "  →  ")
    }
  })
  
  output$state_graph <- renderVisNetwork({
    nodes <- node_positions
    nodes$shape <- "ellipse"
    nodes$color.background <- "#d9edf7"
    nodes$color.border <- "#2c3e50"
    nodes$borderWidth <- 2
    
    nodes$color.background[nodes$id == goal_state] <- "#8fd19e"
    nodes$color.border[nodes$id == goal_state] <- "#2e7d32"
    nodes$borderWidth[nodes$id == goal_state] <- 3
    
    if (!is.null(rv$current_state)) {
      nodes$color.background[nodes$id == rv$current_state] <- "#ffcc80"
      nodes$color.border[nodes$id == rv$current_state] <- "#e65100"
      nodes$borderWidth[nodes$id == rv$current_state] <- 4
    }
    
    edges <- if (is.null(rv$last_update)) {
      build_graph_edges()
    } else {
      build_graph_edges(rv$last_update$s, rv$last_update$a)
    }
    
    visNetwork(nodes, edges, width = "800px", height = "320px") %>%
      visOptions(autoResize = FALSE) %>%
      visEdges(smooth = FALSE, shadow = FALSE) %>%
      visNodes(
        fixed = list(x = TRUE, y = TRUE),
        font = list(size = 22, face = "Microsoft YaHei")
      ) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
      visLayout(randomSeed = 12)
  })
  
  output$r_table <- renderDT({
    R_show <- as.data.frame(R_mat)
    datatable(
      R_show,
      rownames = rownames(R_mat),
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "奖励矩阵 R（无效动作 = -1）"
    ) %>%
      formatStyle(
        columns = names(R_show),
        target = "cell",
        backgroundColor = styleEqual(-1, "#f2f2f2")
      )
  })
  
  output$q_table <- renderDT({
    Q_show <- as.data.frame(round(rv$Q, 4))
    dt <- datatable(
      Q_show,
      rownames = rownames(rv$Q),
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "当前 Q 矩阵"
    )
    
    if (!is.null(rv$last_update)) {
      target_col <- rv$last_update$a + 1
      target_row <- rv$last_update$s + 1
      col_name <- colnames(Q_show)[target_col]
      
      dt <- dt %>%
        formatStyle(
          columns = col_name,
          valueColumns = col_name,
          target = "cell",
          backgroundColor = styleEqual(
            round(rv$Q[target_row, target_col], 4),
            "#ffe082"
          )
        )
    }
    dt
  })
  
  output$policy_table <- renderDT({
    policy_df <- extract_policy(rv$Q, R_mat)
    datatable(
      policy_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "每个状态当前应选择的贪心动作"
    )
  })
  
  output$log_table <- renderDT({
    datatable(
      rv$logs,
      rownames = FALSE,
      options = list(pageLength = 12, ordering = FALSE),
      caption = "当前初始状态下的逐步迭代日志"
    )
  })
  
  output$learning_curve <- renderPlot({
    req(length(rv$max_q_history) > 0)
    df <- data.frame(
      回合 = seq_along(rv$max_q_history),
      最大Q值 = rv$max_q_history
    )
    ggplot(df, aes(x = 回合, y = 最大Q值)) +
      geom_line(color = "#2c7fb8", linewidth = 1) +
      geom_point(color = "#2c7fb8", size = 2) +
      labs(x = "训练回合", y = "Q 矩阵最大值") +
      theme_minimal(base_size = 14)
  })
  
  output$steps_curve <- renderPlot({
    req(length(rv$steps_history) > 0)
    df <- data.frame(
      回合 = seq_along(rv$steps_history),
      步数 = rv$steps_history
    )
    ggplot(df, aes(x = 回合, y = 步数)) +
      geom_line(color = "#d95f02", linewidth = 1) +
      geom_point(color = "#d95f02", size = 2) +
      labs(x = "训练回合", y = "到达目标所需步数") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
