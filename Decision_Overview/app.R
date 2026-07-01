# 依赖包：shiny
# install.packages("shiny")

library(shiny)

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
      .choice-btn { margin: 6px; }
      .result-good { color: #1b7f3b; font-weight: 700; }
      .result-bad { color: #b42318; font-weight: 700; }
    "))
  ),
  
  titlePanel(div(class = "title-main", "决策分析概述教学网页：打鸡蛋案例")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《决策分析概述教学网页：打鸡蛋案例》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("案例设置"),
      sliderInput("bad_prob", "第 6 个鸡蛋是坏蛋的概率", min = 0, max = 1, value = 0.2, step = 0.05),
      tags$hr(),
      h4("你的决策方案"),
      radioButtons(
        "decision",
        "请选择：",
        choices = c(
          "直接打入已有 5 个鸡蛋的碗里" = "direct",
          "打入另一个碗里检查" = "check",
          "将鸡蛋丢弃" = "discard"
        )
      ),
      actionButton("simulate", "模拟一次", class = "btn-primary"),
      actionButton("simulate_many", "模拟 100 次", class = "btn-info"),
      tags$hr(),
      helpText("通过改变坏蛋概率和选择不同方案，理解决策分析的基本要素：决策者、自然状态、决策方案、决策结果、决策准则。")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "案例说明",
          br(),
          div(
            class = "info-box",
            h4("教材案例：家庭主妇打鸡蛋"),
            tags$p("一名家庭主妇准备用 6 个鸡蛋和一碗面粉做鸡蛋煎饼。她已经向碗里打了 5 个鸡蛋（假设都是好的），准备打第 6 个鸡蛋时，不知道这个鸡蛋的质量是好是坏。"),
            tags$p("她面临三种方案："),
            tags$ul(
              tags$li("直接打入碗里；"),
              tags$li("打入另一个碗里检查；"),
              tags$li("将鸡蛋丢弃。")
            ),
            tags$hr(),
            h4("决策分析基本要素"),
            tags$ul(
              tags$li("决策者：家庭主妇"),
              tags$li("决策目标：做出最好的煎饼，省力气、少洗碗"),
              tags$li("决策方案：直接打入、另碗检查、丢弃"),
              tags$li("自然状态：第 6 个鸡蛋质量好或坏"),
              tags$li("决策结果：不同方案在不同状态下的后果"),
              tags$li("决策准则：根据个人偏好选择最满意方案")
            )
          )
        ),
        
        tabPanel(
          "单次模拟",
          br(),
          div(
            class = "info-box",
            h4("模拟结果"),
            uiOutput("single_result")
          )
        ),
        
        tabPanel(
          "多次模拟统计",
          br(),
          div(
            class = "info-box",
            h4("100 次模拟结果统计"),
            verbatimTextOutput("many_result")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    last_result = NULL,
    stats = NULL
  )
  
  payoff_table <- list(
    direct_good = "做成 6 个鸡蛋的煎饼",
    direct_bad = "5 个鸡蛋受到污染，只能做无蛋煎饼",
    check_good = "做成 6 个鸡蛋的煎饼，多洗一个碗",
    check_bad = "做成 5 个鸡蛋煎饼，多洗一个碗",
    discard_good = "做成 5 个鸡蛋煎饼，浪费一个好蛋",
    discard_bad = "做成 5 个鸡蛋煎饼"
  )
  
  observeEvent(input$simulate, {
    is_bad <- runif(1) < input$bad_prob
    key <- paste0(input$decision, "_", if (is_bad) "bad" else "good")
    rv$last_result <- list(
      is_bad = is_bad,
      decision = input$decision,
      outcome = payoff_table[[key]]
    )
  })
  
  observeEvent(input$simulate_many, {
    n <- 100
    is_bads <- runif(n) < input$bad_prob
    decisions <- rep(input$decision, n)
    outcomes <- sapply(seq_len(n), function(i) {
      key <- paste0(decisions[i], "_", if (is_bads[i]) "bad" else "good")
      payoff_table[[key]]
    })
    
    rv$stats <- list(
      n = n,
      n_bad = sum(is_bads),
      n_good = n - sum(is_bads),
      table = table(outcomes)
    )
  })
  
  output$single_result <- renderUI({
    req(rv$last_result)
    r <- rv$last_result
    div(
      tags$p(HTML(paste0("<b>鸡蛋质量：</b>", ifelse(r$is_bad, "<span class='result-bad'>坏蛋</span>", "<span class='result-good'>好蛋</span>")))),
      tags$p(HTML(paste0("<b>你的选择：</b>", switch(r$decision,
                                                      direct = "直接打入碗里",
                                                      check = "打入另一个碗里检查",
                                                      discard = "将鸡蛋丢弃")))),
      tags$p(HTML(paste0("<b>结果：</b>", r$outcome)))
    )
  })
  
  output$many_result <- renderText({
    req(rv$stats)
    s <- rv$stats
    paste0(
      "模拟次数：", s$n, "\n",
      "坏蛋次数：", s$n_bad, "（", round(s$n_bad / s$n * 100, 1), "%）\n",
      "好蛋次数：", s$n_good, "（", round(s$n_good / s$n * 100, 1), "%）\n\n",
      "各种结果出现次数：\n",
      paste(names(s$table), s$table, sep = ": ", collapse = "\n")
    )
  })
}

shinyApp(ui, server)
