# 依赖包：shiny, ggplot2
# install.packages(c("shiny", "ggplot2"))

library(shiny)
library(ggplot2)

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
    "))
  ),
  
  titlePanel(div(class = "title-main", "大数据分析与决策教学网页：客户细分演示")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《大数据分析与决策教学网页：客户细分演示》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("数据设置"),
      numericInput("n_customers", "客户数量", value = 200, min = 50, max = 1000, step = 50),
      sliderInput("k", "聚类数 K", min = 2, max = 6, value = 3, step = 1),
      actionButton("regenerate", "重新生成数据", class = "btn-primary"),
      tags$hr(),
      h4("说明"),
      tags$p("横轴：年均消费次数；纵轴：年均消费金额。通过 K-means 聚类将客户分为不同群体，辅助企业制定差异化营销策略。")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "案例说明",
          br(),
          div(
            class = "info-box",
            h4("大数据决策的核心思想"),
            tags$p("教材第九章指出，大数据决策是利用大数据技术对海量数据进行采集、预处理、分析和挖掘，从中发现规律、支持决策的过程。"),
            tags$p("客户细分是大数据营销中的典型应用：根据客户的消费行为特征，将客户划分为不同群体，以便进行精准营销。"),
            tags$hr(),
            h4("教学目标"),
            tags$ul(
              tags$li("理解 K-means 聚类的基本思想；"),
              tags$li("观察不同 K 值对聚类结果的影响；"),
              tags$li("理解大数据如何从“数据”转化为“决策支持信息”。")
            )
          )
        ),
        
        tabPanel(
          "聚类结果",
          br(),
          div(
            class = "info-box",
            plotOutput("cluster_plot", height = "480px")
          ),
          div(
            class = "info-box",
            h4("各簇统计"),
            verbatimTextOutput("cluster_summary")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL, clusters = NULL)
  
  generate_data <- function(n) {
    # 生成 3 个自然簇的数据
    n1 <- round(n * 0.4)
    n2 <- round(n * 0.35)
    n3 <- n - n1 - n2
    
    df1 <- data.frame(
      消费次数 = rnorm(n1, 5, 1.5),
      消费金额 = rnorm(n1, 300, 80)
    )
    df2 <- data.frame(
      消费次数 = rnorm(n2, 15, 2.5),
      消费金额 = rnorm(n2, 800, 120)
    )
    df3 <- data.frame(
      消费次数 = rnorm(n3, 25, 3),
      消费金额 = rnorm(n3, 1500, 200)
    )
    df <- rbind(df1, df2, df3)
    df$消费次数 <- pmax(df$消费次数, 0)
    df$消费金额 <- pmax(df$消费金额, 0)
    df
  }
  
  observe({
    input$regenerate
    input$n_customers
    rv$data <- generate_data(input$n_customers)
  })
  
  # K-means 简单实现（避免依赖额外包）
  kmeans_simple <- function(X, k, max_iter = 50) {
    n <- nrow(X)
    set.seed(42)
    centers <- X[sample(n, k), ]
    clusters <- integer(n)
    
    for (iter in seq_len(max_iter)) {
      # 分配
      for (i in seq_len(n)) {
        dists <- apply(centers, 1, function(c) sum((X[i, ] - c)^2))
        clusters[i] <- which.min(dists)
      }
      # 更新中心
      new_centers <- do.call(rbind, lapply(seq_len(k), function(j) {
        idx <- which(clusters == j)
        if (length(idx) == 0) return(centers[j, ])
        colMeans(X[idx, , drop = FALSE])
      }))
      if (all(abs(new_centers - centers) < 1e-6)) break
      centers <- new_centers
    }
    list(clusters = clusters, centers = centers)
  }
  
  output$cluster_plot <- renderPlot({
    req(rv$data)
    X <- as.matrix(rv$data)
    res <- kmeans_simple(X, input$k)
    rv$clusters <- res$clusters
    
    df <- rv$data
    df$簇 <- factor(res$clusters)
    centers <- as.data.frame(res$centers)
    centers$簇 <- factor(seq_len(nrow(centers)))
    
    ggplot(df, aes(x = 消费次数, y = 消费金额, color = 簇)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_point(data = centers, aes(x = 消费次数, y = 消费金额),
                 color = "black", size = 6, shape = 4, stroke = 2) +
      labs(title = "K-means 客户细分结果", color = "客户群") +
      theme_minimal(base_size = 14)
  })
  
  output$cluster_summary <- renderText({
    req(rv$data, rv$clusters)
    df <- rv$data
    df$簇 <- rv$clusters
    summary_text <- sapply(sort(unique(rv$clusters)), function(k) {
      sub <- df[df$簇 == k, ]
      paste0(
        "簇 ", k, "：客户数 ", nrow(sub),
        "，平均消费次数 ", round(mean(sub$消费次数), 2),
        "，平均消费金额 ", round(mean(sub$消费金额), 2), " 元"
      )
    })
    paste(summary_text, collapse = "\n")
  })
}

shinyApp(ui, server)
