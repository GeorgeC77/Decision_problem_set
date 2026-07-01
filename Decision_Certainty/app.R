# 依赖包：shiny, DT, ggplot2, scales
# install.packages(c("shiny", "DT", "ggplot2"))

library(shiny)
library(DT)
library(ggplot2)

# =========================
# 工具函数
# =========================
fmt_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "—",
    formatC(x, format = "f", digits = digits, big.mark = ",")
  )
}

pvaf <- function(r, n) {
  if (abs(r) < 1e-10) {
    n
  } else {
    (1 - (1 + r)^(-n)) / r
  }
}

# 计算 IRR：使得 NPV = 0 的贴现率（使用二分法）
calc_irr <- function(cash_flows, low = -0.999, high = 2, tol = 1e-6) {
  npv_at <- function(r) sum(cash_flows / (1 + r)^(seq_along(cash_flows) - 1))
  
  if (length(unique(sign(cash_flows[cash_flows != 0]))) == 1) {
    return(NA_real_)  # 现金流符号无变化，无意义
  }
  
  # 保证根在 [low, high] 内
  f_low <- npv_at(low)
  f_high <- npv_at(high)
  if (is.na(f_low) || is.na(f_high)) return(NA_real_)
  
  for (i in seq_len(100)) {
    mid <- (low + high) / 2
    f_mid <- npv_at(mid)
    if (is.na(f_mid)) break
    if (abs(f_mid) < tol) return(mid)
    if (f_low * f_mid <= 0) {
      high <- mid
      f_high <- f_mid
    } else {
      low <- mid
      f_low <- f_mid
    }
  }
  (low + high) / 2
}

# =========================
# 默认参数（教材案例二：自制还是外购）
# =========================
default_vals <- list(
  demand = 80000,          # 年需求量
  buy_price = 10,          # 外购单价
  equip_invest = 380000,   # 设备投资
  salvage = 20000,         # 残值
  working_capital = 100000,# 流动资金
  unit_var_cost = 6,       # 单位变动成本
  fixed_cash_cost = 150000,# 年固定成本（不含折旧）
  tax_rate = 40,           # 所得税率 %
  discount_rate = 12,      # 资本成本 %
  life = 4                 # 使用年限
)

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f9fb;
      }
      .title-main {
        font-weight: 700;
        color: #1f3b5b;
      }
      .copyright-box {
        background: #fff3cd;
        border: 1px solid #f0d98c;
        border-radius: 8px;
        padding: 12px 16px;
        margin: 8px 0 16px 0;
        line-height: 1.7;
        color: #7a4b00;
        font-size: 14px;
      }
      .info-box {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 10px;
        padding: 16px 18px;
        margin-bottom: 14px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
      }
      .metric-card {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 14px 16px;
        margin-bottom: 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
        min-height: 108px;
      }
      .metric-title {
        color: #5b7083;
        font-size: 13px;
        margin-bottom: 8px;
      }
      .metric-value {
        font-size: 26px;
        font-weight: 700;
        color: #1f3b5b;
      }
      .metric-note {
        font-size: 12px;
        color: #6b7c93;
        margin-top: 6px;
      }
      .good {
        color: #1b7f3b;
        font-weight: 700;
      }
      .bad {
        color: #b42318;
        font-weight: 700;
      }
      .formula-box {
        background: #f8fafc;
        border: 1px solid #d9e2ec;
        border-radius: 10px;
        padding: 14px 16px;
        line-height: 1.8;
      }
      .small-note {
        color: #667085;
        font-size: 13px;
      }
      .sidebar-note {
        background: #eef6ff;
        border: 1px solid #cfe2ff;
        border-radius: 8px;
        padding: 10px 12px;
        line-height: 1.6;
        font-size: 13px;
        color: #1f3b5b;
      }
      .btn-row {
        margin-top: 10px;
      }
    "))
  ),
  
  titlePanel(div(class = "title-main", "确定型决策分析教学网页：案例二 自制还是外购决策")),
  
  div(
    class = "copyright-box",
    HTML("
    <b>版权声明：</b><br/>
    《确定型决策分析教学网页：案例二 自制还是外购决策》应用程序 © 2026 中国石油大学（华东）崔耕，
    采用 <b>CC BY-NC-SA 4.0</b>（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。<br/>
    如发现任何程序缺陷或错误，请发送邮件至
    <a href='mailto:gengc25@hotmail.com'>gengc25@hotmail.com</a>。
  ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("参数设置"),
      numericInput("demand", "年需求量（件）", value = default_vals$demand, min = 1, step = 1000),
      numericInput("buy_price", "外购单价（元/件）", value = default_vals$buy_price, min = 0, step = 0.1),
      numericInput("equip_invest", "设备投资（元）", value = default_vals$equip_invest, min = 0, step = 10000),
      numericInput("salvage", "设备残值（元）", value = default_vals$salvage, min = 0, step = 5000),
      numericInput("working_capital", "垫支流动资金（元）", value = default_vals$working_capital, min = 0, step = 10000),
      numericInput("unit_var_cost", "自制单位变动成本（元/件）", value = default_vals$unit_var_cost, min = 0, step = 0.1),
      numericInput("fixed_cash_cost", "年固定成本（不含折旧，元）", value = default_vals$fixed_cash_cost, min = 0, step = 10000),
      sliderInput("tax_rate", "所得税率（%）", min = 0, max = 60, value = default_vals$tax_rate, step = 1),
      sliderInput("discount_rate", "资本成本/贴现率（%）", min = 0, max = 30, value = default_vals$discount_rate, step = 0.5),
      numericInput("life", "设备使用年限（年）", value = default_vals$life, min = 1, max = 10, step = 1),
      
      div(
        class = "btn-row",
        actionButton("reset_btn", "恢复教材默认值", class = "btn-warning"),
        downloadButton("download_results", "下载结果")
      ),
      tags$hr(),
      
      div(
        class = "sidebar-note",
        HTML("
          <b>使用建议：</b><br/>
          1. 先使用教材默认值，观察网页如何得到“自制优于外购”的结论；<br/>
          2. 再修改年需求量、外购单价、资本成本等参数；<br/>
          3. 重点观察：NPV 是否变号、临界需求量如何变化、IRR 与资本成本的关系。
        ")
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
            tags$p("某公司为了生产产品 A，每年需要零件 h 80,000 件。"),
            tags$p("方案一：直接外购，每件外购价格（含运费）可设定。"),
            tags$p("方案二：选择自制，则需要新增专用设备投资，并占用流动资金；同时存在单位变动成本、年固定成本和折旧。"),
            tags$p("在所得税率与资本成本已知的条件下，比较“自制”相对“外购”的增量净现值（ΔNPV），从而判断哪种方案更优。"),
            tags$hr(),
            h4("核心教学目标"),
            tags$ul(
              tags$li("理解增量现金流思想：比较的不是两个方案各自的总值，而是自制相对于外购的“差额现金流”。"),
              tags$li("理解折旧税盾：折旧本身不是现金流，但会影响纳税，从而影响经营现金流。"),
              tags$li("理解临界点：当需求量、外购单价或资本成本变化时，最优方案可能发生改变。"),
              tags$li("掌握 IRR、回收期等辅助评价指标的含义与计算。")
            ),
            tags$hr(),
            h4("本网页的主要假设"),
            tags$ul(
              tags$li("采用直线折旧法，折旧基数为：设备投资 - 残值。"),
              tags$li("终结期回收残值和流动资金。"),
              tags$li("案例聚焦教学演示，暂不单独处理残值处置税差。"),
              tags$li("每年经营现金流相同，最后一年额外回收残值与流动资金。")
            )
          )
        ),
        
        tabPanel(
          "结果总览",
          br(),
          uiOutput("metric_cards"),
          br(),
          uiOutput("decision_text"),
          br(),
          uiOutput("formula_box")
        ),
        
        tabPanel(
          "现金流表",
          br(),
          div(
            class = "info-box",
            h4("增量现金流明细表（自制 - 外购）"),
            DTOutput("cf_table")
          ),
          div(
            class = "info-box",
            h4("关键指标汇总"),
            DTOutput("summary_table")
          )
        ),
        
        tabPanel(
          "图形分析",
          br(),
          div(
            class = "info-box",
            h4("图 1：各年增量现金流"),
            plotOutput("cf_plot", height = "340px"),
            tags$p(class = "small-note", "说明：第 0 年为初始投资与流动资金占用，最后一年包含终结回收。")
          ),
          div(
            class = "info-box",
            h4("图 2：需求量变化对 ΔNPV 的影响"),
            plotOutput("npv_plot", height = "340px"),
            tags$p(class = "small-note", "说明：这张图能帮助学生理解“为什么存在临界需求量”。")
          ),
          div(
            class = "info-box",
            h4("图 3：自制与外购的年成本结构对比"),
            plotOutput("cost_plot", height = "320px"),
            tags$p(class = "small-note", "说明：自制方案分为变动成本、固定成本和折旧；外购方案主要体现为采购成本。")
          ),
          div(
            class = "info-box",
            h4("图 4：参数敏感性龙卷风图"),
            plotOutput("tornado_plot", height = "360px"),
            tags$p(class = "small-note", "说明：以教材默认值为基准，各参数在 ±20% 范围内变动时 ΔNPV 的变化幅度。基线以上的参数对 ΔNPV 影响更大。")
          )
        ),
        
        tabPanel(
          "课堂练习",
          br(),
          div(
            class = "info-box",
            h4("建议学生亲手尝试的 6 个问题"),
            uiOutput("practice_box")
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
  
  observeEvent(input$reset_btn, {
    updateNumericInput(session, "demand", value = default_vals$demand)
    updateNumericInput(session, "buy_price", value = default_vals$buy_price)
    updateNumericInput(session, "equip_invest", value = default_vals$equip_invest)
    updateNumericInput(session, "salvage", value = default_vals$salvage)
    updateNumericInput(session, "working_capital", value = default_vals$working_capital)
    updateNumericInput(session, "unit_var_cost", value = default_vals$unit_var_cost)
    updateNumericInput(session, "fixed_cash_cost", value = default_vals$fixed_cash_cost)
    updateSliderInput(session, "tax_rate", value = default_vals$tax_rate)
    updateSliderInput(session, "discount_rate", value = default_vals$discount_rate)
    updateNumericInput(session, "life", value = default_vals$life)
  })
  
  calc <- reactive({
    demand <- input$demand
    buy_price <- input$buy_price
    equip_invest <- input$equip_invest
    salvage <- input$salvage
    working_capital <- input$working_capital
    unit_var_cost <- input$unit_var_cost
    fixed_cash_cost <- input$fixed_cash_cost
    tax <- input$tax_rate / 100
    r <- input$discount_rate / 100
    n <- input$life
    
    dep <- (equip_invest - salvage) / n
    make_cash_cost <- demand * unit_var_cost + fixed_cash_cost
    buy_cash_cost <- demand * buy_price
    
    annual_ocf <- (buy_cash_cost - make_cash_cost) * (1 - tax) + dep * tax
    terminal_extra <- salvage + working_capital
    
    years <- 0:n
    cash_flows <- c(-(equip_invest + working_capital), rep(annual_ocf, n))
    cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + terminal_extra
    
    pv_factors <- 1 / (1 + r)^years
    pv_values <- cash_flows * pv_factors
    cum_pv <- cumsum(pv_values)
    npv <- sum(pv_values)
    
    annuity_factor <- pvaf(r, n)
    terminal_pv <- terminal_extra / (1 + r)^n
    
    # 静态回收期（年）
    cumulative_cf <- cumsum(cash_flows)
    payback <- NA_real_
    pos_idx <- which(cumulative_cf >= 0)[1]
    if (!is.na(pos_idx) && pos_idx > 1) {
      payback <- (pos_idx - 1) + abs(cumulative_cf[pos_idx - 1]) / cash_flows[pos_idx]
    } else if (!is.na(pos_idx) && pos_idx == 1) {
      payback <- 0
    }
    
    # 动态回收期（按累计现值）
    discounted_payback <- NA_real_
    pos_idx2 <- which(cum_pv >= 0)[1]
    if (!is.na(pos_idx2) && pos_idx2 > 1) {
      discounted_payback <- (pos_idx2 - 1) + abs(cum_pv[pos_idx2 - 1]) / pv_values[pos_idx2]
    } else if (!is.na(pos_idx2) && pos_idx2 == 1) {
      discounted_payback <- 0
    }
    
    # IRR
    irr <- calc_irr(cash_flows)
    
    denom <- (buy_price - unit_var_cost) * (1 - tax) * annuity_factor
    break_even_demand <- if (abs(denom) < 1e-10) {
      NA_real_
    } else {
      (equip_invest + working_capital - terminal_pv +
         fixed_cash_cost * (1 - tax) * annuity_factor -
         dep * tax * annuity_factor) / denom
    }
    
    # 敏感性：需求量变化
    demand_seq <- seq(max(1, demand * 0.4), max(2, demand * 1.6), length.out = 120)
    annual_ocf_seq <- (demand_seq * (buy_price - unit_var_cost) - fixed_cash_cost) * (1 - tax) + dep * tax
    npv_seq <- -(equip_invest + working_capital) + annual_ocf_seq * annuity_factor + terminal_pv
    
    sensitivity_df <- data.frame(
      需求量 = demand_seq,
      NPV = npv_seq
    )
    
    cf_table <- data.frame(
      年度 = years,
      增量现金流 = round(cash_flows, 2),
      现值系数 = round(pv_factors, 4),
      现值 = round(pv_values, 2),
      累计现值 = round(cum_pv, 2),
      check.names = FALSE
    )
    
    summary_df <- data.frame(
      指标 = c(
        "年折旧",
        "自制年付现成本",
        "外购年付现成本",
        "年增量经营现金流",
        "初始总投资",
        "终结期回收",
        "增量净现值 ΔNPV",
        "内部收益率 IRR",
        "静态回收期（年）",
        "动态回收期（年）",
        "临界需求量"
      ),
      数值 = c(
        dep,
        make_cash_cost,
        buy_cash_cost,
        annual_ocf,
        equip_invest + working_capital,
        terminal_extra,
        npv,
        ifelse(is.na(irr), NA_real_, irr * 100),
        payback,
        discounted_payback,
        break_even_demand
      ),
      单位 = c("元", "元", "元", "元", "元", "元", "元", "%", "年", "年", "件"),
      check.names = FALSE
    )
    
    cost_df <- data.frame(
      方案 = c("自制", "自制", "自制", "外购"),
      项目 = c("变动成本", "固定成本", "折旧", "采购成本"),
      金额 = c(demand * unit_var_cost, fixed_cash_cost, dep, demand * buy_price)
    )
    
    # 龙卷风图数据：各参数 ±20% 对 NPV 的影响
    base_npv <- npv
    tornado_vars <- c(
      "年需求量" = "demand",
      "外购单价" = "buy_price",
      "单位变动成本" = "unit_var_cost",
      "设备投资" = "equip_invest",
      "年固定成本" = "fixed_cash_cost",
      "资本成本" = "discount_rate",
      "残值" = "salvage",
      "流动资金" = "working_capital"
    )
    
    calc_npv_with <- function(var_name, multiplier) {
      d <- demand; bp <- buy_price; ev <- equip_invest; sv <- salvage
      wc <- working_capital; uv <- unit_var_cost; fc <- fixed_cash_cost
      tr <- tax; dr <- r; nn <- n
      
      switch(var_name,
             demand = d <- d * multiplier,
             buy_price = bp <- bp * multiplier,
             unit_var_cost = uv <- uv * multiplier,
             equip_invest = ev <- ev * multiplier,
             fixed_cash_cost = fc <- fc * multiplier,
             discount_rate = dr <- dr * multiplier,
             salvage = sv <- sv * multiplier,
             working_capital = wc <- wc * multiplier
      )
      
      dd <- (ev - sv) / nn
      mc <- d * uv + fc
      bc <- d * bp
      ocf <- (bc - mc) * (1 - tr) + dd * tr
      te <- sv + wc
      af <- pvaf(dr, nn)
      tp <- te / (1 + dr)^nn
      -(ev + wc) + ocf * af + tp
    }
    
    tornado_df <- do.call(rbind, lapply(names(tornado_vars), function(nm) {
      var <- tornado_vars[[nm]]
      npv_low <- calc_npv_with(var, 0.8)
      npv_high <- calc_npv_with(var, 1.2)
      data.frame(
        参数 = nm,
        低点 = npv_low - base_npv,
        高点 = npv_high - base_npv,
        幅度 = max(abs(npv_low - base_npv), abs(npv_high - base_npv)),
        stringsAsFactors = FALSE
      )
    }))
    tornado_df <- tornado_df[order(tornado_df$幅度), ]
    tornado_df$参数 <- factor(tornado_df$参数, levels = tornado_df$参数)
    
    list(
      dep = dep,
      make_cash_cost = make_cash_cost,
      buy_cash_cost = buy_cash_cost,
      annual_ocf = annual_ocf,
      terminal_extra = terminal_extra,
      npv = npv,
      irr = irr,
      payback = payback,
      discounted_payback = discounted_payback,
      cf_table = cf_table,
      summary_df = summary_df,
      cost_df = cost_df,
      sensitivity_df = sensitivity_df,
      tornado_df = tornado_df,
      break_even_demand = break_even_demand,
      tax = tax,
      r = r,
      n = n,
      demand = demand,
      buy_price = buy_price,
      unit_var_cost = unit_var_cost,
      fixed_cash_cost = fixed_cash_cost,
      equip_invest = equip_invest,
      working_capital = working_capital,
      salvage = salvage,
      cash_flows = cash_flows,
      years = years
    )
  })
  
  output$metric_cards <- renderUI({
    res <- calc()
    
    rec_text <- if (res$npv > 0) "建议自制" else if (res$npv < 0) "建议外购" else "两方案无差异"
    rec_class <- if (res$npv > 0) "good" else if (res$npv < 0) "bad" else ""
    
    be_text <- if (is.na(res$break_even_demand) || !is.finite(res$break_even_demand)) {
      "不存在/不适用"
    } else {
      paste0(fmt_num(res$break_even_demand, 0), " 件")
    }
    
    irr_text <- if (is.na(res$irr)) "不存在" else paste0(fmt_num(res$irr * 100, 2), "%")
    
    fluidRow(
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-title", "增量净现值 ΔNPV"),
          div(class = paste("metric-value", rec_class), paste0(fmt_num(res$npv, 0), " 元")),
          div(class = "metric-note", "ΔNPV > 0 表示自制优于外购")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-title", "内部收益率 IRR"),
          div(class = "metric-value", irr_text),
          div(class = "metric-note", "IRR > 资本成本时项目经济上可行")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-title", "临界需求量"),
          div(class = "metric-value", be_text),
          div(class = "metric-note", "需求量高于该值时，自制更可能优于外购")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-title", "当前决策建议"),
          div(class = paste("metric-value", rec_class), rec_text),
          div(class = "metric-note", "结论会随参数变化而变化")
        )
      )
    )
  })
  
  output$decision_text <- renderUI({
    res <- calc()
    
    rec_html <- if (res$npv > 0) {
      HTML(paste0(
        "<span class='good'>当前结论：自制优于外购。</span><br/>",
        "原因是：在当前参数下，自制相对于外购的增量净现值 ΔNPV = ",
        fmt_num(res$npv, 0), " 元，为正值。"
      ))
    } else if (res$npv < 0) {
      HTML(paste0(
        "<span class='bad'>当前结论：外购优于自制。</span><br/>",
        "原因是：在当前参数下，自制相对于外购的增量净现值 ΔNPV = ",
        fmt_num(res$npv, 0), " 元，为负值。"
      ))
    } else {
      HTML("当前结论：两方案净现值相同。")
    }
    
    dp_text <- if (is.na(res$discounted_payback)) "无法收回" else paste0(fmt_num(res$discounted_payback, 2), " 年")
    sp_text <- if (is.na(res$payback)) "无法收回" else paste0(fmt_num(res$payback, 2), " 年")
    
    div(
      class = "info-box",
      h4("一句话解释"),
      rec_html,
      tags$hr(),
      HTML(paste0(
        "<b>回收期：</b>静态回收期 = ", sp_text,
        "；动态回收期 = ", dp_text,
        "。<br/><b>临界判断：</b>若年需求量高于 ",
        ifelse(is.na(res$break_even_demand), "—", fmt_num(res$break_even_demand, 0)),
        " 件，自制方案更具经济优势。"
      ))
    )
  })
  
  output$formula_box <- renderUI({
    res <- calc()
    
    div(
      class = "formula-box",
      HTML(paste0(
        "<b>1. 年折旧：</b><br/>",
        "年折旧 = (设备投资 - 残值) / 使用年限 = (",
        fmt_num(res$equip_invest, 0), " - ", fmt_num(res$salvage, 0),
        ") / ", res$n, " = <b>", fmt_num(res$dep, 2), "</b><br/><br/>",
        
        "<b>2. 自制年付现成本：</b><br/>",
        "需求量 × 单位变动成本 + 年固定成本 = ",
        fmt_num(res$demand, 0), " × ", fmt_num(res$unit_var_cost, 2),
        " + ", fmt_num(res$fixed_cash_cost, 0),
        " = <b>", fmt_num(res$make_cash_cost, 2), "</b><br/><br/>",
        
        "<b>3. 外购年付现成本：</b><br/>",
        "需求量 × 外购单价 = ",
        fmt_num(res$demand, 0), " × ", fmt_num(res$buy_price, 2),
        " = <b>", fmt_num(res$buy_cash_cost, 2), "</b><br/><br/>",
        
        "<b>4. 年增量经营现金流：</b><br/>",
        "[(外购年付现成本 - 自制年付现成本) × (1 - 税率)] + 折旧 × 税率<br/>",
        "= [(",
        fmt_num(res$buy_cash_cost, 2), " - ", fmt_num(res$make_cash_cost, 2),
        ") × (1 - ", res$tax * 100, "%)] + ",
        fmt_num(res$dep, 2), " × ", res$tax * 100, "%<br/>",
        "= <b>", fmt_num(res$annual_ocf, 2), "</b><br/><br/>",
        
        "<b>5. 增量净现值：</b><br/>",
        "ΔNPV = 初始现金流 + 各年经营现金流现值 + 终结点回收现值 = <b>",
        fmt_num(res$npv, 2), "</b>"
      ))
    )
  })
  
  output$cf_table <- renderDT({
    res <- calc()
    datatable(
      res$cf_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "各年增量现金流、现值与累计现值"
    )
  })
  
  output$summary_table <- renderDT({
    res <- calc()
    show_df <- res$summary_df
    # 根据指标格式化
    show_df$数值 <- sapply(seq_len(nrow(show_df)), function(i) {
      u <- show_df$单位[i]
      v <- show_df$数值[i]
      if (is.na(v)) return("—")
      if (u == "%") return(paste0(fmt_num(v, 2), "%"))
      if (u == "年") return(fmt_num(v, 2))
      if (u == "件") return(fmt_num(v, 0))
      fmt_num(v, 2)
    })
    show_df$单位 <- NULL
    
    datatable(
      show_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      caption = "关键指标汇总"
    )
  })
  
  output$cf_plot <- renderPlot({
    res <- calc()
    df <- res$cf_table
    df$颜色 <- ifelse(df$增量现金流 >= 0, "流入/回收", "投资/流出")
    
    ggplot(df, aes(x = factor(年度), y = 增量现金流, fill = 颜色)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = fmt_num(增量现金流, 0)),
                vjust = ifelse(df$增量现金流 >= 0, -0.4, 1.2),
                size = 4) +
      scale_fill_manual(values = c("流入/回收" = "#1b9e77", "投资/流出" = "#d95f02")) +
      labs(x = "年份", y = "现金流（元）", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  output$npv_plot <- renderPlot({
    res <- calc()
    df <- res$sensitivity_df
    
    p <- ggplot(df, aes(x = 需求量, y = NPV)) +
      geom_line(linewidth = 1.2, color = "#2c7fb8") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#b42318") +
      geom_point(aes(x = res$demand, y = res$npv), color = "#1b7f3b", size = 3) +
      annotate("text",
               x = res$demand,
               y = res$npv,
               label = paste0("当前情形\n(", fmt_num(res$demand, 0), "件, ", fmt_num(res$npv, 0), "元)"),
               vjust = -1,
               color = "#1b7f3b",
               size = 4) +
      labs(x = "年需求量（件）", y = "ΔNPV（元）") +
      theme_minimal(base_size = 14)
    
    if (!is.na(res$break_even_demand) &&
        is.finite(res$break_even_demand) &&
        res$break_even_demand >= min(df$需求量) &&
        res$break_even_demand <= max(df$需求量)) {
      p <- p +
        geom_vline(xintercept = res$break_even_demand, linetype = "dotted", color = "#7a4b00") +
        annotate("text",
                 x = res$break_even_demand,
                 y = max(df$NPV, na.rm = TRUE) * 0.85,
                 label = paste0("临界需求量\n", fmt_num(res$break_even_demand, 0), " 件"),
                 color = "#7a4b00",
                 size = 4)
    }
    
    p
  })
  
  output$cost_plot <- renderPlot({
    res <- calc()
    df <- res$cost_df
    df$项目 <- factor(df$项目, levels = c("变动成本", "固定成本", "折旧", "采购成本"))
    
    ggplot(df, aes(x = 方案, y = 金额, fill = 项目)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = fmt_num(金额, 0)),
                position = position_stack(vjust = 0.5),
                color = "white",
                size = 4) +
      scale_fill_manual(values = c("变动成本" = "#4c78a8", "固定成本" = "#f58518",
                                   "折旧" = "#54a24b", "采购成本" = "#e45756")) +
      labs(x = NULL, y = "金额（元）", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  output$tornado_plot <- renderPlot({
    res <- calc()
    df <- res$tornado_df
    
    df_long <- data.frame(
      参数 = rep(df$参数, 2),
      变化 = c(rep("-20%", nrow(df)), rep("+20%", nrow(df))),
      NPV变化 = c(df$低点, df$高点),
      stringsAsFactors = FALSE
    )
    df_long$xend <- ifelse(df_long$变化 == "-20%", df_long$NPV变化, 0)
    df_long$xstart <- ifelse(df_long$变化 == "-20%", 0, df_long$NPV变化)
    
    ggplot(df_long, aes(y = 参数, fill = 变化)) +
      geom_segment(aes(x = xstart, xend = xend, yend = 参数),
                   linewidth = 8, lineend = "round") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#333") +
      scale_fill_manual(values = c("-20%" = "#d95f02", "+20%" = "#1b9e77")) +
      labs(x = "ΔNPV 变化（元）", y = NULL, fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  output$practice_box <- renderUI({
    res <- calc()
    
    be_msg <- if (is.na(res$break_even_demand) || !is.finite(res$break_even_demand)) {
      "试着把外购单价和自制单位变动成本调得更接近，看看“临界需求量”为什么会变得不稳定。"
    } else {
      paste0("把年需求量改到接近 ", fmt_num(res$break_even_demand, 0), " 件，观察 ΔNPV 为什么会接近 0。")
    }
    
    tagList(
      tags$ol(
        tags$li("把外购单价从当前值逐步调低，找出“自制不再占优”的拐点。"),
        tags$li("把年需求量逐步提高，观察图 2 中 ΔNPV 曲线如何上升。"),
        tags$li(be_msg),
        tags$li("把资本成本从当前值提高到 15%、18%、20%，思考为什么未来现金流的价值会下降。"),
        tags$li("把垫支流动资金调大，比较初始投资增加后对最终决策的影响。"),
        tags$li("观察图 4 龙卷风图，判断在当前参数下对 ΔNPV 影响最大的因素是哪个。")
      ),
      tags$hr(),
      HTML(paste0(
        "<b>课堂提示：</b> 当前情形下，ΔNPV = ",
        fmt_num(res$npv, 0),
        " 元，IRR = ",
        ifelse(is.na(res$irr), "不存在", paste0(fmt_num(res$irr * 100, 2), "%")),
        "，说明“自制相对外购”",
        ifelse(res$npv > 0, "增加了企业价值。", "降低了企业价值。")
      ))
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("决策结果_自制外购_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- calc()
      out <- cbind(res$cf_table, res$summary_df)
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
