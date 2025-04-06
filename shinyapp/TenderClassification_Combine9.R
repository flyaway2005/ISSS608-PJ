# 修改 Monthly Analysis Plot 部分
output$monthly_analysis_plot <- renderPlotly({
  # 根據選擇的時間單位進行分組
  time_unit <- switch(input$time_unit,
                    "Monthly" = "month",
                    "Quarterly" = "quarter",
                    "Yearly" = "year")
  
  # 計算每個時間單位的總標案數和總金額
  monthly_data <- market_data %>%
    mutate(
      time_point = floor_date(as.Date(award_date, format = "%d/%m/%Y"), time_unit),
      awarded_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
    ) %>%
    group_by(time_point) %>%
    summarise(
      count = n(),
      total_value = sum(awarded_value, na.rm = TRUE)
    )
  
  # 建立主圖表
  p1 <- ggplot(monthly_data, aes(x = time_point)) +
    geom_bar(aes(y = count), stat = "identity", fill = "steelblue") +
    geom_line(aes(y = total_value/1000), color = "red") +
    scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Total Value")) +
    theme_minimal() +
    labs(title = paste(input$time_unit, "Analysis"),
         x = input$time_unit,
         y = "Number of Tenders")
  
  # 計算每個時間單位各類別的標案金額
  monthly_category_data <- market_data %>%
  mutate(
      time_point = floor_date(as.Date(award_date, format = "%d/%m/%Y"), time_unit),
      awarded_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
    ) %>%
    group_by(time_point, LDA_Category) %>%
    summarise(
      category_value = sum(awarded_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(time_point) %>%
    mutate(
      total_month_value = sum(category_value, na.rm = TRUE),
      percentage = category_value / total_month_value * 100
    )
  
  # 建立類別金額分布圖
  p2 <- ggplot(monthly_category_data, aes(x = time_point, y = percentage, fill = LDA_Category)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "General Procurement" = "#FF6B6B",
      "Engineering Procurement" = "#4ECDC4",
      "PPP Procurement" = "#45B7D1",
      "Other" = "#96CEB4"
    )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(
      title = paste(input$time_unit, "Category Value Distribution"),
      x = input$time_unit,
      y = "Category Value Percentage (%)",
      fill = "Category"
    )
  
  # 將兩個圖表組合在一起
  subplot(
    ggplotly(p1, tooltip = c("time_point", "count", "total_value")),
    ggplotly(p2, tooltip = c("time_point", "LDA_Category", "percentage")),
    nrows = 2,
    heights = c(0.5, 0.5),
    shareX = TRUE,
    shareY = FALSE  # 不共用 Y 軸
  ) %>%
    layout(
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.2,
        xanchor = "right",
        x = 1
      ),
      hovermode = "x unified"
    )
})

# 修改 update_market_visualizations 函數
update_market_visualizations <- function(market_data) {
  # 確保 market_data 存在且不為空
  if (is.null(market_data) || nrow(market_data) == 0) {
    showNotification("No data available for visualization", type = "warning")
    return(NULL)
  }
  
    # Market Overview Boxes
    output$total_tenders <- renderValueBox({
      valueBox(
        value = nrow(market_data),
        subtitle = HTML("<span style='font-size: 16px; font-weight: bold;'>Total Tenders</span>"),
        icon = icon("file-contract"),
        color = "blue",
        width = 4
      )
    })
    
    output$total_value <- renderValueBox({
      valueBox(
      value = paste("$", format(sum(as.numeric(gsub("[^0-9.]", "", market_data$awarded_amt)), na.rm = TRUE), big.mark = ",")),
        subtitle = HTML("<span style='font-size: 16px; font-weight: bold;'>Total Value</span>"),
        icon = icon("dollar-sign"),
        color = "green",
        width = 4
      )
    })
    
    output$avg_value <- renderValueBox({
      valueBox(
      value = paste("$", format(mean(as.numeric(gsub("[^0-9.]", "", market_data$awarded_amt)), na.rm = TRUE), big.mark = ",")),
        subtitle = HTML("<span style='font-size: 16px; font-weight: bold;'>Average Value</span>"),
        icon = icon("chart-line"),
        color = "purple",
        width = 4
      )
    })
    
    # Market Trend Plot
    output$market_trend_plot <- renderPlotly({
      # 計算每半年的數據
      trend_data <- market_data %>%
        mutate(
        half_year = floor_date(as.Date(award_date, format = "%d/%m/%Y"), "6 months"),
        awarded_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
      ) %>%
      group_by(half_year, LDA_Category) %>%
        summarise(
          count = n(),
        total_value = sum(awarded_value, na.rm = TRUE)
      ) %>%
      ungroup() %>%
        group_by(half_year) %>%
      mutate(
        total_count = sum(count),
        total_value = sum(total_value),
        avg_value = total_value / total_count
      )
    
    # 建立圖表
    p <- ggplot() +
      # 堆疊長條圖顯示標案數量
      geom_bar(data = trend_data,
              aes(x = half_year, y = count, fill = LDA_Category),
              stat = "identity",
              position = "stack",
              alpha = 0.8) +
      # 線圖顯示平均金額
      geom_line(data = trend_data %>% distinct(half_year, avg_value),
               aes(x = half_year, y = avg_value/1000),
               color = "red",
               size = 1) +
      # 設定顏色
        scale_fill_manual(values = c(
          "General Procurement" = "#FF6B6B",
          "Engineering Procurement" = "#4ECDC4",
          "PPP Procurement" = "#45B7D1",
          "Other" = "#96CEB4"
        )) +
      # 設定雙 Y 軸
      scale_y_continuous(
        name = "Number of Tenders",
        sec.axis = sec_axis(~.*1000, name = "Average Value (K)")
      ) +
      # 主題設定
        theme_minimal() +
        theme(
        axis.title.y.left = element_text(color = "steelblue"),
        axis.title.y.right = element_text(color = "red"),
          legend.position = "bottom",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      # 標題和軸標籤
      labs(
        title = "Tender Analysis by Half Year",
        x = "Time Period",
        fill = "Category"
      )
    
    # 轉換為 plotly 並加入互動性
    ggplotly(p, tooltip = c("half_year", "LDA_Category", "count", "avg_value")) %>%
          layout(
        hovermode = "x unified",
            legend = list(
              orientation = "h",
              yanchor = "bottom",
          y = -0.2,
          xanchor = "right",
          x = 1
        )
      )
  })
  
  # Category Distribution Plot
  output$category_dist_plot <- renderPlotly({
    category_data <- market_data %>%
      mutate(
        awarded_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
      ) %>%
      group_by(LDA_Category) %>%
      summarise(
        count = n(),
        total_value = sum(awarded_value, na.rm = TRUE),
        avg_value = mean(awarded_value, na.rm = TRUE)
      )
    
    p <- ggplot(category_data, aes(x = reorder(LDA_Category, -count), y = count)) +
      geom_bar(stat = "identity", fill = "#4ECDC4", alpha = 0.8) +
      geom_text(aes(label = count), vjust = -0.5) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(
        title = "Tender Distribution by Category",
        x = "Category",
        y = "Number of Tenders"
      )
    
    ggplotly(p, tooltip = c("LDA_Category", "count", "total_value", "avg_value"))
  })
  
  # Top Agencies Table
  output$top_agencies_table <- renderDT({
    agency_data <- market_data %>%
      mutate(
        awarded_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
      ) %>%
      group_by(agency) %>%
      summarise(
        total_tenders = n(),
        total_value = sum(awarded_value, na.rm = TRUE),
        avg_value = mean(awarded_value, na.rm = TRUE)
      ) %>%
      arrange(desc(total_value))
    
    datatable(agency_data,
        options = list(
          pageLength = 10,
                scrollX = TRUE
              ),
              colnames = c("Agency", "Total Tenders", "Total Value", "Average Value"))
  })
} 