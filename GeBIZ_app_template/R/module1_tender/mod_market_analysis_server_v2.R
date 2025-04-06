mod_market_analysis_server <- function(id, lda_results, Cleaned_GP) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store market_data for downstream plots
    market_data <- reactiveVal(NULL)
    
    update_market_visualizations <- function(data) {
      output$total_tenders <- renderValueBox({
        valueBox(nrow(data), "Total Tenders", icon = icon("file-contract"), color = "blue")
      })
      output$total_value <- renderValueBox({
        valueBox(paste0("$", format(sum(data$tender_value, na.rm = TRUE), big.mark = ",")),
                 "Total Value", icon = icon("dollar-sign"), color = "green")
      })
      output$avg_value <- renderValueBox({
        valueBox(paste0("$", format(mean(data$tender_value, na.rm = TRUE), big.mark = ",")),
                 "Average Value", icon = icon("chart-line"), color = "purple")
      })
    }
    
    output$date_slider <- renderUI({
      min_date <- lubridate::floor_date(min(Cleaned_GP$tender_date, na.rm = TRUE), "month")
      max_date <- lubridate::ceiling_date(max(Cleaned_GP$tender_date, na.rm = TRUE), "month")
      dateRangeInput(ns("date_range"), "Select Date Range:",
                     start = min_date, end = max_date, min = min_date, max = max_date)
    })
    
    observeEvent(input$run_market_analysis, {
      req(lda_results())
      data <- Cleaned_GP %>%
        left_join(lda_results(), by = "tender_no") %>%
        filter(!is.na(tender_date), !is.na(tender_value), !is.na(LDA_Category))
      
      if (input$remove_outliers) {
        data <- data %>%
          group_by(LDA_Category) %>%
          mutate(
            Q1 = quantile(tender_value, 0.25),
            Q3 = quantile(tender_value, 0.75),
            IQR = Q3 - Q1,
            lower = Q1 - 1.5 * IQR,
            upper = Q3 + 1.5 * IQR
          ) %>%
          filter(tender_value >= lower, tender_value <= upper)
      }
      
      if (!is.null(input$date_range)) {
        data <- data %>%
          filter(tender_date >= input$date_range[1], tender_date <= input$date_range[2])
      }
      
      if (input$market_category != "All") {
        data <- data %>% filter(grepl(input$market_category, LDA_Category))
      }
      
      if (input$tender_status != "All") {
        data <- data %>% filter(tender_detail_status == input$tender_status)
      }
      
      market_data(data)  # Store for plots
      update_market_visualizations(data)
    })
    
    # Plot: Market Trend
    output$market_trend_plot <- renderPlotly({
      req(market_data())
      df <- market_data()
      
      trend_data <- df %>%
        mutate(half_year = lubridate::floor_date(tender_date, "6 months")) %>%
        group_by(half_year, LDA_Category) %>%
        summarise(count = n(), total_value = sum(tender_value, na.rm = TRUE), .groups = "drop")
      
      avg_data <- df %>%
        mutate(half_year = lubridate::floor_date(tender_date, "6 months")) %>%
        group_by(half_year) %>%
        summarise(avg_value = mean(tender_value, na.rm = TRUE), .groups = "drop")
      
      p1 <- ggplot(trend_data, aes(x = half_year, y = count, fill = LDA_Category,
                                   text = paste("Date:", half_year,
                                                "<br>Category:", LDA_Category,
                                                "<br>Tenders:", count))) +
        geom_bar(stat = "identity") +
        labs(title = "Number of Tenders by Category", x = "Half-Year", y = "Count") +
        theme_minimal()
      
      p2 <- ggplot(avg_data, aes(x = half_year, y = avg_value / 1000,
                                 text = paste("Date:", half_year,
                                              "<br>Avg Value:", round(avg_value / 1000)))) +
        geom_line(color = "red") + geom_point(color = "red") +
        labs(title = "Average Tender Value (in $K)", x = "Half-Year", y = "Value ($K)") +
        theme_minimal()
      
      subplot(
        ggplotly(p1, tooltip = "text"),
        ggplotly(p2, tooltip = "text"),
        nrows = 2,
        heights = c(0.6, 0.4),
        shareX = TRUE
      )
    })
    
    # Plot: Monthly/Quarterly/Yearly Trend
    output$monthly_analysis_plot <- renderPlotly({
      req(market_data())
      df <- market_data()
      time_unit <- input$time_unit
      
      df <- df %>%
        mutate(time_point = case_when(
          time_unit == "month" ~ floor_date(tender_date, "month"),
          time_unit == "quarter" ~ floor_date(tender_date, "quarter"),
          time_unit == "year" ~ floor_date(tender_date, "year")
        )) %>%
        group_by(time_point) %>%
        summarise(count = n(), total_value = sum(tender_value, na.rm = TRUE), .groups = "drop")
      
      # Calculate Y-axis range
      y_max <- max(df$count)
      y_breaks <- pretty(c(0, y_max), n = 5)
      
      # Set date format based on time unit
      date_format <- case_when(
        time_unit == "month" ~ "%Y-%m",
        time_unit == "quarter" ~ "%Y-Q%q",
        time_unit == "year" ~ "%Y"
      )
      
      p <- ggplot(df, aes(x = time_point,
                          text = paste("Date:", format(time_point, date_format),
                                       "<br>Number of Tenders:", count,
                                       "<br>Total Value:", sprintf("$%.2fK", total_value/1000)))) +
        geom_bar(aes(y = count), stat = "identity", fill = "steelblue", alpha = 0.7) +
        geom_line(aes(y = total_value/1000), color = "red", size = 1, alpha = 0.8) +
        geom_point(aes(y = total_value/1000), color = "red", size = 3, alpha = 0.8) +
        scale_y_continuous(
          name = "Number of Tenders",
          breaks = y_breaks,
          sec.axis = sec_axis(~.*1000, 
                              name = "Total Value",
                              labels = scales::label_number(scale_cut = cut_short_scale()))
        ) +
        labs(title = paste(tools::toTitleCase(time_unit), "Analysis"),
             x = case_when(
               time_unit == "month" ~ "Month",
               time_unit == "quarter" ~ "Quarter",
               time_unit == "year" ~ "Year"
             )) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white"),
          margin = list(b = 50),
          yaxis = list(
            range = c(0, y_max * 1.1)
          )
        )
    })
    
    # Plot: Dynamic Scatter
    output$dynamic_scatter_plot <- renderPlotly({
      req(market_data())
      df <- market_data()
      
      scatter_data <- df %>%
        mutate(half_year = floor_date(tender_date, "6 months")) %>%
        group_by(half_year, LDA_Category) %>%
        summarise(count = n(), total_amount = sum(tender_value, na.rm = TRUE), .groups = "drop") %>%
        group_by(half_year) %>%
        mutate(spending_ratio = (total_amount / sum(total_amount)) * 100) %>%
        ungroup()
      
      p <- ggplot(scatter_data, aes(x = count, y = spending_ratio, size = count, color = LDA_Category,
                                    frame = half_year,
                                    text = paste("Date:", half_year,
                                                 "<br>Category:", LDA_Category,
                                                 "<br>Count:", count,
                                                 "<br>Spending Ratio:", round(spending_ratio, 2), "%"))) +
        geom_point(alpha = 0.7) +
        labs(title = "Spending Ratio by Category over Time", x = "Tender Count", y = "Spending Ratio (%)") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>%
        animation_opts(frame = 1000, transition = 500, easing = "linear")
    })
  })
}