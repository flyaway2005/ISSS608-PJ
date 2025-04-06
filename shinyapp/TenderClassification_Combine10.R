# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(topicmodels)
library(tm)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(reshape2)
library(DT)
library(wordcloud)
library(shinyjs)
library(text2vec)
library(tm)
library(tidytext)
library(lubridate)
library(scales)
library(gganimate)

# **📌 Read Cleaned_GP_LDA Data**
Cleaned_GP_LDA <- read_csv("data/Cleaned_GP_LDA.csv")
Cleaned_GP <- read_csv("data/Cleaned_GP.csv") %>%  
  select(tender_no, award_date, awarded_amt, tender_detail_status) %>%  # Modified to tender_detail_status
  mutate(
    tender_date = as.Date(award_date, format = "%d/%m/%Y"),
    tender_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
  ) %>%
  filter(!is.na(tender_date), !is.na(tender_value))  # Ensure date and amount are not NA

# **📌 Default Stopwords**
default_stopwords <- c(stopwords("en"), "please", "refer", "another", "one", "two", "three", 
                       "framework", "edition", "related", "whole", "period", "government", 
                       "entities", "various", "including", "requirement", "provide", "supply", 
                       "service", "procurement", "year", "option", "extend", "agreement", 
                       "singapore", "Singapore")

# **📌 Shiny UI**
ui <- dashboardPage(
  dashboardHeader(title = "LDA Classification"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Selection", tabName = "data_selection", icon = icon("database")),
      menuItem("LDA Supervised Learning", tabName = "supervised", icon = icon("brain")),
      menuItem("LDA Clustering", tabName = "unsupervised", icon = icon("chart-line")),
      menuItem("Market Analysis", tabName = "market_analysis", icon = icon("chart-bar"))  # ✅ 新增市場分析分頁
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .small-box .inner h3 {
        font-size: 20px !important;  /* 調整數值的字體大小 */
      }
      .small-box .inner p {
        font-size: 20px !important;  /* 調整標題的字體大小 */
      }
    "))
    ),
    tabItems(
      # **📌 選擇數據量**
      tabItem(tabName = "data_selection",
              fluidPage(
                titlePanel("Select Data Sample Size"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("sample_size", "Choose Data Sample:", 
                                choices = c("1000" = 1000, "5000" = 5000, "10000" = 10000, "All" = nrow(Cleaned_GP_LDA)),
                                selected = 1000),  
                    actionButton("load_data", "Load Data")
                  ),
                  mainPanel(
                    verbatimTextOutput("data_summary")
                  )
                )
              )
      ),
      
      # **📌 LDA 監督學習**
      tabItem(tabName = "supervised",
              fluidPage(
                titlePanel("LDA Supervised Learning"),
                sidebarLayout(
                  sidebarPanel(
                    actionButton("run_supervised", "Run LDA Analysis"),
                    sliderInput("num_words", "Number of Words:", min = 5, max = 20, value = 10),
                    selectInput("lda_category", "Select LDA Category:", choices = NULL, selected = "All"),
                    actionButton("reload_lda", "Reload"),
                    hr(),
                    # 新增 Stopwords 管理區塊
                    h4("Stopwords Management"),
                    DTOutput("stopwords_table"),
                    textInput("new_stopword", "Add New Stopword:"),
                    actionButton("add_stopword", "Add Stopword"),
                    actionButton("remove_stopword", "Remove Selected"),
                    actionButton("reset_stopwords", "Reset to Default")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("LDA Distribution", plotlyOutput("lda_category_plot")),
                      tabPanel("TF-IDF Table", DTOutput("tfidf_table")),
                      tabPanel("Wordcloud", plotOutput("wordcloud")),
                      tabPanel("TF-IDF Bar Plot", plotlyOutput("tfidf_plot"))
                    )
                  )
                )
              )
      ),
      # **📌 LDA Clustering (Unsupervised)**
      tabItem(tabName = "unsupervised",
              fluidPage(
                titlePanel("LDA Clustering"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    # 修改 topic 數量選擇
                    numericInput("num_topics", 
                                 "Number of Topics:", 
                                 min = 2, 
                                 max = 10, 
                                 value = 10,
                                 step = 1),
                    # 修改 cluster 數量選擇
                    numericInput("num_clusters", 
                                 "Number of Clusters:", 
                                 min = 2, 
                                 max = 10, 
                                 value = 5,
                                 step = 1),
                    actionButton("run_unsupervised", 
                                 "Run Clustering",
                                 class = "btn-primary")
                  ),
                  mainPanel(
                    width = 9,
                    tabsetPanel(
                      tabPanel("All Clusters", 
                               plotlyOutput("cluster_plot", height = "500px")),
                      tabPanel("Single Cluster View", 
                               selectInput("select_cluster", 
                                           "Select Cluster:", 
                                           choices = NULL),
                               plotlyOutput("single_cluster_plot", height = "400px"))
                    )
                  )
                )
              )
      ),
      # **📌 Market Analysis**
      tabItem(tabName = "market_analysis",
              fluidPage(
                titlePanel("Tender Market Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    # 修改日期選擇為滑動條
                    uiOutput("date_slider"),
                    selectInput("market_category", "Select Category:",
                                choices = c("All", "General Procurement", "Engineering Procurement", "PPP Procurement")),
                    selectInput("tender_status", "Select Tender Status:",
                                choices = c("All", 
                                            "Awarded to Suppliers",
                                            "Awarded by Items",
                                            "Award by interface record")),
                    checkboxInput("remove_outliers", "Remove Outliers", value = TRUE),
                    actionButton("run_market_analysis", "Run Analysis")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Market Overview",
                               fluidRow(
                                 valueBoxOutput("total_tenders", width = 4),
                                 valueBoxOutput("total_value", width = 4),
                                 valueBoxOutput("avg_value", width = 4)
                               ),
                               plotlyOutput("market_trend_plot")),
                      tabPanel("Time Analysis",
                               selectInput("time_unit", "Select Time Unit:",
                                           choices = c("Monthly" = "month",
                                                       "Quarterly" = "quarter",
                                                       "Yearly" = "year")),
                               plotlyOutput("monthly_analysis_plot")),
                      tabPanel("Dynamic Scatter Plot",
                               plotlyOutput("dynamic_scatter_plot"))
                    )
                  )
                )
              )
      )
    )
  )
)

# **📌 Shiny Server**
server <- function(input, output, session) {
  selected_data <- reactiveVal(NULL)
  lda_results <- reactiveVal(NULL)
  current_stopwords <- reactiveVal(default_stopwords)  # New: Manage current stopwords
  
  # Define visualization update logic as a function
  update_market_visualizations <- function(market_data) {
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
        value = paste("$", format(sum(market_data$tender_value, na.rm = TRUE), big.mark = ",")),
        subtitle = HTML("<span style='font-size: 16px; font-weight: bold;'>Total Value</span>"),
        icon = icon("dollar-sign"),
        color = "green",
        width = 4
      )
    })
    
    output$avg_value <- renderValueBox({
      valueBox(
        value = paste("$", format(mean(market_data$tender_value, na.rm = TRUE), big.mark = ",")),
        subtitle = HTML("<span style='font-size: 16px; font-weight: bold;'>Average Value</span>"),
        icon = icon("chart-line"),
        color = "purple",
        width = 4
      )
    })
    
    # Market Trend Plot
    output$market_trend_plot <- renderPlotly({
      # Calculate data for each half year
      trend_data <- market_data %>%
        mutate(
          half_year = floor_date(tender_date, "6 months"),
          category = case_when(
            grepl("General Procurement", LDA_Category) ~ "General Procurement",
            grepl("Engineering Procurement", LDA_Category) ~ "Engineering Procurement",
            grepl("PPP Procurement", LDA_Category) ~ "PPP Procurement",
            TRUE ~ "Other"
          )
        ) %>%
        group_by(half_year, category) %>%
        summarise(
          count = n(),
          total_value = sum(tender_value, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Calculate average amount for each half year
      avg_data <- market_data %>%
        mutate(half_year = floor_date(tender_date, "6 months")) %>%
        group_by(half_year) %>%
        summarise(avg_value = mean(tender_value, na.rm = TRUE)) %>%
        ungroup()
      
      # Create stacked bar chart
      p1 <- ggplot(trend_data, aes(x = half_year, y = count, fill = category,
                                   text = paste("Date:", format(half_year, "%Y-%m"),
                                                "<br>Category:", category,
                                                "<br>Number of Tenders:", count))) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
        scale_fill_manual(values = c(
          "General Procurement" = "#FF6B6B",
          "Engineering Procurement" = "#4ECDC4",
          "PPP Procurement" = "#45B7D1",
          "Other" = "#96CEB4"
        )) +
        scale_x_date(
          date_breaks = "6 months",
          date_labels = "%Y-%m",
          expand = c(0.05, 0.05)
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(title = "Number of Tenders by Category and Half-Year",
             x = "Half Year",
             y = "Number of Tenders",
             fill = "Category")
      
      # Create average amount line chart
      p2 <- ggplot(avg_data, aes(x = half_year, y = avg_value/1000,
                                 text = paste("Date:", format(half_year, "%Y-%m"),
                                              "<br>Average Amount:", sprintf("$%.2fK", avg_value/1000)))) +
        geom_line(color = "red", size = 1, alpha = 0.8) +
        geom_point(color = "red", size = 3, alpha = 0.8) +
        scale_x_date(
          date_breaks = "6 months",
          date_labels = "%Y-%m",
          expand = c(0.05, 0.05)
        ) +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = cut_short_scale())
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(title = "Average Tender Value / Tender Amount by Half Year",
             x = "Half Year",
             y = "Average Amount (K)")
      
      # Combine two charts using subplot
      subplot(
        ggplotly(p1, tooltip = "text") %>%
          layout(
            hoverlabel = list(bgcolor = "white"),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              yanchor = "bottom",
              y = -0.3,
              xanchor = "center",
              x = 0.5
            ),
            margin = list(b = 100)
          ),
        ggplotly(p2, tooltip = "text") %>%
          layout(
            hoverlabel = list(bgcolor = "white"),
            margin = list(b = 50)
          ),
        nrows = 2,
        heights = c(0.6, 0.4),
        shareX = TRUE
      )
    })
    
    # Monthly Analysis Plot
    output$monthly_analysis_plot <- renderPlotly({
      req(input$time_unit)
      
      # Group by selected time unit
      time_data <- market_data %>%
        mutate(
          time_point = case_when(
            input$time_unit == "month" ~ floor_date(tender_date, "month"),
            input$time_unit == "quarter" ~ floor_date(tender_date, "quarter"),
            input$time_unit == "year" ~ floor_date(tender_date, "year")
          )
        ) %>%
        group_by(time_point) %>%
        summarise(
          count = n(),
          total_value = sum(tender_value, na.rm = TRUE)
        )
      
      # Set date format and interval based on time unit
      date_format <- case_when(
        input$time_unit == "month" ~ "%Y-%m",
        input$time_unit == "quarter" ~ "%Y-Q%q",
        input$time_unit == "year" ~ "%Y"
      )
      
      date_breaks <- case_when(
        input$time_unit == "month" ~ "3 months",
        input$time_unit == "quarter" ~ "3 months",
        input$time_unit == "year" ~ "1 year"
      )
      
      # Set title
      plot_title <- case_when(
        input$time_unit == "month" ~ "Monthly Analysis",
        input$time_unit == "quarter" ~ "Quarterly Analysis",
        input$time_unit == "year" ~ "Yearly Analysis"
      )
      
      # Calculate Y-axis range
      y_max <- max(time_data$count)
      y_breaks <- pretty(c(0, y_max), n = 5)  # Automatically calculate appropriate scales
      
      p <- ggplot(time_data, aes(x = time_point,
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
        scale_x_date(
          date_breaks = date_breaks,
          date_labels = date_format,
          expand = c(0.05, 0.05)
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(title = plot_title,
             x = case_when(
               input$time_unit == "month" ~ "Month",
               input$time_unit == "quarter" ~ "Quarter",
               input$time_unit == "year" ~ "Year"
             ))
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white"),
          margin = list(b = 50),
          yaxis = list(
            range = c(0, y_max * 1.1)  # Set Y-axis range with some space
          )
        )
    })
    
    # Dynamic Scatter Plot
    output$dynamic_scatter_plot <- renderPlotly({
      scatter_data <- market_data %>%
        mutate(
          time_point = floor_date(tender_date, "6 months"),
          Year = year(time_point),
          Month = month(time_point)
        ) %>%
        group_by(time_point, Year, Month, LDA_Category) %>%
        summarise(
          tender_count = n(),
          total_amount = sum(tender_value, na.rm = TRUE)
        ) %>%
        group_by(time_point) %>%
        mutate(
          total_spending = sum(total_amount, na.rm = TRUE),
          spending_ratio = (total_amount / total_spending) * 100
        ) %>%
        ungroup() %>%
        arrange(time_point)
      
      p <- ggplot(scatter_data, aes(x = tender_count, 
                                    y = spending_ratio,
                                    size = tender_count,
                                    color = LDA_Category,
                                    frame = time_point,
                                    text = paste("Date:", format(time_point, "%Y-%m"),
                                                 "<br>Category:", LDA_Category,
                                                 "<br>Count:", tender_count,
                                                 "<br>Spending Ratio:", sprintf("%.1f%%", spending_ratio)))) +
        geom_point(alpha = 0.7) +
        scale_size(range = c(2, 8)) +  # Adjust point size range
        scale_color_brewer(palette = "Dark2") +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          legend.position = "right",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 8)
        ) +
        labs(title = "Tender Count vs Spending Ratio by Category",
             x = "Number of Tenders",
             y = "Spending Ratio (%)",
             color = "Category")
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 10)
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            yanchor = "top",
            y = 0.8,
            xanchor = "left",
            x = 1.05,
            font = list(size = 10)
          ),
          margin = list(r = 100),  # Increase right margin
          title = list(
            font = list(size = 12),
            y = 0.95
          )
        ) %>%
        animation_opts(
          frame = 800,
          transition = 600,
          easing = "linear",
          redraw = TRUE
        ) %>%
        animation_button(
          x = 1,
          xanchor = "right",
          y = 0,
          yanchor = "bottom"
        )
    })
  }
  
  # Display Stopwords table
  output$stopwords_table <- renderDT({
    datatable(
      data.frame(stopword = current_stopwords()),
      selection = "multiple",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "200px"
      )
    )
  })
  
  # Add new Stopword
  observeEvent(input$add_stopword, {
    new_word <- trimws(input$new_stopword)
    if (new_word != "") {
      current_stopwords(c(current_stopwords(), new_word))
      updateTextInput(session, "new_stopword", value = "")
      
      # If LDA results exist, reprocess data
      if (!is.null(selected_data())) {
        sample_data <- selected_data()
        sample_data <- sample_data %>%
          mutate(
            tender_clean = tender_description %>%
              tolower() %>%
              removePunctuation() %>%
              removeNumbers() %>%
              stripWhitespace() %>%
              removeWords(current_stopwords())
          )
        selected_data(sample_data)
        
        # Re-run LDA analysis
        if (!is.null(lda_results())) {
          # Rebuild Document-Term Matrix
          dtm <- sample_data %>%
            unnest_tokens(word, tender_clean) %>%
            count(tender_no, word) %>%
            cast_dtm(document = tender_no, term = word, value = n)
          
          # Retrain LDA model
          lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
          
          # Get new LDA classification results
          lda_assignments <- tidy(lda_model, matrix = "gamma")
          
          sample_data <- sample_data %>%
            left_join(lda_assignments %>% group_by(document) %>% top_n(1, gamma),
                      by = c("tender_no" = "document")) %>%
            mutate(
              LDA_Category = case_when(
                topic == 1 ~ "General Procurement - Goods",
                topic == 2 ~ "General Procurement - Services",
                topic == 3 ~ "Engineering Procurement - Goods",
                topic == 4 ~ "Engineering Procurement - Services",
                topic == 5 ~ "Engineering Procurement - EPC",
                topic == 6 ~ "PPP Procurement - DBO",
                topic == 7 ~ "PPP Procurement - DBFO",
                TRUE ~ "Unclassified"
              )
            )
          
          # Update LDA results
          lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
          
          # Trigger reload
          updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          shinyjs::click("reload_lda")
        }
      }
    }
  })
  
  # Remove selected Stopwords
  observeEvent(input$remove_stopword, {
    selected_rows <- input$stopwords_table_rows_selected
    if (!is.null(selected_rows)) {
      current_stopwords(current_stopwords()[-selected_rows])
      
      # If LDA results exist, reprocess data
      if (!is.null(selected_data())) {
        sample_data <- selected_data()
        sample_data <- sample_data %>%
          mutate(
            tender_clean = tender_description %>%
              tolower() %>%
              removePunctuation() %>%
              removeNumbers() %>%
              stripWhitespace() %>%
              removeWords(current_stopwords())
          )
        selected_data(sample_data)
        
        # Re-run LDA analysis
        if (!is.null(lda_results())) {
          # Rebuild Document-Term Matrix
          dtm <- sample_data %>%
            unnest_tokens(word, tender_clean) %>%
            count(tender_no, word) %>%
            cast_dtm(document = tender_no, term = word, value = n)
          
          # Retrain LDA model
          lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
          
          # Get new LDA classification results
          lda_assignments <- tidy(lda_model, matrix = "gamma")
          
          sample_data <- sample_data %>%
            left_join(lda_assignments %>% group_by(document) %>% top_n(1, gamma),
                      by = c("tender_no" = "document")) %>%
            mutate(
              LDA_Category = case_when(
                topic == 1 ~ "General Procurement - Goods",
                topic == 2 ~ "General Procurement - Services",
                topic == 3 ~ "Engineering Procurement - Goods",
                topic == 4 ~ "Engineering Procurement - Services",
                topic == 5 ~ "Engineering Procurement - EPC",
                topic == 6 ~ "PPP Procurement - DBO",
                topic == 7 ~ "PPP Procurement - DBFO",
                TRUE ~ "Unclassified"
              )
            )
          
          # Update LDA results
          lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
          
          # Trigger reload
          updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          shinyjs::click("reload_lda")
        }
      }
    }
  })
  
  # Reset to default Stopwords
  observeEvent(input$reset_stopwords, {
    current_stopwords(default_stopwords)
    
    # If LDA results exist, reprocess data
    if (!is.null(selected_data())) {
      sample_data <- selected_data()
      sample_data <- sample_data %>%
        mutate(
          tender_clean = tender_description %>%
            tolower() %>%
            removePunctuation() %>%
            removeNumbers() %>%
            stripWhitespace() %>%
            removeWords(current_stopwords())
        )
      selected_data(sample_data)
      
      # Re-run LDA analysis
      if (!is.null(lda_results())) {
        # Rebuild Document-Term Matrix
        dtm <- sample_data %>%
          unnest_tokens(word, tender_clean) %>%
          count(tender_no, word) %>%
          cast_dtm(document = tender_no, term = word, value = n)
        
        # Retrain LDA model
        lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
        
        # Get new LDA classification results
        lda_assignments <- tidy(lda_model, matrix = "gamma")
        
        sample_data <- sample_data %>%
          left_join(lda_assignments %>% group_by(document) %>% top_n(1, gamma),
                    by = c("tender_no" = "document")) %>%
          mutate(
            LDA_Category = case_when(
              topic == 1 ~ "General Procurement - Goods",
              topic == 2 ~ "General Procurement - Services",
              topic == 3 ~ "Engineering Procurement - Goods",
              topic == 4 ~ "Engineering Procurement - Services",
              topic == 5 ~ "Engineering Procurement - EPC",
              topic == 6 ~ "PPP Procurement - DBO",
              topic == 7 ~ "PPP Procurement - DBFO",
              TRUE ~ "Unclassified"
            )
          )
        
        # Update LDA results
        lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
        
        # Trigger reload
        updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
        shinyjs::click("reload_lda")
      }
    }
  })
  
  # Modify data cleaning part, use current_stopwords
  observe({
    sample_size <- 1000
    set.seed(1234)
    sample_data <- Cleaned_GP_LDA %>% sample_n(sample_size)
    
    sample_data <- sample_data %>%
      mutate(
        tender_clean = tender_description %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(current_stopwords())  # Use current_stopwords
      )
    
    selected_data(sample_data)
    
    output$data_summary <- renderPrint({
      paste("Loaded default", sample_size, "records.")
    })
  })
  
  # Modify manual data size selection part
  observeEvent(input$load_data, {
    sample_size <- as.numeric(input$sample_size)
    set.seed(1234)
    sample_data <- Cleaned_GP_LDA %>% sample_n(sample_size)
    
    sample_data <- sample_data %>%
      mutate(
        tender_clean = tender_description %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(current_stopwords())  # Use current_stopwords
      )
    
    selected_data(sample_data)
    
    output$data_summary <- renderPrint({
      paste("Loaded", sample_size, "records.")
    })
  })
  
  # **📌 LDA Supervised Learning**
  
  observeEvent(input$run_supervised, {
    req(selected_data())
    
    sample_data <- selected_data()
    
    # **📌 Create Document-Term Matrix**
    dtm <- sample_data %>%
      unnest_tokens(word, tender_clean) %>%
      count(tender_no, word) %>%
      cast_dtm(document = tender_no, term = word, value = n)
    
    # **📌 Train LDA Model**
    lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
    
    # **📌 Get LDA Classification Results**
    lda_assignments <- tidy(lda_model, matrix = "gamma")
    
    sample_data <- sample_data %>%
      left_join(lda_assignments %>% group_by(document) %>% top_n(1, gamma),
                by = c("tender_no" = "document")) %>%
      mutate(
        LDA_Category = case_when(
          topic == 1 ~ "General Procurement - Goods",
          topic == 2 ~ "General Procurement - Services",
          topic == 3 ~ "Engineering Procurement - Goods",
          topic == 4 ~ "Engineering Procurement - Services",
          topic == 5 ~ "Engineering Procurement - EPC",
          topic == 6 ~ "PPP Procurement - DBO",
          topic == 7 ~ "PPP Procurement - DBFO",
          TRUE ~ "Unclassified"
        )
      )
    
    # Save LDA results with necessary columns
    lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
    
    updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
  })
  
  observeEvent(input$reload_lda, {
    req(lda_results())
    
    # Reprocess text using current stopwords
    processed_data <- lda_results() %>%
      mutate(
        tender_clean = tender_clean %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(current_stopwords())
      )
    
    # ✅ **TF-IDF Calculation, ensure `filtered_data()` is not empty**
    word_tf_idf <- processed_data %>%
      unnest_tokens(word, tender_clean) %>%
      count(LDA_Category, tender_no, word) %>%
      bind_tf_idf(word, LDA_Category, n) %>%
      filter(tf_idf > quantile(tf_idf, 0.25) & tf_idf < quantile(tf_idf, 0.95)) %>%
      mutate(tf_idf = ifelse(tf_idf < 0, 0, tf_idf))
    
    filtered_data <- reactive({
      if (input$lda_category == "All") {
        word_tf_idf
      } else {
        word_tf_idf %>% filter(LDA_Category == input$lda_category)
      }
    })
    
    # ✅ **LDA Distribution**
    output$lda_category_plot <- renderPlotly({
      lda_counts <- lda_results() %>%
        count(LDA_Category) %>%
        arrange(desc(n))
      
      p <- ggplot(lda_counts, aes(x = reorder(LDA_Category, n), y = n, fill = LDA_Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(title = "LDA Distribution", x = "LDA Category", y = "Number of Tenders")
      
      ggplotly(p) %>%
        layout(showlegend = FALSE)
    })
    
    # ✅ **TF-IDF Table**
    output$tfidf_table <- renderDT({
      req(filtered_data())  
      datatable(
        filtered_data() %>% arrange(desc(tf_idf)), 
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "500px",
          autoWidth = TRUE,
          fixedHeader = TRUE
        ), 
        rownames = FALSE
      )
    })
    
    # ✅ **Wordcloud**
    output$wordcloud <- renderPlot({
      req(filtered_data())
      data <- filtered_data() %>% slice_head(n = input$num_words)
      color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(input$num_words)
      with(data, wordcloud(word, tf_idf, max.words = input$num_words, random.order = FALSE, colors = color_palette))
    })
    
    # ✅ **TF-IDF Bar Plot**
    output$tfidf_plot <- renderPlotly({
      req(filtered_data())
      
      data <- filtered_data() %>%
        filter(tf_idf > 0) %>%
        slice_head(n = input$num_words)
      
      p <- ggplot(data, aes(x = reorder(word, tf_idf), y = tf_idf, fill = LDA_Category, text = paste0(
        "Word: ", word, "<br>",
        "TF-IDF: ", round(tf_idf, 6), "<br>",
        "Category: ", LDA_Category
      ))) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Top TF-IDF Words", x = "Word", y = "TF-IDF Score")
      
      ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "lightblue"), showlegend = FALSE)
    })
  })
  
  
  # **📌 LDA Clustering (Unsupervised)**
  observeEvent(input$run_unsupervised, {
    req(selected_data())
    
    sample_data <- selected_data()
    
    dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_data$tender_clean)))
    lda_model <- LDA(dtm, k = input$num_topics, control = list(seed = 1234))  # Use selected topic count
    
    doc_topic_matrix <- posterior(lda_model)$topics %>%
      as.data.frame() %>%
      mutate(document = seq_len(nrow(.)))
    
    # **📌 Calculate Top TF-IDF Words within Topics**
    word_topic_tfidf <- tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      summarise(top_words = paste(term, collapse = ", "))
    
    num_clusters <- input$num_clusters
    if (nrow(doc_topic_matrix) < num_clusters) {
      showNotification("K cannot be larger than available documents.", type = "error")
      return(NULL)
    }
    
    set.seed(1234)
    kmeans_result <- kmeans(doc_topic_matrix[,-ncol(doc_topic_matrix)], centers = num_clusters)
    
    clustered_matrix <- doc_topic_matrix
    clustered_matrix$cluster <- factor(kmeans_result$cluster)
    
    doc_topic_melted <- melt(clustered_matrix, id.vars = c("document", "cluster"), 
                             variable.name = "Topic", value.name = "Probability")
    
    # **📌 Add Top TF-IDF Words**
    doc_topic_melted <- doc_topic_melted %>%
      mutate(topic = as.numeric(gsub("V", "", Topic))) %>%
      left_join(word_topic_tfidf, by = c("topic" = "topic"))
    
    # **📌 Update Cluster Selection**
    updateSelectInput(session, "select_cluster", choices = c("All", unique(doc_topic_melted$cluster)))
    
    # **📌 Plot All Clusters**
    p_all <- ggplot(doc_topic_melted, aes(x = Topic, y = Probability, group = document, color = cluster,
                                          text = paste("Topic:", topic, "<br>",
                                                       "Top Words:", top_words, "<br>",
                                                       "Probability:", round(Probability, 4), "<br>",
                                                       "Document:", document, "<br>",
                                                       "Cluster:", cluster))) +
      geom_line(alpha = 0.1, size = 0.3) +
      geom_point(size = 1, alpha = 0.5) +
      facet_wrap(~ cluster, scales = "free_y") +
      scale_color_manual(values = RColorBrewer::brewer.pal(10, "Set3")) +
      theme_minimal() +
      labs(title = paste("Topic Clustering with", num_clusters, "Clusters and", input$num_topics, "Topics"),
           x = "Topic", y = "Probability")
    
    output$cluster_plot <- renderPlotly({
      ggplotly(p_all, tooltip = "text")
    })
    
    ###Check
    observeEvent(input$run_unsupervised, {
      req(selected_data())  
      
      print("Checking selected_data()...")
      print(head(selected_data()))  # Check first few rows of data
      
      print("Checking tender_clean column...")
      print(sum(nchar(selected_data()$tender_clean) == 0))  # Calculate if tender_clean has empty values
    })
    
    # **📌 Plot Single Cluster**
    observeEvent(input$select_cluster, {
      req(input$select_cluster)
      
      if (input$select_cluster == "All") {
        output$single_cluster_plot <- renderPlotly(NULL)
      } else {
        filtered_data <- doc_topic_melted %>% filter(cluster == input$select_cluster)
        
        p_single <- ggplot(filtered_data, aes(x = Topic, y = Probability, group = document, color = cluster,
                                              text = paste("Topic:", topic, "<br>",
                                                           "Top Words:", top_words, "<br>",
                                                           "Probability:", round(Probability, 4), "<br>",
                                                           "Document:", document, "<br>",
                                                           "Cluster:", cluster))) +
          geom_line(alpha = 0.1, size = 0.3) +
          geom_point(size = 1, alpha = 0.5) +
          facet_wrap(~ cluster, scales = "free_y") +
          scale_color_manual(values = RColorBrewer::brewer.pal(10, "Set3")) +
          theme_minimal() +
          labs(title = paste("Cluster", input$select_cluster, "Topic Distribution"),
               x = "Topic", y = "Probability")
        
        output$single_cluster_plot <- renderPlotly({
          ggplotly(p_single, tooltip = "text")
        })
      }
    })
  })
  
  # **📌 Market Analysis**
  # Initialize date selector
  output$date_slider <- renderUI({
    tryCatch({
      # Use preprocessed Cleaned_GP data to calculate date range
      market_data <- Cleaned_GP %>%
        filter(!is.na(tender_date))
      
      if (nrow(market_data) > 0) {
        min_date <- floor_date(min(market_data$tender_date), "month")
        max_date <- ceiling_date(max(market_data$tender_date), "month")
        
        dateRangeInput("date_range",
                       "Select Date Range:",
                       start = min_date,
                       end = max_date,
                       min = min_date,
                       max = max_date,
                       format = "yyyy-mm",
                       startview = "month",
                       autoclose = TRUE,
                       language = "en",
                       separator = " to ",
                       width = "100%")
      } else {
        dateRangeInput("date_range",
                       "No valid dates available",
                       start = Sys.Date(),
                       end = Sys.Date(),
                       format = "yyyy-mm",
                       startview = "month",
                       autoclose = TRUE,
                       language = "en",
                       separator = " to ",
                       width = "100%")
      }
    }, error = function(e) {
      dateRangeInput("date_range",
                     "Error loading dates",
                     start = Sys.Date(),
                     end = Sys.Date(),
                     format = "yyyy-mm",
                     startview = "month",
                     autoclose = TRUE,
                     language = "en",
                     separator = " to ",
                     width = "100%")
    })
  })
  
  observeEvent(input$run_market_analysis, {
    tryCatch({
      # Check if LDA has been run
      if (is.null(lda_results())) {
        showNotification(
          "Please run LDA analysis first",
          type = "warning"
        )
        return(NULL)
      }
      
      # Check Cleaned_GP data
      if (is.null(Cleaned_GP) || nrow(Cleaned_GP) == 0) {
        showNotification(
          "Unable to load tender data, please check if the data file is correct",
          type = "error"
        )
        return(NULL)
      }
      
      # Use preprocessed data
      market_data <- Cleaned_GP %>%
        left_join(lda_results(), by = "tender_no") %>%
        mutate(
          tender_date = as.Date(award_date, format = "%d/%m/%Y"),
          tender_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
        )
      
      # Check required columns
      required_columns <- c("award_date", "awarded_amt", "tender_detail_status", "LDA_Category")
      missing_columns <- required_columns[!required_columns %in% names(market_data)]
      
      if (length(missing_columns) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
      
      # Filter NA values
      market_data <- market_data %>%
        filter(
          !is.na(tender_date),
          !is.na(tender_value),
          !is.na(LDA_Category)
        )
      
      if (nrow(market_data) == 0) {
        showNotification(
          "No valid data after filtering",
          type = "warning"
        )
        return(NULL)
      }
      
      # Remove outliers based on checkbox
      if (input$remove_outliers) {
        market_data <- market_data %>%
          group_by(LDA_Category) %>%
          mutate(
            Q1 = quantile(tender_value, 0.25, na.rm = TRUE),
            Q3 = quantile(tender_value, 0.75, na.rm = TRUE),
            IQR = Q3 - Q1,
            lower_bound = Q1 - 1.5 * IQR,
            upper_bound = Q3 + 1.5 * IQR
          ) %>%
          filter(
            tender_value >= lower_bound,
            tender_value <= upper_bound
          ) %>%
          select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)
      }
      
      # Use selected date range
      if (!is.null(input$date_range)) {
        market_data <- market_data %>%
          filter(
            tender_date >= input$date_range[1],
            tender_date <= input$date_range[2]
          )
      }
      
      # Filter category and tender status
      if (input$market_category != "All") {
        market_data <- market_data %>%
          filter(grepl(input$market_category, LDA_Category))
      }
      
      if (input$tender_status != "All") {
        market_data <- market_data %>%
          filter(tender_detail_status == input$tender_status)
      }
      
      if (nrow(market_data) == 0) {
        showNotification(
          "No data matches the selected filter criteria",
          type = "warning"
        )
        return(NULL)
      }
      
      # Update visualizations
      update_market_visualizations(market_data)
      
    }, error = function(e) {
      showNotification(
        paste("Market analysis error:", e$message),
        type = "error"
      )
    })
  })
}

shinyApp(ui, server)