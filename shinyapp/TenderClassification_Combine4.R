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



# **📌 讀取 Cleaned_GP_LDA 數據**
Cleaned_GP_LDA <- read_csv("data/Cleaned_GP_LDA.csv")
Cleaned_GP <- read_csv("data/Cleaned_GP.csv")  # 新增讀取完整資料

# **📌 預設 Stopwords**
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
                    sliderInput("num_clusters", "Number of Clusters:", min = 2, max = 10, value = 5, step = 1),
                    actionButton("run_unsupervised", "Run Clustering")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("All Clusters", plotlyOutput("cluster_plot")),
                      tabPanel("Single Cluster View", 
                               selectInput("select_cluster", "Select Cluster:", choices = NULL),
                               plotlyOutput("single_cluster_plot"))
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
                                          "Award by interface record",
                                          "Awarded by Items",
                                          "Awarded to Suppliers")),
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
                      tabPanel("Category Distribution",
                               plotlyOutput("category_dist_plot")),
                      tabPanel("Top Agencies",
                               DTOutput("top_agencies_table")),
                      tabPanel("Monthly Analysis",
                               plotlyOutput("monthly_analysis_plot"))
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
  current_stopwords <- reactiveVal(default_stopwords)  # 新增：管理當前 stopwords
  
  # 顯示 Stopwords 表格
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
  
  # 新增 Stopword
  observeEvent(input$add_stopword, {
    new_word <- trimws(input$new_stopword)
    if (new_word != "") {
      current_stopwords(c(current_stopwords(), new_word))
      updateTextInput(session, "new_stopword", value = "")
      
      # 如果已經有 LDA 結果，重新處理資料
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
        
        # 重新執行 LDA 分析
        if (!is.null(lda_results())) {
          # 重新建立 Document-Term Matrix
          dtm <- sample_data %>%
            unnest_tokens(word, tender_clean) %>%
            count(tender_no, word) %>%
            cast_dtm(document = tender_no, term = word, value = n)
          
          # 重新訓練 LDA 模型
          lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
          
          # 取得新的 LDA 分類結果
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
          
          # 更新 LDA 結果
          lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
          
          # 觸發重新載入
          updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          shinyjs::click("reload_lda")
        }
      }
    }
  })
  
  # 移除選中的 Stopwords
  observeEvent(input$remove_stopword, {
    selected_rows <- input$stopwords_table_rows_selected
    if (!is.null(selected_rows)) {
      current_stopwords(current_stopwords()[-selected_rows])
      
      # 如果已經有 LDA 結果，重新處理資料
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
        
        # 重新執行 LDA 分析
        if (!is.null(lda_results())) {
          # 重新建立 Document-Term Matrix
          dtm <- sample_data %>%
            unnest_tokens(word, tender_clean) %>%
            count(tender_no, word) %>%
            cast_dtm(document = tender_no, term = word, value = n)
          
          # 重新訓練 LDA 模型
          lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
          
          # 取得新的 LDA 分類結果
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
          
          # 更新 LDA 結果
          lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
          
          # 觸發重新載入
          updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          shinyjs::click("reload_lda")
        }
      }
    }
  })
  
  # 重置為預設 Stopwords
  observeEvent(input$reset_stopwords, {
    current_stopwords(default_stopwords)
    
    # 如果已經有 LDA 結果，重新處理資料
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
      
      # 重新執行 LDA 分析
      if (!is.null(lda_results())) {
        # 重新建立 Document-Term Matrix
        dtm <- sample_data %>%
          unnest_tokens(word, tender_clean) %>%
          count(tender_no, word) %>%
          cast_dtm(document = tender_no, term = word, value = n)
        
        # 重新訓練 LDA 模型
        lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
        
        # 取得新的 LDA 分類結果
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
        
        # 更新 LDA 結果
        lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
        
        # 觸發重新載入
        updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
        shinyjs::click("reload_lda")
      }
    }
  })
  
  # 修改資料清理部分，使用 current_stopwords
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
          removeWords(current_stopwords())  # 使用 current_stopwords
      )
    
    selected_data(sample_data)
    
    output$data_summary <- renderPrint({
      paste("Loaded default", sample_size, "records.")
    })
  })
  
  # 修改手動選擇數據量部分
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
          removeWords(current_stopwords())  # 使用 current_stopwords
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
    
    # **📌 建立 Document-Term Matrix**
    dtm <- sample_data %>%
      unnest_tokens(word, tender_clean) %>%
      count(tender_no, word) %>%
      cast_dtm(document = tender_no, term = word, value = n)
    
    # **📌 訓練 LDA 模型**
    lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
    
    # **📌 取得 LDA 分類結果**
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
    
    # 儲存 LDA 結果，包含必要的欄位
    lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
    
    updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
  })
  
  observeEvent(input$reload_lda, {
    req(lda_results())
    
    # 重新處理文本，使用當前的 stopwords
    processed_data <- lda_results() %>%
      mutate(
        tender_clean = tender_clean %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(current_stopwords())
      )
    
    # ✅ **TF-IDF 計算，確保 `filtered_data()` 不為空**
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
    lda_model <- LDA(dtm, k = 10, control = list(seed = 1234))  # 固定 10 個 topic
    
    doc_topic_matrix <- posterior(lda_model)$topics %>%
      as.data.frame() %>%
      mutate(document = seq_len(nrow(.)))
    
    # **📌 計算 Topic 內的 Top TF-IDF Words**
    word_topic_tfidf <- tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      summarise(top_words = paste(term, collapse = ", "))  # ✅ 每個 Topic 取前 5 高 TF-IDF 詞
    
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
    
    # **📌 加入 Top TF-IDF Words**
    doc_topic_melted <- doc_topic_melted %>%
      mutate(topic = as.numeric(gsub("V", "", Topic))) %>%
      left_join(word_topic_tfidf, by = c("topic" = "topic"))
    
    # **📌 更新 Cluster 選擇**
    updateSelectInput(session, "select_cluster", choices = c("All", unique(doc_topic_melted$cluster)))
    
    # **📌 繪製所有 Clusters**
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
      labs(title = paste("Topic Clustering with", num_clusters, "Clusters"),
           x = "Topic", y = "Probability")
    
    output$cluster_plot <- renderPlotly({
      ggplotly(p_all, tooltip = "text")
    })
    
    ###檢查
    observeEvent(input$run_unsupervised, {
      req(selected_data())  
      
      print("Checking selected_data()...")
      print(head(selected_data()))  # 檢查前幾行數據
      
      print("Checking tender_clean column...")
      print(sum(nchar(selected_data()$tender_clean) == 0))  # 計算 tender_clean 是否有空值
    })
    
    # **📌 繪製單一 Cluster**
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
  # 初始化日期選擇器
  output$date_slider <- renderUI({
    tryCatch({
      # 使用完整的 Cleaned_GP 資料來計算日期範圍
      market_data <- Cleaned_GP %>%
        mutate(
          tender_date = as.Date(award_date, format = "%d/%m/%Y")
        ) %>%
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
      # 檢查是否已經執行過 LDA
      if (is.null(lda_results())) {
        showNotification(
          "Please run LDA Analysis first to get category information",
          type = "warning"
        )
        return(NULL)
      }
      
      # 使用完整的 Cleaned_GP 資料並與 LDA 結果合併
      market_data <- Cleaned_GP %>%
        left_join(lda_results(), by = "tender_no")
      
      # 檢查必要的欄位是否存在
      required_columns <- c("award_date", "awarded_amt", "agency", "LDA_Category")
      missing_columns <- required_columns[!required_columns %in% names(market_data)]
      
      if (length(missing_columns) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
      
      # 資料清理和轉換
      market_data <- market_data %>%
        mutate(
          tender_date = as.Date(award_date, format = "%d/%m/%Y"),
          tender_value = as.numeric(gsub("[^0-9.]", "", awarded_amt))
        )
      
      # 過濾 NA 值
      market_data <- market_data %>%
        filter(
          !is.na(tender_date),
          !is.na(tender_value)
        )
      
      if (nrow(market_data) == 0) {
        showNotification("No valid data available after filtering NA values", type = "warning")
        return(NULL)
      }
      
      # 使用選擇的日期範圍
      if (!is.null(input$date_range)) {
        market_data <- market_data %>%
          filter(
            tender_date >= input$date_range[1],
            tender_date <= input$date_range[2]
          )
      }
      
      # 過濾類別
      if (input$market_category != "All") {
        market_data <- market_data %>%
          filter(grepl(input$market_category, LDA_Category))
      }
      
      # 過濾標案狀態
      if (input$tender_status != "All") {
        market_data <- market_data %>%
          filter(tender_detail_status == input$tender_status)
      }
      
      if (nrow(market_data) == 0) {
        showNotification("No data available for the selected filters", type = "warning")
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
        trend_data <- market_data %>%
          group_by(tender_date) %>%
          summarise(
            count = n(),
            total_value = sum(tender_value, na.rm = TRUE)
          )
        
        p <- ggplot(trend_data, aes(x = tender_date)) +
          geom_line(aes(y = count, color = "Number of Tenders")) +
          geom_line(aes(y = total_value/1000, color = "Total Value (K)")) +
          scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Total Value")) +
          theme_minimal() +
          labs(title = "Market Trends Over Time",
               x = "Date",
               y = "Number of Tenders",
               color = "Metric")
        
        ggplotly(p)
      })
      
      # Category Distribution Plot
      output$category_dist_plot <- renderPlotly({
        category_data <- market_data %>%
          group_by(LDA_Category) %>%
          summarise(
            count = n(),
            total_value = sum(tender_value, na.rm = TRUE)
          )
        
        p <- ggplot(category_data, aes(x = reorder(LDA_Category, count), y = count)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          theme_minimal() +
          labs(title = "Distribution by Category",
               x = "Category",
               y = "Number of Tenders")
        
        ggplotly(p)
      })
      
      # Top Agencies Table
      output$top_agencies_table <- renderDT({
        agency_data <- market_data %>%
          group_by(agency) %>%
          summarise(
            total_tenders = n(),
            total_value = sum(tender_value, na.rm = TRUE),
            avg_value = mean(tender_value, na.rm = TRUE)
          ) %>%
          arrange(desc(total_value))
        
        datatable(agency_data,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE
                  ),
                  colnames = c("Agency", "Total Tenders", "Total Value", "Average Value"))
      })
      
      # Monthly Analysis Plot
      output$monthly_analysis_plot <- renderPlotly({
        monthly_data <- market_data %>%
          mutate(
            month = floor_date(tender_date, "month")
          ) %>%
          group_by(month) %>%
          summarise(
            count = n(),
            total_value = sum(tender_value, na.rm = TRUE)
          )
        
        p <- ggplot(monthly_data, aes(x = month)) +
          geom_bar(aes(y = count), stat = "identity", fill = "steelblue") +
          geom_line(aes(y = total_value/1000), color = "red") +
          scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Total Value")) +
          theme_minimal() +
          labs(title = "Monthly Analysis",
               x = "Month",
               y = "Number of Tenders")
        
        ggplotly(p)
      })
    }, error = function(e) {
      showNotification(paste("Error in market analysis:", e$message), type = "error")
    })
  })
}


shinyApp(ui, server)