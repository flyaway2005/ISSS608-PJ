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

# **📌 讀取 Cleaned_GP 數據**
Cleaned_GP <- read_csv("data/GovernmentProcurementviaGeBIZ.csv")

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
      menuItem("LDA Clustering", tabName = "unsupervised", icon = icon("chart-line"))  # ✅ 新增 Unsupervised 分頁
    )
  ),
  dashboardBody(
    tabItems(
      # **📌 選擇數據量**
      tabItem(tabName = "data_selection",
              fluidPage(
                titlePanel("Select Data Sample Size"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("sample_size", "Choose Data Sample:", 
                                choices = c("1000" = 1000, "5000" = 5000, "10000" = 10000, "All" = nrow(Cleaned_GP)),
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
                    actionButton("reload_lda", "Reload")  # ✅ 新增 Reload 按鈕
                  )
                  ,
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
                    plotlyOutput("cluster_plot")
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
  
  # **📌 預設載入 1000 筆數據**
  observe({
    sample_size <- 1000
    set.seed(1234)
    sample_data <- Cleaned_GP %>% sample_n(sample_size)
    
    sample_data <- sample_data %>%
      mutate(
        tender_clean = tender_description %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(default_stopwords)
      )
    
    selected_data(sample_data)
    
    output$data_summary <- renderPrint({
      paste("Loaded default", sample_size, "records.")
    })
  })
  
  # **📌 手動選擇數據量**
  observeEvent(input$load_data, {
    sample_size <- as.numeric(input$sample_size)
    set.seed(1234)
    sample_data <- Cleaned_GP %>% sample_n(sample_size)
    
    sample_data <- sample_data %>%
      mutate(
        tender_clean = tender_description %>%
          tolower() %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace() %>%
          removeWords(default_stopwords)
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
    
    updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
    
    # ✅ **存儲計算結果，等 Reload 按鈕被點擊後才更新圖表**
    lda_results <<- sample_data
  })
  
  observeEvent(input$reload_lda, {
    req(lda_results)
    
    # ✅ **TF-IDF 計算，確保 `filtered_data()` 不為空**
    word_tf_idf <- lda_results %>%
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
      lda_counts <- lda_results %>%
        count(LDA_Category) %>%
        arrange(desc(n))
      
      p <- ggplot(lda_counts, aes(x = reorder(LDA_Category, n), y = n, fill = LDA_Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(title = "LDA Distribution", x = "LDA Category", y = "Number of Tenders")
      
      ggplotly(p) %>%
        layout(showlegend = FALSE)  # ✅ 隱藏 Legend
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
      mutate(topic = as.numeric(gsub("V", "", Topic))) %>%  # 轉換 Topic 為數字
      left_join(word_topic_tfidf, by = c("topic" = "topic"))  # ✅ 加入對應的高 TF-IDF 詞
    
    # **📌 更新 Plot**
    p <- ggplot(doc_topic_melted, aes(x = Topic, y = Probability, group = document, color = cluster,
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
      ggplotly(p, tooltip = "text")  # ✅ 讓 Tooltip 顯示 "text" 內的資訊
    })
  })
  
  # **📌 LDA Clustering (Unsupervised)**
  observeEvent(input$run_unsupervised, {
    req(selected_data())

    sample_data <- selected_data()

    dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_data$tender_clean)))
    lda_model <- LDA(dtm, k = 10, control = list(seed = 1234))  

    doc_topic_matrix <- posterior(lda_model)$topics %>%
      as.data.frame() %>%
      mutate(document = seq_len(nrow(.)))

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

    doc_topic_melted <- doc_topic_melted %>%
      mutate(topic = as.numeric(gsub("V", "", Topic))) %>%
      left_join(word_topic_tfidf, by = c("topic" = "topic"))  

    p <- ggplot(doc_topic_melted, aes(x = Topic, y = Probability, group = document, color = cluster,
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
      ggplotly(p, tooltip = "text")  
    })
  })
}

shinyApp(ui, server)
