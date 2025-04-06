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

# **📌 擴充 Stopwords**
extra_stopwords <- c("please", "refer", "another", "one", "two", "three", "framework", "edition", 
                     "related", "whole", "period", "government", "entities", "various", "including",
                     "requirement", "provide", "supply", "service", "procurement", "year", "option", 
                     "term", "extend", "agreement")
custom_stopwords <- c("singapore", "services", "tender", "provision", "works", "contract", 
                      "supply", "invitation", "appointment", "board", "national", "years", "delivery")
all_stopwords <- c(stopwords("en"), custom_stopwords, extra_stopwords)

# **📌 Shiny UI**
ui <- dashboardPage(
  dashboardHeader(title = "LDA Classification"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Selection", tabName = "data_selection", icon = icon("database")),
      menuItem("LDA Supervised Learning", tabName = "supervised", icon = icon("brain"))
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
                                selected = 1000),  # 預設為 1000
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
                    selectInput("lda_category", "Select LDA Category:", choices = NULL, selected = "All") # **LDA分類選單**
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("LDA Distribution", plotOutput("lda_category_plot")),
                      tabPanel("TF-IDF Table", DTOutput("tfidf_table")),
                      tabPanel("Wordcloud", plotOutput("wordcloud")),
                      
                      tabPanel("TF-IDF Bar Plot", plotlyOutput("tfidf_plot")),
                      
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
  # **📌 動態數據抽樣**
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
          removeWords(all_stopwords)
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
          removeWords(all_stopwords)
      )
    
    selected_data(sample_data)
    
    output$data_summary <- renderPrint({
      paste("Loaded", sample_size, "records.")
    })
  })
  
  # **📌 LDA 監督學習**
  observeEvent(input$run_supervised, {
    req(selected_data())
    
    sample_data <- selected_data()
    
    dtm <- sample_data %>%
      unnest_tokens(word, tender_clean) %>%
      count(tender_no, word) %>%
      cast_dtm(document = tender_no, term = word, value = n)
    
    lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))
    
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
    
    output$lda_category_plot <- renderPlot({
      lda_counts <- sample_data %>%
        count(LDA_Category) %>%
        arrange(desc(n))  # ✅ 按數量降序排列
      
      ggplot(lda_counts, aes(x = reorder(LDA_Category, n), y = n, fill = LDA_Category)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() +  # 讓 bar chart 是水平的
        theme_minimal() +
        labs(title = "LDA Distribution", x = "LDA Category", y = "Number of Tenders")
    })
    
    
    # **📌 TF-IDF 分析**
    word_tf_idf <- sample_data %>%
      unnest_tokens(word, tender_clean) %>%
      count(LDA_Category, tender_no, word) %>%
      bind_tf_idf(word, LDA_Category, n) %>%
      filter(tf_idf > quantile(tf_idf, 0.25) & tf_idf < quantile(tf_idf, 0.95)) %>%
      mutate(tf_idf = ifelse(tf_idf < 0, 0, tf_idf))  # 確保 TF-IDF 非負數
    
    
    filtered_data <- reactive({
      if (input$lda_category == "All") {
        word_tf_idf
      } else {
        word_tf_idf %>% filter(LDA_Category == input$lda_category)
      }
    })
    
    output$tfidf_table <- renderDT({
  datatable(
    filtered_data() %>% arrange(desc(tf_idf)), 
    options = list(
      pageLength = 10,   # 預設顯示 10 列
      scrollX = TRUE,    # ✅ 啟用水平捲動
      scrollY = "500px", # ✅ 限制最大高度
      autoWidth = TRUE,  # ✅ 自動調整欄位寬度
      fixedHeader = TRUE # ✅ 固定表頭
    ), 
    rownames = FALSE
  )
})

    
    output$wordcloud <- renderPlot({
      data <- filtered_data() %>% slice_head(n = input$num_words)
      color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(input$num_words)
      with(data, wordcloud(word, tf_idf, max.words = input$num_words, random.order = FALSE, colors = color_palette))
    })
    
    output$tfidf_plot <- renderPlotly({
      req(filtered_data())  
      
      data <- filtered_data() %>%
        filter(tf_idf > 0) %>%  
        slice_head(n = input$num_words)
      
      if (nrow(data) == 0) {
        return(plotly::plot_ly() %>% layout(title = "No TF-IDF Data Available"))
      }
      
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
        layout(hoverlabel = list(bgcolor = "lightblue"),showlegend = FALSE)
         
    })
  })
}

shinyApp(ui, server)
