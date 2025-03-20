
# 加載必要的套件
pacman::p_load(plotly, shiny, igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, dplyr, bsicons, bslib, 
               tidytext, topicmodels, tm, wordcloud, DT, RColorBrewer)

# 讀取 Cleaned_GP 數據
Cleaned_GP <- read_csv("data/GovernmentProcurementviaGeBIZ.csv")

# 擴充 Stopwords
extra_stopwords <- c("please", "refer", "another", "one", "two", "three", "framework", "edition", 
                     "related", "whole", "period", "government", "entities", "various", "including",
                     "requirement", "provide", "supply", "service", "procurement", "year", "option", 
                     "term", "extend", "agreement")
custom_stopwords <- c("singapore", "services", "tender", "provision", "works", "contract", 
                      "supply", "invitation", "appointment", "board", "national", "years", "delivery")
all_stopwords <- c(stopwords("en"), custom_stopwords, extra_stopwords)

# 文本清理
Cleaned_GP <- Cleaned_GP %>%
  mutate(
    tender_clean = tender_description %>%
      tolower() %>%
      removePunctuation() %>%
      removeNumbers() %>%
      stripWhitespace() %>%
      removeWords(all_stopwords)
  )

# 訓練 LDA 模型 (7 類)
dtm <- Cleaned_GP %>%
  unnest_tokens(word, tender_clean) %>%
  count(tender_no, word) %>%
  cast_dtm(document = tender_no, term = word, value = n)

lda_model <- LDA(dtm, k = 7, control = list(seed = 1234))

lda_assignments <- tidy(lda_model, matrix = "gamma")

# LDA 分類對應到政府採購類別
Cleaned_GP <- Cleaned_GP %>%
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

# 計算 TF-IDF
word_tf_idf <- Cleaned_GP %>%
  unnest_tokens(word, tender_clean) %>%
  count(LDA_Category, tender_no, word) %>%
  bind_tf_idf(word, LDA_Category, n) %>%
  filter(tf_idf > quantile(tf_idf, 0.25) & tf_idf < quantile(tf_idf, 0.95))


# --------------- UI ----------------
ui <- page_fluid(
  
  tags$head(tags$style(HTML(" 
      .nav-tabs { border-bottom: 3px solid #E74C3C !important; }
      .custom-header { display: flex; justify-content: space-between; align-items: center; 
                       background: linear-gradient(to right, #3366FF, #2858D7); 
                       padding: 20px; color: white; border-radius: 10px 10px 0 0; }
      #go_home { background-color: white; color: #3366FF; border: 2px solid white;
                 border-radius: 8px; padding: 10px 20px; font-size: 14px; font-weight: bold; }
      #go_home:hover { background-color: #2858D7; color: #b9f5f7; }
  "))),
  
  div(class = "custom-header",
      h1("Let's Track Public Dollars!", style = "margin: 0;"),
      actionButton("go_home", "Back to Home Page", onclick = "window.location.href='https://isss608-sgmoneytracker.netlify.app/'")
  ),
  
  div(style = "padding: 20px;",
      navset_tab(
        nav_panel("Tender Classification", 
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("category", "Select LDA Category:", choices = unique(Cleaned_GP$LDA_Category), selected = "General Procurement - Goods"),
                      sliderInput("num_words", "Number of Top Words:", min = 5, max = 30, value = 10)
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("LDA Distribution", plotOutput("lda_category_plot")),
                        tabPanel("TF-IDF Table", DTOutput("tfidf_table")),
                        tabPanel("Wordcloud", plotOutput("wordcloud")),
                        tabPanel("TF-IDF Bar Plot", plotOutput("tfidf_plot"))
                      )
                    )
                  )
        ),
        nav_panel("Procurement Dashboard", card(card_body(" "))),
        nav_panel("G2B Procurement Networks", card(card_body(" "))),
        id = "tab"
      )
  )
)

# --------------- Server ----------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    word_tf_idf %>% filter(LDA_Category == input$category)
  })
  output$lda_category_plot <- renderPlot({
    lda_counts <- Cleaned_GP %>% count(LDA_Category)
    ggplot(lda_counts, aes(x = reorder(LDA_Category, -n), y = n, fill = LDA_Category)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(title = "LDA Distribution", x = "LDA Category", y = "Number of Tenders")
  })
  output$tfidf_table <- renderDT({
    datatable(filtered_data() %>% arrange(desc(tf_idf)), options = list(pageLength = 10), rownames = FALSE)
  })
  

  output$wordcloud <- renderPlot({
    data <- filtered_data() %>%
      arrange(desc(tf_idf)) %>%
      slice_head(n = input$num_words)  # 选取前 num_words 个 tf-idf 最高的词
    
    # 生成一个足够长的颜色列表，防止颜色重复
    color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(input$num_words)
    
    
    with(data, wordcloud(word, tf_idf, 
                         max.words = input$num_words, 
                         random.order = FALSE, 
                         colors = color_palette))
  })
  
  
  output$tfidf_plot <- renderPlot({
    data <- filtered_data() %>%
      arrange(desc(tf_idf)) %>%  # 按 tf_idf 降序排列
      slice_head(n = input$num_words)  # 取前 num_words 個詞
    
    ggplot(data, aes(x = reorder(word, tf_idf), y = tf_idf, fill = LDA_Category)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top TF-IDF Words", x = "Word", y = "TF-IDF Score")
  })
  
}

# 啟動 Shiny 應用
shinyApp(ui, server)