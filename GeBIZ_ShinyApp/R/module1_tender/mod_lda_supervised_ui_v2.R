mod_lda_supervised_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "lda_supervised",
          fluidPage(
            titlePanel("LDA Supervised Learning"),
            sidebarLayout(
              sidebarPanel(
                actionButton(ns("run_supervised"), "Run LDA Analysis"),
                sliderInput(ns("num_words"), "Number of Words:", min = 5, max = 20, value = 10),
                selectInput(ns("lda_category"), "Select LDA Category:", choices = NULL, selected = "All"),
                actionButton(ns("reload_lda"), "Reload"),
                hr(),
                mod_stopwords_ui(ns("stopwords")),  
                hr(),
                h4("TF-IDF Network Settings"),
                sliderInput(ns("top_n_words_network"), "Top N Words (TF-IDF):", min = 20, max = 200, value = 100, step = 10),
                sliderInput(ns("cor_threshold"), "Min Correlation Threshold:", min = 0.1, max = 0.9, value = 0.3, step = 0.05)
              )
              ,
              mainPanel(
                tabsetPanel(
                  tabPanel("LDA Distribution", plotlyOutput(ns("lda_category_plot"))),
                  tabPanel("TF-IDF Table", DTOutput(ns("tfidf_table"))),
                  tabPanel("Wordcloud", plotOutput(ns("wordcloud"))),
                  tabPanel("TF-IDF Bar Plot", plotlyOutput(ns("tfidf_plot"))),
                  tabPanel("TF-IDF Network", plotOutput(ns("tfidf_network_plot"), height = "500px"))
                )
              )
            )
          ))
}