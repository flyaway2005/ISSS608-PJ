mod_lda_supervised_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("LDA Supervised Learning"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("run_supervised"), "Run LDA Analysis"),
        sliderInput(ns("num_words"), "Number of Words:", min = 5, max = 20, value = 10),
        selectInput(ns("lda_category"), "Select LDA Category:", choices = NULL, selected = "All"),
        actionButton(ns("reload_lda"), "Reload")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("LDA Distribution", plotly::plotlyOutput(ns("lda_category_plot"))),
          tabPanel("TF-IDF Table", DT::DTOutput(ns("tfidf_table"))),
          tabPanel("Wordcloud", plotOutput(ns("wordcloud"))),
          tabPanel("TF-IDF Bar Plot", plotly::plotlyOutput(ns("tfidf_plot")))
        )
      )
    )
  )
}