mod_text_data_server <- function(id, selected_data, current_stopwords, Cleaned_GP_LDA) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$load_data, {
      sample_size <- as.numeric(input$sample_size)
      if (is.na(sample_size)) {
        sample_data <- Cleaned_GP_LDA
      } else {
        set.seed(1234)
        sample_data <- Cleaned_GP_LDA %>% sample_n(sample_size)
      }
      
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
      
      output$data_summary <- renderPrint({
        paste("Loaded", ifelse(is.na(sample_size), "all", sample_size), "records.")
      })
    })
  })
}
