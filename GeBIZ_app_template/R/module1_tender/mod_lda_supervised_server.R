mod_lda_supervised_server <- function(id, selected_data, lda_results, current_stopwords) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$run_supervised, {
      req(selected_data())
      data <- selected_data()
      
      dtm <- data %>%
        unnest_tokens(word, tender_clean) %>%
        count(tender_no, word) %>%
        cast_dtm(document = tender_no, term = word, value = n)
      
      lda_model <- topicmodels::LDA(dtm, k = 7, control = list(seed = 1234))
      
      lda_assignments <- tidy(lda_model, matrix = "gamma")
      
      data <- data %>%
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
      
      lda_results(data %>% select(tender_no, LDA_Category, tender_clean))
      updateSelectInput(session, "lda_category", choices = c("All", unique(data$LDA_Category)))
    })
    
    observeEvent(input$reload_lda, {
      req(lda_results())
      processed <- lda_results()
      
      word_tf_idf <- processed %>%
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
      
      output$lda_category_plot <- plotly::renderPlotly({
        plot_data <- lda_results() %>% count(LDA_Category)
        p <- ggplot(plot_data, aes(x = reorder(LDA_Category, n), y = n, fill = LDA_Category)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal()
        plotly::ggplotly(p) %>% layout(showlegend = FALSE)
      })
      
      output$tfidf_table <- DT::renderDT({
        req(filtered_data())
        DT::datatable(filtered_data() %>% arrange(desc(tf_idf)))
      })
      
      output$wordcloud <- renderPlot({
        req(filtered_data())
        wc_data <- filtered_data() %>% slice_head(n = input$num_words)
        color_palette <- RColorBrewer::brewer.pal(8, "Dark2")
        with(wc_data, wordcloud::wordcloud(word, tf_idf, colors = color_palette))
      })
      
      output$tfidf_plot <- plotly::renderPlotly({
        req(filtered_data())
        p <- filtered_data() %>% 
          slice_head(n = input$num_words) %>%
          ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = LDA_Category)) +
          geom_col() +
          coord_flip() +
          theme_minimal()
        plotly::ggplotly(p)
      })
    })
  })
}