mod_lda_supervised_server <- function(id, selected_data, lda_results, current_stopwords = NULL, default_stopwords = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define default stopwords if not provided
    if (is.null(default_stopwords)) {
      default_stopwords <- c(stopwords("en"), "please", "refer", "another", "one", "two", "three", 
                             "framework", "edition", "related", "whole", "period", "government", 
                             "entities", "various", "including", "requirement", "provide", "supply", 
                             "service", "procurement", "year", "option", "extend", "agreement", 
                             "singapore", "Singapore")
    }
    
    # Initialize current_stopwords if not provided
    if (is.null(current_stopwords) || !inherits(current_stopwords, "reactive")) {
      current_stopwords <- reactiveVal(default_stopwords)
    }
    
    
    # Ensure current_stopwords is a reactiveVal
    if (!is.reactiveval(current_stopwords)) {
      current_stopwords <- reactiveVal(current_stopwords)
    }
    
    # Display Stopwords table
    output[[ns("full_stopwords_table")]] <- renderDT({
      # Use req() to ensure current_stopwords() has a value
      req(current_stopwords())
      datatable(
        data.frame(Stopwords = current_stopwords()),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "200px",
          searching = TRUE,  # Enable searching
          lengthChange = FALSE
        ),
        rownames = FALSE,
        colnames = "Stopwords",
        selection = "multiple"  # Allow multiple row selection
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
          
          # Re-run LDA analysis if already run before
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
            
            # Update category choices
            updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          }
        }
        
        # Trigger reload
        shinyjs::click(ns("reload_lda"))
      }
    })
    
    # Remove selected Stopwords
    observeEvent(input$remove_stopword, {
      selected_rows <- input$stopwords_table_rows_selected
      if (!is.null(selected_rows)) {
        # Get the current stopwords and the selected stopwords
        all_stopwords <- current_stopwords()
        selected_stopwords <- all_stopwords[selected_rows]
        
        # Only remove custom stopwords, keep default stopwords
        custom_stopwords <- all_stopwords[!all_stopwords %in% default_stopwords]
        selected_custom_stopwords <- selected_stopwords[!selected_stopwords %in% default_stopwords]
        
        # Remove only the selected custom stopwords
        new_custom_stopwords <- custom_stopwords[!custom_stopwords %in% selected_custom_stopwords]
        
        # Combine default stopwords with remaining custom stopwords
        current_stopwords(c(default_stopwords, new_custom_stopwords))
        
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
          
          # Re-run LDA analysis if already run before
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
            
            # Update category choices
            updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
          }
        }
        
        # Trigger reload
        shinyjs::click(ns("reload_lda"))
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
        
        # Re-run LDA analysis if already run before
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
          
          # Update category choices
          updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
        }
      }
      
      # Trigger reload
      shinyjs::click(ns("reload_lda"))
    })
    
    # Observe Run Supervised Button
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
      
      lda_results(sample_data %>% select(tender_no, LDA_Category, tender_clean))
      updateSelectInput(session, "lda_category", choices = c("All", unique(sample_data$LDA_Category)))
    })
    
    observeEvent(input$reload_lda, {
      req(lda_results())
      # Clear cached network data on reload
      cached_network_data(NULL)
      
      processed_data <- lda_results() %>%
        mutate(
          tender_clean = tender_clean %>%
            tolower() %>%
            removePunctuation() %>%
            removeNumbers() %>%
            stripWhitespace() %>%
            removeWords(current_stopwords())
        )
      
      word_tf_idf <- processed_data %>%
        unnest_tokens(word, tender_clean) %>%
        count(LDA_Category, tender_no, word) %>%
        distinct(LDA_Category, tender_no, word, .keep_all = TRUE) %>%
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
      
      output$lda_category_plot <- renderPlotly({
        lda_counts <- lda_results() %>%
          count(LDA_Category) %>%
          arrange(desc(n))
        
        p <- ggplot(lda_counts, aes(x = reorder(LDA_Category, n), y = n, fill = LDA_Category)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() +
          labs(title = "LDA Distribution", x = "LDA Category", y = "Number of Tenders")
        
        ggplotly(p) %>% layout(showlegend = FALSE)
      })
      
      output$tfidf_table <- renderDT({
        req(filtered_data())
        datatable(filtered_data() %>% arrange(desc(tf_idf)),
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "500px",
                    autoWidth = TRUE,
                    fixedHeader = TRUE
                  ), 
                  rownames = FALSE)
      })
      
      output$wordcloud <- renderPlot({
        req(filtered_data())
        data <- filtered_data() %>% slice_head(n = input$num_words)
        color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(input$num_words)
        with(data, wordcloud(word, tf_idf, max.words = input$num_words, random.order = FALSE, colors = color_palette))
      })
      
      output$tfidf_plot <- renderPlotly({
        req(filtered_data())
        data <- filtered_data() %>% filter(tf_idf > 0) %>% slice_head(n = input$num_words)
        p <- ggplot(data, aes(x = reorder(word, tf_idf), y = tf_idf, fill = LDA_Category, text = paste0(
          "Word: ", word, "<br>", "TF-IDF: ", round(tf_idf, 6), "<br>", "Category: ", LDA_Category))) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          theme_minimal() +
          labs(title = "Top TF-IDF Words", x = "Word", y = "TF-IDF Score")
        
        ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "lightblue"), showlegend = FALSE)
      })
      
      output$tfidf_network_plot <- renderPlot({
        req(filtered_data())
        set.seed(1234)
        top_words <- filtered_data() %>%
          group_by(word) %>%
          summarise(avg_tfidf = mean(tf_idf)) %>%
          arrange(desc(avg_tfidf)) %>%
          slice_head(n = input$top_n_words_network) %>%
          pull(word)
        
        word_doc_pairs <- filtered_data() %>%
          filter(word %in% top_words) %>%
          select(tender_no, word) %>%
          distinct()
        
        word_cor <- word_doc_pairs %>% pairwise_cor(item = word, feature = tender_no, sort = TRUE, upper = FALSE)
        word_cor_filtered <- word_cor %>% filter(correlation > input$cor_threshold)
        
        if (nrow(word_cor_filtered) == 0) {
          plot.new()
          text(0.5, 0.5, "No strong word correlations found.")
          return()
        }
        
        g <- graph_from_data_frame(word_cor_filtered)
        
        ggraph(g, layout = "fr") +
          geom_edge_link(aes(alpha = correlation, width = correlation), color = "gray40") +
          geom_node_point(size = 6, color = "lightblue") +
          geom_node_text(aes(label = name), color = "red", repel = TRUE) +
          scale_edge_width_continuous(name = "correlation") +
          scale_edge_alpha_continuous(guide = "none") +
          scale_edge_width(range = c(0.5, 3)) +
          scale_edge_alpha(range = c(0.3, 1)) +
          theme_void()
      })
    })
  })
}