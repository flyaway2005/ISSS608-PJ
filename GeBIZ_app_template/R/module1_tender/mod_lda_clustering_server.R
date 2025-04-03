mod_lda_clustering_server <- function(id, selected_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$run_unsupervised, {
      req(selected_data())
      
      sample_data <- selected_data()
      dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_data$tender_clean)))
      lda_model <- LDA(dtm, k = input$num_topics, control = list(seed = 1234))
      
      doc_topic_matrix <- posterior(lda_model)$topics %>%
        as.data.frame() %>%
        mutate(document = seq_len(nrow(.)))
      
      word_topic_tfidf <- tidy(lda_model, matrix = "beta") %>%
        group_by(topic) %>%
        top_n(5, beta) %>%
        summarise(top_words = paste(term, collapse = ", "))
      
      set.seed(1234)
      kmeans_result <- kmeans(doc_topic_matrix[,-ncol(doc_topic_matrix)], centers = input$num_clusters)
      clustered_matrix <- doc_topic_matrix
      clustered_matrix$cluster <- factor(kmeans_result$cluster)
      
      doc_topic_melted <- reshape2::melt(clustered_matrix, id.vars = c("document", "cluster"), 
                                         variable.name = "Topic", value.name = "Probability")
      
      doc_topic_melted <- doc_topic_melted %>%
        mutate(topic = as.numeric(gsub("V", "", Topic))) %>%
        left_join(word_topic_tfidf, by = c("topic" = "topic"))
      
      updateSelectInput(session, "select_cluster", choices = c("All", unique(doc_topic_melted$cluster)))
      
      # All cluster plot
      output$cluster_plot <- renderPlotly({
        ggplotly(ggplot(doc_topic_melted, aes(x = Topic, y = Probability, group = document, color = cluster,
                                              text = paste("Topic:", topic, "<br>",
                                                           "Top Words:", top_words, "<br>",
                                                           "Probability:", round(Probability, 4), "<br>",
                                                           "Document:", document, "<br>",
                                                           "Cluster:", cluster))) +
                   geom_line(alpha = 0.1, size = 0.3) +
                   geom_point(size = 1, alpha = 0.5) +
                   facet_wrap(~ cluster, scales = "free_y") +
                   theme_minimal(), tooltip = "text")
      })
      
      # Single cluster view
      observeEvent(input$select_cluster, {
        if (input$select_cluster == "All") {
          output$single_cluster_plot <- renderPlotly(NULL)
        } else {
          filtered_data <- doc_topic_melted %>% filter(cluster == input$select_cluster)
          output$single_cluster_plot <- renderPlotly({
            ggplotly(ggplot(filtered_data, aes(x = Topic, y = Probability, group = document, color = cluster,
                                               text = paste("Topic:", topic, "<br>",
                                                            "Top Words:", top_words, "<br>",
                                                            "Probability:", round(Probability, 4), "<br>",
                                                            "Document:", document, "<br>",
                                                            "Cluster:", cluster))) +
                       geom_line(alpha = 0.1, size = 0.3) +
                       geom_point(size = 1, alpha = 0.5) +
                       facet_wrap(~ cluster, scales = "free_y") +
                       theme_minimal(), tooltip = "text")
          })
        }
      })
    })
  })
}