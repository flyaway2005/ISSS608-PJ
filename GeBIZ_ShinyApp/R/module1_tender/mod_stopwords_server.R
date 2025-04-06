mod_stopwords_server <- function(id, selected_data, lda_results, current_stopwords, default_stopwords) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 初始化 stopwords 表格
    output$stopwords_table <- renderDT({
      req(current_stopwords())
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
    
    # 添加新的 stopword
    observeEvent(input$add_stopword, {
      new_word <- trimws(input$new_stopword)
      if (new_word != "") {
        current_stopwords(c(current_stopwords(), new_word))
        updateTextInput(session, "new_stopword", value = "")
      }
    })
    
    # 刪除選中的 stopwords
    observeEvent(input$remove_stopword, {
      selected_rows <- input$stopwords_table_rows_selected
      if (!is.null(selected_rows)) {
        current_stopwords(current_stopwords()[-selected_rows])
      }
    })
    
    # 重置為預設 stopwords
    observeEvent(input$reset_stopwords, {
      current_stopwords(default_stopwords)
    })
  })
}

