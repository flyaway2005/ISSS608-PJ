mod_stopwords_server <- function(id, selected_data, lda_results, current_stopwords, default_stopwords) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ✅ 加 ns() 才能 render 成模組正確 ID
    output[[ns("stopwords_table")]] <- renderDT({
      print("current_stopwords content:")
      print(current_stopwords())  # ✅ 觀察 console 中有沒有初始值
      datatable(
        data.frame(stopword = current_stopwords()),
        selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE, scrollY = "200px")
      )
    })
    
    observeEvent(input[[ns("add_stopword")]], {
      new_word <- trimws(input[[ns("new_stopword")]])
      if (new_word != "") {
        current_stopwords(c(current_stopwords(), new_word))
        updateTextInput(session, "new_stopword", value = "")
      }
    })
    
    observeEvent(input[[ns("remove_stopword")]], {
      selected_rows <- input[[ns("stopwords_table_rows_selected")]]
      if (!is.null(selected_rows)) {
        current_stopwords(current_stopwords()[-selected_rows])
      }
    })
    
    observeEvent(input[[ns("reset_stopwords")]], {
      current_stopwords(default_stopwords)
    })
  })
}

