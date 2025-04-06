mod_stopwords_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Stopwords Management"),
    DTOutput(ns("stopwords_table")),
    textInput(ns("new_stopword"), "Add New Stopword:"),
    actionButton(ns("add_stopword"), "Add Stopword"),
    actionButton(ns("remove_stopword"), "Remove Selected"),
    actionButton(ns("reset_stopwords"), "Reset to Default")
  )
}
