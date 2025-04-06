mod_text_data_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "data_sampling",  # this should match the sidebar tabName
    fluidPage(
      titlePanel("Select Data Sample Size"),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("sample_size"), "Choose Data Sample:", 
                      choices = c("1000" = 1000, "5000" = 5000, "10000" = 10000, "All" = NA),
                      selected = 1000),
          actionButton(ns("load_data"), "Load Data")
        ),
        mainPanel(
          verbatimTextOutput(ns("data_summary"))
        )
      )
    )
  )
}