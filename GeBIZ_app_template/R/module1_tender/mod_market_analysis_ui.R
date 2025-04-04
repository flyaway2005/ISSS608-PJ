mod_market_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "market_analysis",
    fluidPage(
      titlePanel("Tender Market Analysis"),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("market_data_source"), "Select Data Source:",
                      choices = c("Default Dataset" = "default",
                                  "LDA Analysis Results" = "lda")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'lda'", ns("market_data_source")),
            div(class = "alert alert-warning",
                HTML("<i class='fa fa-exclamation-triangle'></i> <strong>Note:</strong> To use LDA Analysis Results, please go back to the Data Selection tab, select your dataset, and run LDA analysis first.")
            )
          ),
          uiOutput(ns("date_slider")),
          selectInput(ns("market_category"), "Select Category:",
                      choices = c("All", "General Procurement", "Engineering Procurement", "PPP Procurement")),
          selectInput(ns("tender_status"), "Select Tender Status:",
                      choices = c("All", 
                                  "Awarded to Suppliers",
                                  "Awarded by Items",
                                  "Award by interface record")),
          checkboxInput(ns("remove_outliers"), "Remove Outliers", value = TRUE),
          actionButton(ns("run_market_analysis"), "Run Analysis")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Market Overview",
                     fluidRow(
                       valueBoxOutput(ns("total_tenders"), width = 4),
                       valueBoxOutput(ns("total_value"), width = 4),
                       valueBoxOutput(ns("avg_value"), width = 4)
                     ),
                     plotlyOutput(ns("market_trend_plot"))),
            tabPanel("Time Analysis",
                     selectInput(ns("time_unit"), "Select Time Unit:",
                                 choices = c("Monthly" = "month",
                                             "Quarterly" = "quarter",
                                             "Yearly" = "year")),
                     plotlyOutput(ns("monthly_analysis_plot"))),
            tabPanel("Dynamic Scatter Plot",
                     plotlyOutput(ns("dynamic_scatter_plot")))
          )
        )
      )
    )
  )
}