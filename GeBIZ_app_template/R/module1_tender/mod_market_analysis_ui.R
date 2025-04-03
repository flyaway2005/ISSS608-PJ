mod_market_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "market_analysis",
          fluidPage(
            titlePanel("Tender Market Analysis"),
            sidebarLayout(
              sidebarPanel(
                uiOutput(ns("date_slider")),
                selectInput(ns("market_category"), "Select Category:",
                            choices = c("All", "General Procurement", "Engineering Procurement", "PPP Procurement")),
                selectInput(ns("tender_status"), "Select Tender Status:",
                            choices = c("All", "Awarded to Suppliers", "Awarded by Items", "Award by interface record")),
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
                           plotly::plotlyOutput(ns("market_trend_plot"))),
                  tabPanel("Time Analysis",
                           selectInput(ns("time_unit"), "Select Time Unit:",
                                       choices = c("Monthly" = "month", "Quarterly" = "quarter", "Yearly" = "year")),
                           plotly::plotlyOutput(ns("monthly_analysis_plot"))),
                  tabPanel("Dynamic Scatter Plot",
                           plotly::plotlyOutput(ns("dynamic_scatter_plot")))
                )
              )
            )
          ))
}