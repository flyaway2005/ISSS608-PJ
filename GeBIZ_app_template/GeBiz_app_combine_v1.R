library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(networkD3)
library(igraph)
library(DT)
library(visNetwork)
library(RColorBrewer)


# Load global settings and functions
source("global.R")

#-----------------------------------------------------
# add more source (modules here)
#-----------------------------------------------------
# Source module file
source("R/module1_tender/mod_text_data_ui.R")
source("R/module1_tender/mod_text_data_server.R")
source("R/module2_tempo/module2_overview_git_0330_3_tooltip.R")
source("R/module2_tempo/module2_agency_git.R")
source("R/module3_network/module3-1_network_overview_git_0330_1_nodeshape.R")
source("R/module3_network/module3-2_community_git_0329_3.R")



#-----------------------------------------------------
# This is the app UI
#-----------------------------------------------------
ui <- dashboardPage(
  skin = "blue",  
  
  dashboardHeader(title = "SG Public Dollars"),
  
  # Sidebar for navigation only
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Tender Analysis", icon = icon("search"),
               menuSubItem("Data Sampling", tabName = "data_sampling"),
               menuSubItem("LDA Supervised", tabName = "lda_supervised"),
               menuSubItem("LDA Clustering", tabName = "lda_clustering"),
               menuSubItem("Market Insights", tabName = "market_analysis")
               ),
      menuItem("Procurement Trends", tabName = "temo_analysis", icon = icon("dashboard"),
               menuSubItem("Time Series", tabName = "time_series"),
               menuSubItem("Agency Explorer", tabName = "agency_analysis")),
      menuItem("Network Insights", tabName = "network_insights", icon = icon("project-diagram"),
               menuSubItem("Network Overview", tabName = "network"),
               menuSubItem("Community Detection", tabName = "community"))
    )),
  
  dashboardBody(
    # Apply CSS from global.R
    tags$head(
      tags$style(app_theme$css)
    ),
    # Custom CSS
    # tags$head(
    #   tags$style("
    #     /* Header background color */
    #     .skin-blue .main-header .navbar {
    #       background-color: #B7CADB;
    #     }
    #     
    #     /* Logo background color */
    #     .skin-blue .main-header .logo {
    #       background-color: #A7BDCD;
    #     }
    #     
    #     /* Logo hover color */
    #     .skin-blue .main-header .logo:hover {
    #       background-color: #97ADBD;
    #     }
    #     
    #     /* Sidebar background color */
    #     .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    #       background-color: #FDF6EC;
    #     }
    #     
    #     /* Sidebar text color */
    #     .skin-blue .sidebar a {
    #       color: #444;
    #     }
    #     
    #     /* Sidebar width */
    #     .main-sidebar {
    #       width: 208px;
    #     }
    #     
    #     /* Content wrapper adjustment */
    #     .content-wrapper, .right-side {
    #       margin-left: 200px;
    #     }
    #     
    #     /* Active sidebar item */
    #     .skin-blue .sidebar-menu > li.active > a,
    #     .skin-blue .sidebar-menu > li:hover > a {
    #       color: #000;
    #       background: #F7E3C1;
    #       border-left-color: #B7CADB;
    #     }
    #     
    #     /* Box borders */
    #     .box.box-primary {
    #       border-top-color: #B7CADB;
    #     }
    #     
    #     /* Button colors */
    #     .btn-primary {
    #       background-color: #B7CADB;
    #       border-color: #A7BDCD;
    #     }
    #     
    #     .btn-primary:hover {
    #       background-color: #A7BDCD;
    #     }
    #     
    #     /* Control panel styling */
    #     .control-panel {
    #       background-color: #F8F8F8;
    #       border-right: 1px solid #ddd;
    #       padding: 15px;
    #       margin-bottom: 15px;
    #     }
    #     
    #     /* Main layout with flexbox */
    #     .tab-content {
    #       display: flex;
    #       flex-direction: row;
    #     }
    #     
    #     /* Control panel width */
    #     .control-section {
    #       width: 250px;
    #       flex-shrink: 0;
    #     }
    #     
    #     /* Content section */
    #     .content-section {
    #       flex-grow: 1;
    #       padding-left: 15px;
    #     }
    #   ")
    # ),
    
#-----------------------------------------------------
# All tab items
# Each tanItem() is a module that we develop, whether we categorise it as main or sub-module.
#-----------------------------------------------------
    tabItems(
#-------------      
# Module 1-1 tab
#-------------
      tabItem(tabName = "data_sampling",
              mod_text_data_ui("data_sampling_module")
      ),
#-------------      
# Module 1-2 tab
#-------------
      tabItem(tabName = "unsupervised_learning",
              # Flexbox container for layout
              div(class = "tab-content",
                  # Control section (left)
                  div(class = "control-section",
                      div(class = "control-panel",
                          h3("Dashboard Controls"),
                          dateRangeInput("date_range_ul", "Date range:",
                                         start = Sys.Date() - 30, end = Sys.Date()),
                          selectInput("dashboard_metric_ul", "Metric:", 
                                      choices = c("Visitors", "Revenue", "Conversions")),
                          actionButton("refresh_ul", "Refresh Data", class = "btn-primary")
                      )
                  ),
                  
                  # Content section (right)
                  div(class = "content-section",
                      h2("Unsupervised Learning Dashboard"),
                      box(
                        title = "Overview", status = "primary", solidHeader = TRUE,
                        width = NULL,
                        "Dashboard content goes here"
                      ),
                      fluidRow(
                        infoBox(
                          title = "Visitors", value = "10,521", subtitle = "Total",
                          icon = icon("users"), color = "light-blue", width = 4
                        ),
                        infoBox(
                          title = "Revenue", value = "$45,820", subtitle = "Total",
                          icon = icon("dollar-sign"), color = "light-blue", width = 4
                        ),
                        infoBox(
                          title = "Conversions", value = "521", subtitle = "Total",
                          icon = icon("shopping-cart"), color = "light-blue", width = 4
                        )
                      )
                  )
              )
              
      ),

#------------------------------
# Module 2-1 Overview 
# need to update width of plot
#------------------------------ 
tabItem(tabName = "time_series",
#        h2("Procurement Trends"),
        time_series_ui("timeSeries")
),

      
#------------------------------
# Module 2-2 Agency analysis (pending update)
#------------------------------    
      tabItem(tabName = "agency_analysis",
              # Flexbox container for layout
              div(class = "tab-content",
                  # Control section (left)
                  div(class = "control-section",
                      div(class = "control-panel",
                          h3("Analysis Controls"),
                          selectInput("var", "Choose variable:", 
                                      choices = c("mpg", "wt", "hp")),
                          sliderInput("bins", "Number of bins:",
                                      min = 5, max = 50, value = 15),
                          checkboxInput("density", "Add density curve", FALSE),
                          hr(),
                          actionButton("update", "Update Plot", class = "btn-primary")
                      )
                  ),
                  
                  # Content section (right)
                  div(class = "content-section",
                      h2("Analysis"),
                      box(
                        plotOutput("plot"), 
                        width = NULL,
                        title = "Data Visualization"
                      ),
                      fluidRow(
                        valueBox(
                          value = textOutput("mean_value"), 
                          subtitle = "Mean Value", 
                          icon = icon("chart-line"),
                          color = "light-blue",
                          width = 4
                        ),
                        valueBox(
                          value = textOutput("median_value"), 
                          subtitle = "Median Value", 
                          icon = icon("chart-bar"),
                          color = "light-blue",
                          width = 4
                        ),
                        valueBox(
                          value = textOutput("sd_value"), 
                          subtitle = "Standard Deviation", 
                          icon = icon("calculator"),
                          color = "light-blue",
                          width = 4
                        )
                      )
                  )
              )
      ),

#------------------------------
# Module 2-3 Supplier analysis
#------------------------------ 
    tabItem(tabName = "supplier_analysis",
        h2("Supplier Markets"),
        "Supplier analysis content will go here."
),


#-----------------------------------------------------
# Module 3 Network analysis
#-----------------------------------------------------

    tabItem(tabName = "network",
            network_analysis_ui("network_module")
    ),

    tabItem(tabName = "community",
            network_community_ui("community_module")
    ),


#-------------------
# Introduction tab
#-------------------
tabItem(tabName = "introduction",
        h2("Introduction"),
        "Welcome to the SG Public Dollars dashboard. Use the menu on the left to navigate."
        )
      )
  )
)

#--------------------------------------
# Server
#-------------------------
server <- function(input, output, session) {
  selected_data <- reactiveVal(NULL)
  # Shared reactive values
  selected_data <- reactiveVal(NULL)
  lda_results <- reactiveVal(NULL)
  default_stopwords <- c(stopwords("en"), "please", "refer", "another", "one", "two", "three", 
                         "framework", "edition", "related", "whole", "period", "government", 
                         "entities", "various", "including", "requirement", "provide", "supply", 
                         "service", "procurement", "year", "option", "extend", "agreement", 
                         "singapore", "Singapore")
  current_stopwords <- reactiveVal(default_stopwords)
  
  # Load cleaned LDA dataset for sampling
  Cleaned_GP_LDA <- readr::read_csv("data/Cleaned_GP_LDA.csv")

  # Load time series data
  ts_overview_data <- read_csv("data/GeBiz_add_y_m.csv")
  # Load network data
  community_data_global <- read_csv("data/network_community_data.csv")
  network_data <- readRDS("data/m3_processed_network_data.rds")
  
  # Reactive value for plot data
  plot_data <- reactive({
    mtcars
  })
  
  # Main plot
  output$plot <- renderPlot({
    p <- ggplot(plot_data(), aes_string(x = input$var)) +
      geom_histogram(fill = "#B7CADB", color = "#333333", 
                     bins = input$bins) +
      theme_minimal() +
      labs(title = paste("Distribution of", input$var),
           x = input$var, y = "Count")
    
    if(input$density) {
      p <- p + geom_density(alpha = 0.5, fill = "#A7BDCD")
    }
    
    p
  })
  
  #Initialise data sampling module server
  mod_text_data_server(
    id = "data_sampling_module",
    selected_data = selected_data,
    current_stopwords = current_stopwords,
    Cleaned_GP_LDA = readr::read_csv("data/Cleaned_GP_LDA.csv")
  )
  
  # Initialise time series module server
  time_series_server("timeSeries", ts_overview_data)
  
  # Initialise network module server
  network_analysis_server("network_module", network_data)
  network_community_server("community_module", community_data_global)
  
  # Statistics for valueBoxes
  output$mean_value <- renderText({
    round(mean(plot_data()[[input$var]]), 2)
  })
  
  output$median_value <- renderText({
    round(median(plot_data()[[input$var]]), 2)
  })
  
  output$sd_value <- renderText({
    round(sd(plot_data()[[input$var]]), 2)
  })
}

shinyApp(ui, server)