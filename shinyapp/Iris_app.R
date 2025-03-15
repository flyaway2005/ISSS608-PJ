
pacman::p_load(plotly, shiny, igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, dplyr, bsicons, bslib)

# Load Data
GP <- read_csv("data/GovernmentProcurementviaGeBiz.csv")
GP$award_date <- as.Date(GP$award_date, format="%d/%m/%Y")

#----------------- Module 1: Tender Classification --------------------
tender_classification_module <- function(input, output, session) {
  # Module-specific logic goes here
}

#----------------- Module 2: Procurement Dashboard -------------------
procurement_dashboard_module <- function(input, output, session) {
  # Module-specific logic goes here
}

#----------------- Module 3: G2B Procurement Networks ----------------
g2b_procurement_networks_module <- function(input, output, session) {
  # Module-specific logic goes here
}

#---------------------- UI ----------------------
library(shiny)
library(bslib)

ui <- page_fluid(
  
  # ✅ Include CSS styles
  tags$head(
    tags$style(HTML("
    
    /* Change the bottom border color of Tabs */
      .nav-tabs {
        border-bottom: 3px solid #E74C3C !important; /* Match Data section color */
      }
      
      /* Header styling */
      .custom-header {
        display: flex; 
        justify-content: space-between; 
        align-items: center; 
        background: linear-gradient(to right, #3366FF, #2858D7);
        padding: 20px; 
        color: white; 
        border-radius: 10px 10px 0 0;
      }

      /* Back to Home Page button styling */
      #go_home {
        background-color: white;
        color: #3366FF;
        border: 2px solid white;
        border-radius: 8px;
        padding: 10px 20px;
        font-size: 14px;
        font-weight: bold;
        cursor: pointer;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
      }
      #go_home:hover {
        background-color: #2858D7;
        color: #b9f5f7;
      }

      /* Tabs styling */
      .nav-link {
        font-size: 16px !important;
        font-weight: bold;
        padding: 12px 15px !important;
      }
      .nav-tabs .nav-item.show .nav-link, 
      .nav-tabs .nav-link.active {
        background-color: #3366FF !important;
        color: white !important;
        border-radius: 8px 8px 0 0;
      }
      .nav-tabs {
        border-bottom: 2px solid #3366FF !important;
      }
    "))
  ),
  
  # ✅ Header with Back to Home Page button
  div(class = "custom-header",
      h1("Let's Track Public Dollars!", style = "margin: 0;"),
      actionButton("go_home", "Back to Home Page", 
                   onclick = "window.location.href='https://isss608-sgmoneytracker.netlify.app/'")
  ),
  
  # ✅ Main Navigation
  div(style = "padding: 20px;",  # Add spacing
      navset_tab(
        nav_panel("Tender Classification", 
                  card(
                    card_body(" ")
                  )),
        nav_panel("Procurement Dashboard", 
                  card(
                    card_body("")
                  )),
        nav_panel("G2B Procurement Networks", 
                  card(
                    card_body(" ")
                  )),
        id = "tab"
      )
  )
)

#---------------------- Server ----------------------
server <- function(input, output, session) {
  
  # Call Module 1: Tender Classification
  callModule(tender_classification_module, "tender_classification")
  
  # Call Module 2: Procurement Dashboard
  callModule(procurement_dashboard_module, "procurement_dashboard")
  
  # Call Module 3: G2B Procurement Networks
  callModule(g2b_procurement_networks_module, "g2b_procurement_networks")
  
}

# Run App
shinyApp(ui, server)


