# app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(shinyWidgets)
library(plotly)
library(ggstatsplot)
library(DT)
library(shinyjs)


# Load preprocessed network data
agency_analysis_data <- read_csv("data/network_community_data.csv", show_col_types = FALSE)

# Define aggregate_by_time function that was missing
aggregate_by_time <- function(data, time_period) {
  if(time_period == "year") {
    data %>%
      group_by(time_label = award_year) %>%
      summarize(total_awarded = sum(awarded_amt, na.rm = TRUE))
  } else if(time_period == "quarter") {
    data %>%
      mutate(quarter = paste0(award_year, "-Q", ceiling(as.numeric(month)/3))) %>%
      group_by(time_label = quarter) %>%
      summarize(total_awarded = sum(awarded_amt, na.rm = TRUE))
  } else if(time_period == "month") {
    data %>%
      group_by(time_label = year_month) %>%
      summarize(total_awarded = sum(awarded_amt, na.rm = TRUE))
  }
}


#-----------------------------
# Time series UI Module
#-----------------------------
# ---- Top Agencies Analysis UI Module ----
top_agencies_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Top Agencies Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        # Date range filter
        dateRangeInput(ns("date_range"), "Date Range:",
                       start = NULL,
                       end = NULL),
        
        # Agency type selection 
        selectizeInput(ns("agency_type"), "Select Agency Type:",
                       choices = c("All"),
                       multiple = TRUE,
                       selected = "All"),
        
        # Agency selection 
        selectizeInput(ns("agency"), "Select Agency:",
                       choices = c("All"),
                       multiple = TRUE,
                       selected = "All"),
        
        # Supplier selection
        selectizeInput(ns("supplier"), "Select Supplier:",
                       choices = c("All"),
                       multiple = TRUE,
                       selected = "All",
                       options = list(maxOptions = 10000)),
        
        # Tender type selection 
        selectizeInput(ns("tender_type"), "Tender Type:",
                       choices = c("All"), 
                       multiple = TRUE,
                       selected = "All"),
        # Update button
        actionButton(ns("update_btn"), "Update Visualisation", 
                     class = "btn-primary btn-block",
                     style = "margin-top: 20px;")
      ),
    
      mainPanel(id = ns("main_panels"),
                # Include shinyjs for dynamic UI adjustments
                shinyjs::useShinyjs(),
                
                # hidden value to track toggle state
                shinyjs::hidden(textInput(ns("panel_state"), "", value = "normal")),
                uiOutput(ns("dynamic_panels")),     
                
                verbatimTextOutput(ns("debugOutput")),
                
                # Top agencies section - side by side chart and metrics
                fluidRow(
                  # Agency bar chart
                  column(8,
                         box(
                           title = "Top Agencies by Award Amount",
                           width = NULL,
                           status = "primary",
                           solidHeader = TRUE,
                           fluidRow(
                             column(8, 
                                    h4("Award Distribution by Agency", style = "margin-top: 5px;")),
                             column(4,
                                    selectInput(ns("top_n_agencies"), 
                                                "Show:", 
                                                choices = c("Top 5" = 5, 
                                                            "Top 10" = 10, 
                                                            "Top 15" = 15, 
                                                            "Top 20" = 20),
                                                selected = 5,
                                                width = "100%"))
                           ),
                           plotlyOutput(ns("agency_plot"), height = "500px")
                         )
                  ),
                  
                  # Agency metrics table - positioned on the right
                  column(4,
                         box(
                           title = "Agency Metrics",
                           width = NULL,
                           status = "info",
                           solidHeader = TRUE,
                           height = "590px", # Match height with the chart box
                           DT::dataTableOutput(ns("agency_metrics")),
                           div(style = "padding-top: 10px;", 
                               actionButton(ns("toggle_panel"), "Toggle Panel", 
                                            class = "btn-sm btn-default",
                                            icon = icon("arrows-alt-h")))
                         )
                  )
                ),
                
                # Top suppliers section - side by side chart and metrics
                fluidRow(
                  # Supplier bar chart
                  column(8, 
                         box(
                           title = "Top Suppliers (by selected Agency)",
                           width = NULL,
                           status = "primary",
                           solidHeader = TRUE,
                           textOutput(ns("selected_agency_info")),
                           plotlyOutput(ns("supplier_plot"), height = "500px")
                         )
                  ),
                  
                  # Supplier metrics table - positioned on the right
                  column(4,
                         box(
                           title = "Supplier Metrics",
                           width = NULL,
                           status = "info",
                           solidHeader = TRUE,
                           height = "590px", 
                           DT::dataTableOutput(ns("supplier_metrics"))
                         )
                  )
                )
      )
  )
)
}

#-----------------------------------------------
# Server for agency analysis
#-----------------------------------------------
# Check
cat("Data loaded successfully:", !is.null(agency_analysis_data), "\n")
cat("Number of rows in agency_analysis_data:", nrow(agency_analysis_data), "\n")

# ---- Top Agencies Analysis Server Module ----
top_agencies_server <- function(id, parent_data) {
  moduleServer(id, function(input, output, session) {
    
    output$debugOutput <- renderText({
      paste("Data loaded with", nrow(isolate(parent_data())), "rows,", 
            length(unique(isolate(parent_data())$agency_type)), "agency types,",
            length(unique(isolate(parent_data())$agency)), "agencies")
    })
    
    observeEvent(parent_data(), {
      cat("Module initializing with data...\n")
      
      # Get the data
      df <- parent_data()
      
      # Debug what we're working with
      cat("Data dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
      cat("Columns:", paste(head(colnames(df)), collapse=", "), "...\n")
      
      # Only update if we have data
      if(nrow(df) > 0) {
        # Check if expected columns exist
        if(!"agency_type" %in% colnames(df)) {
          cat("WARNING: agency_type column missing!\n")
        }
        if(!"agency" %in% colnames(df)) {
          cat("WARNING: agency column missing!\n")
        }
        if(!"supplier_name" %in% colnames(df)) {
          cat("WARNING: supplier_name column missing!\n")
        }
        if(!"tender_cat" %in% colnames(df)) {
          cat("WARNING: tender_cat column missing!\n")
        }
        
        # Update agency type choices - with error handling
        tryCatch({
          agency_types <- c("All", unique(df$agency_type))
          cat("Updating agency_type with", length(agency_types)-1, "choices\n")
          updateSelectizeInput(session, "agency_type", choices = agency_types, selected = "All")
        }, error = function(e) {
          cat("ERROR updating agency_type:", e$message, "\n")
        })
        
        # Update agency choices - with error handling
        tryCatch({
          agencies <- c("All", unique(df$agency))
          cat("Updating agency with", length(agencies)-1, "choices\n")
          updateSelectizeInput(session, "agency", choices = agencies, selected = "All")
        }, error = function(e) {
          cat("ERROR updating agency:", e$message, "\n")
        })
        
        # Update supplier choices - with error handling
        tryCatch({
          suppliers <- c("All", unique(df$supplier_name))
          cat("Updating supplier with", length(suppliers)-1, "choices\n")
          updateSelectizeInput(session, "supplier", choices = suppliers, selected = "All")
        }, error = function(e) {
          cat("ERROR updating supplier:", e$message, "\n")
        })
        
        # Update tender type choices - with error handling
        tryCatch({
          tender_types <- c("All", unique(df$tender_cat))
          cat("Updating tender_type with", length(tender_types)-1, "choices\n")
          updateSelectizeInput(session, "tender_type", choices = tender_types, selected = "All")
        }, error = function(e) {
          cat("ERROR updating tender_type:", e$message, "\n")
        })
        
        cat("Input choices update completed successfully\n")
      } else {
        cat("WARNING: No data available to update input choices!\n")
      }
    }, ignoreNULL = FALSE)

    # Add a force update button handler
    output$dynamic_panels <- renderUI({
      actionButton(session$ns("force_update"), "Refresh Filters", 
                   icon = icon("sync"),
                   class = "btn-sm btn-info",
                   style = "margin-bottom: 15px;")
    })
    
    # Force update handler
    observeEvent(input$force_update, {
      cat("Force update triggered\n")
      
      df <- parent_data()
      
      # Update all inputs
      updateSelectizeInput(session, "agency_type", 
                           choices = c("All", unique(df$agency_type)), 
                           selected = "All")
      
      updateSelectizeInput(session, "agency", 
                           choices = c("All", unique(df$agency)), 
                           selected = "All")
      
      updateSelectizeInput(session, "supplier", 
                           choices = c("All", unique(df$supplier_name)), 
                           selected = "All")
      
      updateSelectizeInput(session, "tender_type", 
                           choices = c("All", unique(df$tender_cat)), 
                           selected = "All")
    })
 
    # Test with hardcoded values
    observeEvent(input$update_btn, {
      cat("Update button clicked, running test with hardcoded values...\n")
      
      # Try with hardcoded values (no data dependency)
      test_values <- c("All", paste0("Test", 1:5))
      
      # Update with hardcoded test values
      tryCatch({
        # Try to update agency_type with test values
        updateSelectizeInput(session, "agency_type", 
                             choices = test_values,
                             selected = "All")
        cat("Hardcoded test values applied to agency_type\n")
      }, error = function(e) {
        cat("ERROR updating with hardcoded values:", e$message, "\n")
      })
    }, ignoreInit = TRUE)
    
    # Filtered data based on user selections
    filtered_data <- reactive({
      req(parent_data())
      req(input$update_btn)
      
      data <- parent_data()
      
      # Apply date range filter 
      if (!is.null(input$date_range)) {
        date_start <- as.Date(input$date_range[1])
        date_end <- as.Date(input$date_range[2])
        data <- data %>% 
          filter(award_date >= date_start & award_date <= date_end)
      }
      
      # Filter by agency type
      if (!"All" %in% input$agency_type) {
        data <- data %>% filter(agency_type %in% input$agency_type)
      }
      
      # Filter by agency
      if (!"All" %in% input$agency) {
        data <- data %>% filter(agency %in% input$agency)
      }
      
      # Filter by supplier
      if (!"All" %in% input$supplier) {
        data <- data %>% filter(supplier_name %in% input$supplier)
      }
      
      # Filter by tender type (using tender_cat)
      if (!"All" %in% input$tender_type) {
        data <- data %>% filter(tender_cat %in% input$tender_type)
      }
      
      data
    })
    
    # Reactive values for storing selected agency
    selected_values <- reactiveValues(
      selected_agency = NULL
    )
    
    # Reactive values for panel state
    panel_state <- reactiveValues(expanded = FALSE)
    
    # Toggle panel action
    observeEvent(input$toggle_panel, {
      current_state <- input$panel_state
      if(current_state == "normal") {
        updateTextInput(session, "panel_state", value = "expanded")
      } else {
        updateTextInput(session, "panel_state", value = "normal")
      }
    })
    
    # Render the panel width based on state
    observe({
      req(input$panel_state)  # Make sure panel_state exists
      panels_id <- session$ns("main_panels")  # Use session$ns instead of ns
      
      if(input$panel_state == "expanded") {  # Use input$panel_state instead of panel_state$expanded
        shinyjs::runjs(sprintf('
    $("#%s .col-sm-8:first").removeClass("col-sm-8").addClass("col-sm-6");
    $("#%s .col-sm-4:first").removeClass("col-sm-4").addClass("col-sm-6");
    $("#%s .col-sm-8:last").removeClass("col-sm-8").addClass("col-sm-6");
    $("#%s .col-sm-4:last").removeClass("col-sm-4").addClass("col-sm-6");
    ', panels_id, panels_id, panels_id, panels_id))
      } else {
        # Default state: chart takes more space
        shinyjs::runjs(sprintf('
    $("#%s .col-sm-6:first").removeClass("col-sm-6").addClass("col-sm-8");
    $("#%s .col-sm-6:nth-child(2)").removeClass("col-sm-6").addClass("col-sm-4");
    $("#%s .col-sm-6:nth-child(3)").removeClass("col-sm-6").addClass("col-sm-8");
    $("#%s .col-sm-6:last").removeClass("col-sm-6").addClass("col-sm-4");
    ', panels_id, panels_id, panels_id, panels_id))
      }
    })
    
    # Calculate top agencies
    top_agencies_data <- reactive({
      req(filtered_data())
      req(input$top_n_agencies)
      
      # Aggregate data by agency
      agencies <- filtered_data() %>%
        group_by(agency_type, agency) %>%
        summarise(
          award_amount = sum(awarded_amt, na.rm = TRUE),
          tender_count = n_distinct(tender_no),
          .groups = "drop"
        ) %>%
        arrange(desc(award_amount)) %>%
        head(as.numeric(input$top_n_agencies))
      
      # Add percentage of total
      total_amount <- sum(agencies$award_amount)
      agencies <- agencies %>%
        mutate(
          percentage = award_amount / total_amount * 100,
          percentage_label = sprintf("%.1f%%", percentage)
        )
      
      agencies
    })
    
    # Render the agency horizontal bar chart
    output$agency_plot <- renderPlotly({
      req(top_agencies_data())
      agencies <- top_agencies_data()
      
      agencies <- agencies %>%
        arrange(award_amount) %>%
        mutate(agency = factor(agency, levels = agency))
      
      # Create base plot
      p <- ggplot(agencies, aes(x = award_amount, y = agency, 
                                text = paste0("Agency: ", agency,
                                              "<br>Type: ", agency_type,
                                              "<br>Amount: $", formatC(award_amount, format="f", digits=0, big.mark=","),
                                              "<br>Tenders: ", tender_count,
                                              "<br>Percentage: ", percentage_label))) +
        geom_col(aes(fill = agency_type)) +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(x = "Total Award Amount", y = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      # interactivity
      gg <- ggplotly(p, tooltip = "text", source = "agency_plot") %>%
        event_register("plotly_click") %>%
        layout(margin = list(l = 100, r = 100))
      
      gg
    })
    
    # Render agency metrics table
    output$agency_metrics <- DT::renderDataTable({
      req(top_agencies_data())
      
      top_agencies_data() %>%
        select(
          "Agency Type" = agency_type,
          "Agency Name" = agency,
          "Award Amount" = award_amount,
          "Tender Count" = tender_count,
          "Percentage" = percentage
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 5,
            dom = 'tip',
            ordering = TRUE
          ),
          rownames = FALSE
        ) %>%
        DT::formatCurrency(columns = "Award Amount", currency = "$", digits = 0) %>%
        DT::formatPercentage(columns = "Percentage", digits = 1)
    })
    
    # Capture click events from the agency plot
    observeEvent(event_data("plotly_click", source = "agency_plot"), {
      click_data <- event_data("plotly_click", source = "agency_plot")
      
      if(!is.null(click_data)) {
        index <- click_data$pointNumber + 1
        
        agencies <- top_agencies_data() %>% arrange(award_amount)
        
        if(index <= nrow(agencies)) {
          selected_values$selected_agency <- agencies$agency[index]
        }
      }
    })
    
    # Calculate top suppliers for the selected agency
    top_suppliers_data <- reactive({
      req(filtered_data())
      req(selected_values$selected_agency)
      
      # Filter by selected agency
      suppliers <- filtered_data() %>%
        filter(agency == selected_values$selected_agency) %>%
        group_by(supplier_name, tender_cat) %>%
        summarise(
          award_amount = sum(awarded_amt, na.rm = TRUE),
          tender_count = n_distinct(tender_no),
          .groups = "drop"
        )
      
      # Get total per supplier across all tender types
      supplier_totals <- suppliers %>%
        group_by(supplier_name) %>%
        summarise(
          total_amount = sum(award_amount, na.rm = TRUE),
          total_tenders = sum(tender_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total_amount)) %>%
        head(10)  # Top 10 suppliers
      
      # Join back to get tender_type breakdown and keep only top suppliers
      suppliers <- suppliers %>%
        inner_join(supplier_totals %>% select(supplier_name), by = "supplier_name") %>%
        left_join(supplier_totals %>% select(supplier_name, total_amount), by = "supplier_name") %>%
        mutate(
          percentage = award_amount / total_amount * 100,
          percentage_label = sprintf("%.1f%%", percentage)
        )
      
      suppliers
    })
    
    # Display selected agency info
    output$selected_agency_info <- renderText({
      if(is.null(selected_values$selected_agency)) {
        "Click on an agency bar to see its top suppliers"
      } else {
        paste("Selected Agency:", selected_values$selected_agency)
      }
    })
    
    # Render the supplier stacked bar chart
    output$supplier_plot <- renderPlotly({
      req(top_suppliers_data())
      suppliers <- top_suppliers_data()
      
      # aggregate by supplier first
      supplier_totals <- suppliers %>%
        group_by(supplier_name) %>%
        summarise(total_amount = sum(award_amount), .groups = "drop") %>%
        arrange(total_amount) %>%
        mutate(supplier_name = factor(supplier_name, levels = supplier_name))
      
      # Then join back to the tender type breakdown
      suppliers <- suppliers %>%
        left_join(supplier_totals %>% select(supplier_name), by = "supplier_name") %>%
        mutate(supplier_name = factor(supplier_name, levels = levels(supplier_totals$supplier_name)))
      
      # Create the stacked bar chart
      p <- ggplot(suppliers, aes(x = award_amount, y = supplier_name,
                                 fill = tender_cat,
                                 text = paste0("Supplier: ", supplier_name,
                                               "<br>Tender Category: ", tender_cat,
                                               "<br>Amount: $", formatC(award_amount, format="f", digits=0, big.mark=","),
                                               "<br>Percentage: ", percentage_label))) +
        geom_col() +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(x = "Award Amount", y = NULL, fill = "Tender Category") +
        theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 100, r = 100))
    })
    
    # Render supplier metrics table
    output$supplier_metrics <- DT::renderDataTable({
      req(top_suppliers_data())
      
      # Aggregate by supplier for the metrics table
      suppliers <- top_suppliers_data() %>%
        group_by(supplier_name) %>%
        summarise(
          total_amount = sum(award_amount),
          total_tenders = sum(tender_count),
          tender_types = paste(unique(tender_cat), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(total_amount))
      
      suppliers %>%
        select(
          "Supplier Name" = supplier_name,
          "Award Amount" = total_amount,
          "Tender Count" = total_tenders,
          "Tender Types" = tender_types
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 5,
            dom = 'tip',
            ordering = TRUE
          ),
          rownames = FALSE
        ) %>%
        DT::formatCurrency(columns = "Award Amount", currency = "$", digits = 0)
    })
    
  })
}


ui <- dashboardPage(
  dashboardHeader(title = "Procurement Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Agencies", tabName = "top_agencies", icon = icon("building")),
      menuItem("Test Page", tabName = "test_page", icon = icon("vial"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "top_agencies",
              top_agencies_ui("top_agencies")
      ),
      
      # Add this new tab item for testing
      tabItem(tabName = "test_page",
              fluidRow(
                column(12,
                       box(
                         title = "Direct Test Dropdowns",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         selectizeInput("test_dropdown", "Test Dropdown:", 
                                        choices = c("All", "Test1", "Test2", "Test3"),
                                        selected = "All"),
                         actionButton("test_update", "Update Test Dropdown"),
                         verbatimTextOutput("test_output")
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  cat("Data loaded successfully:", !is.null(agency_analysis_data), "\n")
  cat("Number of rows in agency_analysis_data:", nrow(agency_analysis_data), "\n")
  cat("Columns in agency_analysis_data:", paste(colnames(agency_analysis_data), collapse=", "), "\n")
  
  # Create a reactive for the data
  data <- reactiveVal({
    cat("Initializing data reactiveVal...\n")
    tryCatch({
    df <- agency_analysis_data
    
    # Print available columns for debugging
    cat("Data loaded with", nrow(df), "rows and", ncol(df), "columns\n")
    cat("Columns:", paste(colnames(df), collapse=", "), "\n")

    # Ensure required columns exist
    if(!"agency_type" %in% colnames(df)) {
      cat("Creating agency_type column from agency\n")
      df$agency_type <- sapply(strsplit(as.character(df$agency), " "), function(x) x[1])
    }
    
    if(!"tender_cat" %in% colnames(df)) {
      cat("Creating default tender_cat column\n")
      df$tender_cat <- "Default Category"
    }
    
    if(!"supplier_name" %in% colnames(df) && "supplier" %in% colnames(df)) {
      cat("Copying supplier to supplier_name\n")
      df$supplier_name <- df$supplier
    }
    
    return(df)
    }, error = function(e) {
      cat("ERROR initializing data:", e$message, "\n")
      # Return empty data frame with required columns as fallback
      return(data.frame(
        agency_type = character(0),
        agency = character(0),
        supplier_name = character(0),
        tender_cat = character(0),
        stringsAsFactors = FALSE
      ))
    })
  })
  
  # Direct update test - bypassing the module
  observe({
    # Use direct session scope - no module
    cat("Directly updating from server...\n")
    df <- data()
    cat("Direct update with", nrow(df), "rows and", ncol(df), "columns\n")
    
    # Debug - check if data is available
    if(nrow(df) > 0) {
      cat("Debug: First agency_type =", df$agency_type[1], "\n")
      cat("Debug: Number of unique agency_types =", length(unique(df$agency_type)), "\n")
      cat("Debug: First few agency_types:", paste(head(unique(df$agency_type)), collapse=", "), "\n")
      
      # Try to directly update the input (with module namespace)
      tryCatch({
        updateSelectizeInput(session, "top_agencies-agency_type", 
                             choices = c("All", unique(df$agency_type)),
                             selected = "All")
        cat("Direct update completed\n")
      }, error = function(e) {
        cat("ERROR in direct update:", e$message, "\n")
      })
    }
  })

#-----
# debug  
  observe({
    cat("Data reactive initialized with sample data:\n")
    print(head(data(), 3))
    cat("Number of rows:", nrow(data()), "\n")
    cat("Unique agency types:", paste(head(unique(data()$agency_type)), collapse=", "), "...\n")
    cat("Unique agencies:", paste(head(unique(data()$agency)), collapse=", "), "...\n")
  })
  # Direct test of dropdown functionality
  output$test_output <- renderPrint({
    cat("Test dropdown value:", input$test_dropdown, "\n")
    cat("Agency types in data:", length(unique(data()$agency_type)), "\n")
    cat("First few agency types:", paste(head(unique(data()$agency_type)), collapse=", "), "\n")
  })
  
  # Test button handler
  observeEvent(input$test_update, {
    cat("Test update button clicked\n")
    df <- data()
    
    if(nrow(df) > 0) {
      # Get actual values from data
      agency_types <- c("All", unique(df$agency_type))
      cat("Updating with", length(agency_types)-1, "agency types\n")
      
      # Update the test dropdown
      updateSelectizeInput(session, "test_dropdown", 
                           choices = agency_types,
                           selected = "All")
    }
  })
#-----
  
  # Call the top agencies module server
  top_agencies_server("top_agencies", data)
}

# Run the application
shinyApp(ui, server)