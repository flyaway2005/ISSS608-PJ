# app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(visNetwork)
library(igraph)
library(scales)
library(DT)

# Load preprocessed network data
#network_data <- readRDS("data/m3_processed_network_data.rds")


#-----------------------------
# Network Analysis UI Module
#-----------------------------
network_analysis_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Use sidebarLayout to organize components
    sidebarLayout(
      # Left sidebar for controls
      sidebarPanel(
        h4("Control Panel"),
        
        #---------------------------------------------
        # 1. UI MODIFICATIONS - Replace the year filter with this
        #---------------------------------------------
        
        h5("Date Range Filter"),
        # Start date
        fluidRow(
          column(2, h5("Start", style = "margin-top: 7px; text-align: right;")),
          column(5,
                 selectInput(ns("start_year"), "Year",
                             choices = c("2019", "2020", "2021", "2022", "2023", "2024"),
                             selected = "2019",
                             width = "100%"
                 )
          ),
          column(5,
                 selectInput(ns("start_month"), "Month",
                             choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                         "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                             selected = 1,
                             width = "100%"
                 )
          )
        ),
        # End date
        fluidRow(
          column(2, h5("End", style = "margin-top: 7px; text-align: right;")),
          column(5,
                 selectInput(ns("end_year"),"",
                             choices = c("2019", "2020", "2021", "2022", "2023", "2024"),
                             selected = "2024",
                             width = "100%"
                 )
          ),
          column(5,
                 selectInput(ns("end_month"), "",
                             choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                         "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                             selected = 12,
                             width = "100%"
                 )
          )
        ),
        
        # Agency Type filter
        selectizeInput(ns("agency_type_filter"), "Select Agency Types", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select agency types',
                         plugins = list('remove_button')
                       )),
                
        # Agency filter
        selectizeInput(ns("agency_filter"), "Select Agencies", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select agencies',
                         plugins = list('remove_button')
                       )),
        
        # Supplier filter
        selectizeInput(ns("supplier_filter"), "Select Suppliers", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select suppliers',
                         plugins = list('remove_button')
                       )),
        # Tender Category filter
        selectizeInput(ns("tender_cat_filter"), "Select Tender Categories", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select tender categories',
                         plugins = list('remove_button')
                       )),
        
        # Award amount range with improved layout
        div(
          style = "margin-bottom: 25px;",
          sliderInput(ns("award_amount_range"), "Award Amount Range",
#                      label = NULL,
                      min = 0, 
                      max = 1500000000,  
                      value = c(0, 50000),
                      step = 50000),
          div(
            style = "display: flex; justify-content: space-between; margin-top: -15px;",
            div(
              style = "width: 45%;",
              numericInput(ns("min_award_manual"), NULL, 
                           value = 100000,
                           min = 0,
                           max = 1500000000),
              div(style = "text-align: center; margin-top: -15px;", 
                  tags$small("Min"))
            ),
            div(
              style = "width: 45%;",
              numericInput(ns("max_award_manual"), NULL, 
                           value = 1000000,
                           min = 0,
                           max = 1500000000),
              div(style = "text-align: center; margin-top: -15px;", 
                  tags$small("Max"))
            )
          )
        ),    
        
        
        # Maximum edges to plot
        sliderInput(ns("max_edges"), "Maximum Edges to Plot",
                    min = 10, 
                    max = 10000,
                    value = 500,
                    step = 5000),
        
        # Edge metric selection
        radioButtons(ns("edge_metric"), "Edge Thickness Based On",
                     choices = c("Award Amount" = "total_award_amount", 
                                 "Contract Count" = "total_contracts")),
        
        # Layout algorithm
        selectInput(ns("layout_type"), "Network Layout",
                    choices = c("Force-Directed" = "force", 
                                "Bipartite" = "bipartite", 
                                "Radial" = "radial"),
                    selected = "force"),
        # Node size based on degree
        checkboxInput(ns("size_by_degree"), "Size Nodes by Degree Centrality", TRUE),
        
        # Add a performance mode toggle
        checkboxInput(ns("performance_mode"), "Performance Mode", TRUE),
        
        # Update button
        actionButton(ns("update_network"), "Update Network", 
                     class = "btn-primary", 
                     width = "100%"),
        width = 4  # Set width of sidebar (out of 12)
      ),
      #----------------------------------
      # Main panel for visualizations
      #----------------------------------
      mainPanel(  
        width = 8,
        tabsetPanel(
          id = ns("network_tabs"),
          
          # Tab 1: Main Network Visualization
          tabPanel(
            title = "Network Overview",
            # Main Network Visualization - Full Width
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Network Visualization",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  visNetworkOutput(ns("network_plot"), height = "600px")
                )
              )
            ),
            # Network Summary - Full Width
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Network Summary",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "info",
                  verbatimTextOutput(ns("network_summary"))
                )
              )
            ),
            # Network Details Tables - Full Width
            fluidRow(
              column(
                width = 12,
                # Tabbed box for detailed metrics
                tabBox(
                  title = "Network Details",
                  width = NULL,
                  tabPanel(
                    title = "Agencies",
                    DTOutput(ns("agency_table"))
                  ),
                  tabPanel(
                    title = "Suppliers",
                    DTOutput(ns("supplier_table"))
                  ),
                  tabPanel(
                    title = "Top Contracts",
                    DTOutput(ns("contract_table"))
                  )
                )
              )
            )
          ),
        
        # Tab 2: Ego-centric
        tabPanel(
          title = "Ego Network",
          tabPanel(
            title = "Ego Network",
            # First row: Ego Network Visualization
            fluidRow(
              column(
                width = 12,  
                box(
                  title = "Ego Network",
                  width = NULL,
                  solidHeader = TRUE,
                  visNetworkOutput(ns("ego_network_plot"), height = "500px")
                )
              )
            ),
            # Second row: Key Metrics
            fluidRow(
              column(
                width = 12, 
                box(
                  title = "Ego Network Metrics",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "info",
                  verbatimTextOutput(ns("ego_metrics"))
                )
              )
            ),
            # Third row: Detailed tables
            fluidRow(
              column(
                width = 12,
                # Tabbed box for detailed ego metrics
                tabBox(
                  title = "Ego Network Details",
                  width = NULL,
                  tabPanel(
                    title = "Ego Agencies",
                    DTOutput(ns("ego_agency_table"))
                  ),
                  tabPanel(
                    title = "Ego Suppliers",
                    DTOutput(ns("ego_supplier_table"))
                  ),
                  tabPanel(
                    title = "Connections",
                    DTOutput(ns("ego_connection_table"))
                  )
                )
              )
            ),
            # When no node is selected
            uiOutput(ns("no_node_selected_message"))
          ))
      )
    ))
    )}
#--------------------------------
# Network Analysis Server Module
#--------------------------------
network_analysis_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive to store the data
    network_data <- reactive({
      if (is.null(data)) {
        # Load data from file as fallback
        readRDS("data/m3_processed_network_data.rds")
      } else if (is.reactive(data)) {
        # Use the passed data parameter
        data()
      } else{
        data
      }
    })
    
    #debug
    selected_node_tracker <- reactiveVal(NULL)
    
    # Create reactive values to store visualization data at module level
    network_vis_data <- reactiveVal(NULL)
    
    # Create a reactive value to store filtered agencies
    filtered_agencies <- reactiveVal(NULL)

#-----
# Debug    
    observe({
      # Ensure network_data() is available
      req(network_data())
      
      # Debug output
      print("Network data summary:")
      print(paste("Nodes:", nrow(network_data()$nodes)))
      print(paste("Edges:", nrow(network_data()$edges)))
      print("Node columns:")
      print(names(network_data()$nodes))
      print("Edge columns:")
      print(names(network_data()$edges))
    })
    
    
    # Populate month selection
    observe({
      # Check if months data is available
      if (!is.null(network_data()$months)) {
        # Get the years data for year selectors
        years <- sort(unique(network_data()$months$year))
        
        # Update the year selectors
        updateSelectInput(session, "start_year", 
                          choices = years,
                          selected = 2019)
        
        updateSelectInput(session, "end_year", 
                          choices = years,
                          selected = 2024)
        
        # Create month-only choices (1-12)
        month_choices <- setNames(
          1:12,
          c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
        )
        
        # Update the month selectors with numeric months only
        updateSelectInput(session, "start_month", 
                          choices = month_choices,
                          selected = 1)
        
        updateSelectInput(session, "end_month", 
                          choices = month_choices,
                          selected = 12)
        
        # Debug
        print("Month options populated with numeric months only")
        print(paste("Years range:", min(years), "to", max(years)))
      } else {
        # Debug if months data is missing
        print("WARNING: No months data available for filtering")
        
        # Try to create a fallback
        if (!is.null(network_data()$time_info) && 
            !is.null(network_data()$time_info$award_year)) {
          
          print("Using time_info as fallback")
          years <- sort(unique(network_data()$time_info$award_year))
          
          updateSelectInput(session, "start_year", 
                            choices = years,
                            selected = min(years))
          
          updateSelectInput(session, "end_year", 
                            choices = years,
                            selected = max(years))
          
          # Default month choices
          month_choices <- setNames(
            1:12,
            c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
          )
          
          updateSelectInput(session, "start_month", 
                            choices = month_choices,
                            selected = 1)
          
          updateSelectInput(session, "end_month", 
                            choices = month_choices,
                            selected = 12)
        }
      }
    })
    # Populate agency type choices
    observe({
      # Check if agency_types component exists
      if (!is.null(network_data()$agency_types)) {
        # Check what structure agency_types has and extract type values accordingly
        if (is.data.frame(network_data()$agency_types)) {
          # If it's a data frame with agency-type mapping
          print("agency_types is a data frame")
          
          # Assuming the second column contains the type data
          if (ncol(network_data()$agency_types) >= 2) {
            agency_types <- c("All", unique(network_data()$agency_types[[2]]))
          } else {
            agency_types <- c("All", "No agency types found")
          }
        } else if (is.list(network_data()$agency_types)) {
          # If it's a simple list of types
          print("agency_types is a list")
          agency_types <- c("All", unique(unlist(network_data()$agency_types)))
        } else if (is.vector(network_data()$agency_types)) {
          # If it's a vector of types
          print("agency_types is a vector")
          agency_types <- c("All", unique(network_data()$agency_types))
        } else {
          # Fallback
          print("agency_types has unknown structure")
          agency_types <- c("All", "Could not determine agency types")
        }
        
        # Remove NA values
        agency_types <- agency_types[!is.na(agency_types)]
        
        # Update the input
        updateSelectizeInput(
          session, 
          "agency_type_filter", 
          choices = agency_types,
          selected = c("All"),
          server = TRUE
        )
        print(paste("Agency types loaded:", length(agency_types)-1))
        
      } else {
        # Fallback if no agency_types exists
        print("WARNING: agency_types component not found")
        updateSelectizeInput(
          session, 
          "agency_type_filter", 
          choices = c("All", "No agency types available"),
          selected = c("All"),
          server = TRUE
        )
      }
    })

#----------------------------------------------------------------        
    # Observe agency type filter changes and update the filtered agencies list
#----------------------------------------------------------------      
    observeEvent(input$agency_type_filter, {
      # Debug
      print("Selected agency types:")
      print(input$agency_type_filter)
      
      # Default to all agencies if "All" is selected or nothing is selected
      if("All" %in% input$agency_type_filter || length(input$agency_type_filter) == 0) {
        agencies <- unique(network_data()$edges$agency)
        print(paste("No filter applied. Total agencies:", length(agencies)))
      } else {
        # Since agency_types is a vector according to console, let's try to find the agencies
        # We need to check if there's a way to map agency types to agencies
        print("Looking for agencies with the selected types...")
        
        # Check if there's a mapping in the nodes dataframe
        if ("agency_type" %in% names(network_data()$nodes)) {
          # Filter nodes by agency type
          filtered_nodes <- network_data()$nodes %>%
            filter(tolower(agency_type) %in% tolower(input$agency_type_filter))
          
          agencies <- filtered_nodes$name[filtered_nodes$type == "agency"]
          print(paste("Found", length(agencies), "agencies of selected types in nodes data"))
        }
        # Check if there's a mapping in the edges dataframe
        else if ("agency_type" %in% names(network_data()$edges)) {
          # Filter edges by agency type
          filtered_edges <- network_data()$edges %>%
            filter(tolower(agency_type) %in% tolower(input$agency_type_filter))
          
          agencies <- unique(filtered_edges$agency)
          print(paste("Found", length(agencies), "agencies of selected types in edges data"))
        }
        # If there's a specific mapping dataframe we're not aware of
        else if ("agency_type_mapping" %in% names(network_data())) {
          # Use the mapping if it exists
          filtered_agencies <- network_data()$agency_type_mapping %>%
            filter(tolower(agency_type) %in% tolower(input$agency_type_filter))
          
          agencies <- filtered_agencies$agency
          print(paste("Found", length(agencies), "agencies using agency_type_mapping"))
        }
        # Last resort: search all dataframes in network_data for a mapping
        else {
          # Check all dataframes in network_data for agency_type column
          mapping_found <- FALSE
          
          for (name in names(network_data())) {
            if (is.data.frame(network_data()[[name]]) && 
                "agency" %in% names(network_data()[[name]]) && 
                "agency_type" %in% names(network_data()[[name]])) {
              
              filtered_df <- network_data()[[name]] %>%
                filter(tolower(agency_type) %in% tolower(input$agency_type_filter))
              
              agencies <- unique(filtered_df$agency)
              print(paste("Found", length(agencies), "agencies in", name, "dataframe"))
              mapping_found <- TRUE
              break
            }
          }
          
          if (!mapping_found) {
            # If we can't find a mapping, show all agencies
            print("WARNING: Could not find any mapping between agency types and agencies")
            print("Showing all agencies instead")
            agencies <- unique(network_data()$edges$agency)
          }
        }
      }
      
      # Ensure we have agency list
      if (!exists("agencies") || length(agencies) == 0) {
        agencies <- unique(network_data()$edges$agency)
        print("Using all agencies as fallback")
      }
      
      # Update reactive value
      filtered_agencies(agencies)
      
      # Add "All" option and sort
      agency_choices <- c("All", sort(agencies))
      
      # CRITICAL: Update BOTH the choices and selection in one call
      updateSelectizeInput(
        session, 
        "agency_filter", 
        choices = agency_choices,
        selected = "All",
        server = TRUE
      )
      
      print("======= END AGENCY TYPE FILTER DEBUGGING =======")
    }, ignoreInit = FALSE)
    
    # Control date range
    observe({
  # For Start Date - restrict months based on year
  if (input$start_year == "2019") {
    # If 2019 is selected, only allow April-December
    updateSelectInput(session, ns("start_month"),
                     choices = c("04" = 4, "05" = 5, "06" = 6, "07" = 7, "08" = 8, 
                                "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                     selected = ifelse(as.numeric(input$start_month) < 4, 4, input$start_month))
  } else if (input$start_year == "2024") {
    # If 2024 is selected, only allow January-March
    updateSelectInput(session, ns("start_month"),
                     choices = c("01" = 1, "02" = 2, "03" = 3),
                     selected = ifelse(as.numeric(input$start_month) > 3, 3, input$start_month))
  } else {
    # For years 2020-2023, all months are valid
    updateSelectInput(session, ns("start_month"),
                     choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                     selected = input$start_month)
  }
  
  # For End Date - restrict months based on year
  if (input$end_year == "2019") {
    # If 2019 is selected, only allow April-December
    updateSelectInput(session, ns("end_month"),
                     choices = c("04" = 4, "05" = 5, "06" = 6, "07" = 7, "08" = 8, 
                                "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                     selected = ifelse(as.numeric(input$end_month) < 4, 4, input$end_month))
  } else if (input$end_year == "2024") {
    # If 2024 is selected, only allow January-March
    updateSelectInput(session, ns("end_month"),
                     choices = c("01" = 1, "02" = 2, "03" = 3),
                     selected = ifelse(as.numeric(input$end_month) > 3, 3, input$end_month))
  } else {
    # For years 2020-2023, all months are valid
    updateSelectInput(session, ns("end_month"),
                     choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                     selected = input$end_month)
  }
})
    
    # Populate supplier choices
    observe({
      updateSelectizeInput(
        session, 
        "supplier_filter", 
        choices = c("All", unique(network_data()$edges$supplier_name)),
        selected = c("All"),
        server = TRUE
      )
    })

        
    # Populate tender category choices
    observe({
      if ("tender_cat" %in% names(network_data()$edges)) {
        all_categories <- network_data()$edges$tender_cat
        # Split concatenated categories and extract unique values
        unique_categories <- unique(unlist(strsplit(all_categories, ", ")))
        unique_categories <- unique_categories[!is.na(unique_categories) & unique_categories != ""]
        unique_categories <- sort(unique_categories)
        # Add "All" option
        tender_cats <- c("All", unique_categories)
        
        
        updateSelectizeInput(
          session, 
          "tender_cat_filter", 
          choices = tender_cats,
          selected = c("All"),
          server = TRUE
        )
        print(paste("Tender categories loaded:", length(tender_cats)-1))
      } else {
        print("WARNING: tender_cat column not found in edges data")
        
        # Fallback if no tender_cat column exists
        updateSelectizeInput(
          session, 
          "tender_cat_filter", 
          choices = c("All", "No tender categories available"),
          selected = c("All"),
          server = TRUE
        )
      }
    })    
    
    # Sync slider with manual inputs
    # Initialize values to prevent initialization issues
    observeEvent(input$award_amount_range, {
      # Use isolate to prevent circular dependency
      isolate({
        updateNumericInput(session, "min_award_manual", value = input$award_amount_range[1])
        updateNumericInput(session, "max_award_manual", value = input$award_amount_range[2])
      })
    }, ignoreInit = FALSE, priority = 10) # High priority to run first
    
    # When min manual input changes, update slider
    observeEvent(input$min_award_manual, {
      if (!is.null(input$min_award_manual)) {
        isolate({
          current_range <- input$award_amount_range
          if (input$min_award_manual != current_range[1]) {
            # Ensure min doesn't exceed max
            new_min <- min(input$min_award_manual, current_range[2])
            # Only update if actually different to prevent recursion
            if(new_min != current_range[1]) {
              updateSliderInput(session, "award_amount_range", 
                                value = c(new_min, current_range[2]))
            }
          }
        })
      }
    }, ignoreInit = TRUE)
    
    # When max manual input changes, update slider
    observeEvent(input$max_award_manual, {
      if (!is.null(input$max_award_manual)) {
        isolate({
          current_range <- input$award_amount_range
          if (input$max_award_manual != current_range[2]) {
            # Ensure max isn't less than min
            new_max <- max(input$max_award_manual, current_range[1])
            # Only update if actually different to prevent recursion
            if(new_max != current_range[2]) {
              updateSliderInput(session, "award_amount_range", 
                                value = c(current_range[1], new_max))
            }
          }
        })
      }
    }, ignoreInit = TRUE)
    
    #Observe the node click on main network
    observeEvent(input$switch_to_ego, {
      updateTabsetPanel(session, "network_tabs", selected = "Ego Network")
    }, ignoreInit = TRUE)
    
    
    #--------
    # Reactive filtered data
    #---------
    filtered_data <- eventReactive(input$update_network, {
      nodes <- network_data()$nodes
      edges <- network_data()$edges
      
      # Month-based date range filtering
      if (!is.null(input$start_year) && !is.null(input$start_month) && 
          !is.null(input$end_year) && !is.null(input$end_month)) {
        
        print("==== BEGINNING DATE RANGE FILTER DEBUGGING ====")
        print(paste("Start:", input$start_year, "Month:", input$start_month))
        print(paste("End:", input$end_year, "Month:", input$end_month))
        
        if (!is.null(network_data()$monthly_edges)) {
          print("monthly_edges data exists")
          
          # Ensure inputs are numeric
          start_year <- as.numeric(input$start_year)
          start_month <- as.numeric(input$start_month)
          end_year <- as.numeric(input$end_year)
          end_month <- as.numeric(input$end_month)
          
          # Validate inputs before calculating indices
          if (!is.na(start_year) && !is.na(start_month) && 
              !is.na(end_year) && !is.na(end_month)) {
            
            # Calculate start and end month indices
            start_idx <- start_year * 12 + start_month
            end_idx <- end_year * 12 + end_month
            
            print(paste("Start index:", start_idx, "End index:", end_idx))
            
            # Ensure start isn't after end
            if (start_idx > end_idx) {
              temp <- start_idx
              start_idx <- end_idx
              end_idx <- temp
              print("Swapped start and end indices to ensure proper order")
            }
            
            # Check if month_index column exists in monthly_edges
            if ("month_index" %in% names(network_data()$monthly_edges)) {
              # Filter monthly edges
              filtered_monthly_edges <- network_data()$monthly_edges %>%
                filter(month_index >= start_idx & month_index <= end_idx)
              
              print(paste("After filtering, monthly_edges has", nrow(filtered_monthly_edges), "rows"))
              
              if (nrow(filtered_monthly_edges) > 0) {
                # Verify that total_contracts and total_award_amount columns exist
                if (all(c("total_contracts", "total_award_amount") %in% names(filtered_monthly_edges))) {
                  # Aggregate edges for the filtered period
                  monthly_filtered <- filtered_monthly_edges %>%
                    group_by(agency, supplier_name) %>%
                    summarize(
                      total_contracts = sum(total_contracts, na.rm = TRUE),
                      total_award_amount = sum(total_award_amount, na.rm = TRUE),
                      .groups = "drop"
                    )
                  
                  print(paste("After aggregation, we have", nrow(monthly_filtered), "edges"))
                  
                  # Filter the edges based on the monthly data
                  edges_filtered <- tryCatch({
                    edges %>%
                      semi_join(monthly_filtered, by = c("agency", "supplier_name"))
                  }, error = function(e) {
                    print(paste("Error in semi_join:", e$message))
                    return(edges)  # Return original if error
                  })
                  
                  if (nrow(edges_filtered) > 0) {
                    print(paste("After semi_join, edges has", nrow(edges_filtered), "rows"))
                    edges <- edges_filtered
                  } else {
                    print("No edges found after semi_join")
                  }
                } else {
                  print("Missing required columns in monthly_edges data")
                }
              } else {
                print("No monthly edges found in the selected date range")
              }
            } else {
              print("month_index column missing in monthly_edges")
            }
          } else {
            print("Invalid date inputs detected")
          }
        } else {
          print("monthly_edges data is missing")
        }
      }

      # Agency Type direct filtering
      if(!("All" %in% input$agency_type_filter) && length(input$agency_type_filter) > 0) {
        # Use the agency_types data frame to filter the edges
        if (!is.null(network_data()$agency_types) && is.data.frame(network_data()$agency_types)) {
          # Assuming first column is agency name and second is type
          agency_col <- names(network_data()$agency_types)[1]
          type_col <- names(network_data()$agency_types)[2]
          
          # Get agencies of the selected types - use proper data frame filtering
          agencies_filtered <- network_data()$agency_types %>%
            filter(.data[[type_col]] %in% input$agency_type_filter)
          
          agencies_of_type <- agencies_filtered[[agency_col]]
          
          # Print debug info
          print("Selected agency types in filtered_data:")
          print(input$agency_type_filter)
          print("Filtered agencies in filtered_data:")
          print(head(agencies_of_type))
          print("Total agencies found in filtered_data:")
          print(length(agencies_of_type))
          
          # Filter edges to only include these agencies
          if (length(agencies_of_type) > 0) {
            edges <- edges %>%
              filter(agency %in% agencies_of_type)
            
            print(paste("Filtered to", length(agencies_of_type), "agencies by agency type"))
          } else {
            # If no agencies match the selected types
            edges <- edges[0,]
            print("No agencies found for the selected agency types")
          }
        }
      }      
      print("Selected agency types:")
      print(input$agency_type_filter)
      
      # Agency filtering
      if(!("All" %in% input$agency_filter) && length(input$agency_filter) > 0) {
        edges <- edges %>%
          filter(agency %in% input$agency_filter)
      }
      
      # Supplier filtering
      if(!("All" %in% input$supplier_filter) && length(input$supplier_filter) > 0) {
        edges <- edges %>%
          filter(supplier_name %in% input$supplier_filter)
      }
 
      # Tender Category filtering
      if(!("All" %in% input$tender_cat_filter) && length(input$tender_cat_filter) > 0) {
        if ("tender_cat" %in% names(edges)) {
          # Create a vector to hold the filter results
          include_rows <- logical(nrow(edges))
          
          # Check each row
          for(i in 1:nrow(edges)) {
            cat_string <- edges$tender_cat[i]
            if (!is.na(cat_string) && cat_string != "") {
              cat_list <- unlist(strsplit(cat_string, ", "))
              include_rows[i] <- any(input$tender_cat_filter %in% cat_list)
            } else {
              include_rows[i] <- FALSE
            }
          }
          
          # Apply filter
          edges <- edges[include_rows, ]
          
          print(paste("Filtered by tender categories:", 
                      paste(input$tender_cat_filter, collapse=", ")))
        } else {
          print("WARNING: Could not apply tender_cat filter - column not found")
        }
      }
      
      # Award amount range filtering
      min_award <- input$award_amount_range[1]
      max_award <- input$award_amount_range[2]
      
      edges <- edges %>%
        filter(total_award_amount >= min_award & total_award_amount <= max_award)
      
      # Limit number of edges to improve performance
      if(nrow(edges) > input$max_edges) {
        edges <- edges %>%
          arrange(desc(total_award_amount)) %>%
          slice_head(n = input$max_edges)
      }
      
      
      # Get connected nodes
      connected_nodes <- unique(c(edges$agency, edges$supplier_name))
      nodes <- nodes %>%
        filter(name %in% connected_nodes)
      
      list(nodes = nodes, edges = edges)
    }, ignoreNULL = FALSE)
    
    # Update network information
    output$network_info <- renderText({
      data <- filtered_data()
      
      paste(
        "Network Information:",
        paste("Total Nodes:", nrow(data$nodes)),
        paste("Total Edges:", nrow(data$edges)),
        paste("Agencies:", sum(data$nodes$type == "agency")),
        paste("Suppliers:", sum(data$nodes$type == "supplier")),
        paste("Total Award Amount: $", format(sum(data$edges$total_award_amount), big.mark = ",", scientific = FALSE)),
        sep = "\n"
      )
    })
    
    # Prepare visualization data
    prepare_vis_data <- reactive({
      data <- filtered_data()
      
      # Debug output - check if data exists
      print("Filtered data summary:")
      print(paste("Nodes:", nrow(data$nodes)))
      print(paste("Edges:", nrow(data$edges)))
      
      if (nrow(data$nodes) == 0 || nrow(data$edges) == 0) {
        print("WARNING: No nodes or edges available to visualize!")
        return(NULL)
      }
      
      # Prepare nodes for visualization
      nodes_vis <- data$nodes %>%
        mutate(
          id = name,
          label = name,
          color = ifelse(type == "supplier", "red", "blue"),
          shape = ifelse(type == "supplier", "dot", "diamond"),
          title = paste0(
            "<strong>", name, "</strong><br>",
            "Type: ", type, "<br>",
            "Total Contracts: ", total_contracts, "<br>",
            "Total Award Amount: $", 
            format(total_award_amount, big.mark = ",", scientific = FALSE)
          ),
          size = rescale(total_contracts, to = c(10, 30))
        )
      
      # Prepare edges for visualization
      edges_vis <- data$edges %>%
        mutate(
          from = agency,
          to = supplier_name,
          value = if(input$edge_metric == "total_award_amount") {
            rescale(total_award_amount, to = c(1, 10))
          } else {
            rescale(total_contracts, to = c(1, 10))
          },
          title = paste0(
            "Agency: ", agency, "<br>",
            "Supplier: ", supplier_name, "<br>",
            "Total Contracts: ", total_contracts, "<br>",
            "Total Award Amount: $", 
            format(total_award_amount, big.mark = ",", scientific = FALSE)
          )
        )
      
      # Store in reactive value for use elsewhere
      network_vis_data(list(nodes = nodes_vis, edges = edges_vis))
      
      return(list(nodes = nodes_vis, edges = edges_vis))
    })
    
    #--------    
    # Network Visualization - main render function
    #--------
    output$network_plot <- renderVisNetwork({
      # Debug output
      print("renderVisNetwork called")
      
      # Check if update button has been clicked
      if (is.null(input$update_network) || input$update_network == 0) {
        # Default empty network before first update
        return(visNetwork(
          data.frame(id = 1, label = "Click Update to Visualize Network"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
      
      vis_data <- prepare_vis_data()
      
      print(paste("Visualization data has", 
                  ifelse(is.null(vis_data), "0", nrow(vis_data$nodes)), "nodes and", 
                  ifelse(is.null(vis_data), "0", nrow(vis_data$edges)), "edges"))
      
      # Check if we have data to visualize
      if (is.null(vis_data) || nrow(vis_data$nodes) == 0 || nrow(vis_data$edges) == 0) {
        return(visNetwork(
          data.frame(id = 1, label = "No data to display with current filters"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
      
      # Modify node sizes based on degree if checkbox is checked
      # Modify node sizes based on degree if checkbox is checked
      if (input$size_by_degree) {
        print("Calculating node sizes based on degree centrality")
        
        # Create an igraph object from the edges data frame
        # Make sure we use the from/to fields from the edges data
        graph <- igraph::graph_from_data_frame(
          d = data.frame(from = vis_data$edges$from, to = vis_data$edges$to),
          directed = TRUE
        )
        
        # Get all node names in the graph
        graph_nodes <- igraph::V(graph)$name
        print(paste("Graph created with", length(graph_nodes), "nodes"))
        
        # Calculate node degrees - use "all" mode to count both in and out degrees
        degrees <- igraph::degree(graph, mode = "all")
        print("Degree distribution summary:")
        print(summary(degrees))
        
        # Ensure the degrees match the node IDs in vis_data$nodes
        # Create a named vector for easy lookup
        degree_vector <- setNames(degrees, graph_nodes)
        
        # Apply the degrees to nodes with error checking
        vis_data$nodes$degree <- sapply(vis_data$nodes$id, function(id) {
          if(id %in% names(degree_vector)) {
            return(degree_vector[id])
          } else {
            print(paste("WARNING: Node ID not found in graph:", id))
            return(1) # Default degree
          }
        })
        
        # Add debugging to see the degree distribution
        print("Degrees assigned to nodes:")
        print(head(vis_data$nodes[, c("id", "degree")]))
        
        # Scale the degree values to a reasonable size range with more dramatic scaling
        min_size <- 5   # Minimum node size (reduced)
        max_size <- 40  # Maximum node size (increased)
        
        # Use a logarithmic scaling for degrees to better handle skewed distributions
        if(length(unique(vis_data$nodes$degree)) > 1) {
          # Apply log scaling to emphasize differences
          # Add 1 to avoid log(0) issues
          log_degrees <- log1p(vis_data$nodes$degree)
          
          # Apply the scaling
          vis_data$nodes$size <- min_size + 
            (log_degrees - min(log_degrees)) / (max(log_degrees) - min(log_degrees)) * 
            (max_size - min_size)
          
          # Round to nearest integer for cleaner sizes
          vis_data$nodes$size <- round(vis_data$nodes$size, 1)
          
          print("Size range after scaling:")
          print(range(vis_data$nodes$size))
        } else {
          # If all nodes have the same degree, use a default size
          vis_data$nodes$size <- 15
          print("All nodes have the same degree - using uniform size")
        }
      } else {
        # Use default uniform size if checkbox is unchecked
        vis_data$nodes$size <- 15
        print("Node sizing by degree disabled - using uniform size")
      }
      
      # Initial network visualization with base settings
      network <- visNetwork(vis_data$nodes, vis_data$edges, id = "network_plot") %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = list(enabled = TRUE)
        )
      
      # Apply physics settings based on performance mode
      if (input$performance_mode) {
        # Optimized physics for performance
        network <- network %>%
          visPhysics(
            solver = "forceAtlas2Based",
            stabilization = list(enabled = TRUE, iterations = 100),
            timestep = 0.5,
            adaptiveTimestep = TRUE,
            maxVelocity = 50
          )
      } else {
        # Regular physics settings for better visualization
        network <- network %>%
          visPhysics(
            solver = "forceAtlas2Based",
            stabilization = list(enabled = TRUE, iterations = 1000)
          )
      }
      
      # Add remaining visualization settings
      network %>%
        visNodes(
          shape = "dot",
          color = list(
            background = "#666666",
            border = "#2B7CE9"
          )
        ) %>%
        visEdges(
          arrows = "to",
          color = list(
            color = "#D3D3D3",
            highlight = "#FF0000"
          )
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          dragNodes = TRUE,
          zoomView = TRUE
        ) %>%
        visEvents(
          selectNode = sprintf("function(nodes) { 
    Shiny.setInputValue('%s', nodes.nodes[0]);
    Shiny.setInputValue('%s', 'Ego Network', {priority: 'event'});
  }", session$ns("selected_node"), session$ns("switch_to_ego"))
        )
    })
#-------------
    # Ego Network Visualization
#-------------
    
    # Ego Network
    ego_network_data <- reactive({
      # Debug information
      print("ego_network_data reactive called")
      
      # Check if selected_node exists
      if(is.null(input$selected_node)) {
        print("No node selected yet")
        return(NULL)
      }
      
      selected_node <- input$selected_node
      print(paste("Processing ego network for node:", selected_node))
      
      # Get the original visualization data (already prepared with from/to format)
      vis_data <- network_vis_data()
      
      # Make sure we have data to work with
      if(is.null(vis_data) || nrow(vis_data$edges) == 0) {
        print("No visualization data available")
        return(NULL)
      }
      
      # Get all nodes that connect directly to or from the selected node
      connected_from <- vis_data$edges$to[vis_data$edges$from == selected_node]
      connected_to <- vis_data$edges$from[vis_data$edges$to == selected_node]
      all_connected <- unique(c(connected_from, connected_to, selected_node))
      
      print(paste("Number of nodes connected to selected node:", length(all_connected)))
      
      # Filter edges for the ego network - must connect to/from the selected node
      ego_edges <- vis_data$edges %>%
        filter((from == selected_node) | (to == selected_node))
      
      # Filter nodes for the ego network
      ego_nodes <- vis_data$nodes %>%
        filter(id %in% all_connected)
      
      # Calculate degree centrality for the ego network
      if(input$size_by_degree && nrow(ego_nodes) > 1) {
        print("Calculating degree centrality for ego network")
        
        # Get ego network connectivity within the full network
        # Create a mini graph just for the ego network
        ego_graph <- igraph::graph_from_data_frame(
          d = ego_edges[, c("from", "to")],
          directed = TRUE
        )
        # Calculate degrees in this ego network
        ego_degrees <- igraph::degree(ego_graph, mode = "all")
        
        # Create a named vector for easy lookup
        degree_vector <- setNames(ego_degrees, igraph::V(ego_graph)$name)
        
        # Apply the degrees to nodes
        ego_nodes$degree <- sapply(ego_nodes$id, function(id) {
          if(id %in% names(degree_vector)) {
            return(degree_vector[id])
          } else {
            return(1) # Default degree
          }
        })
        
        # Scale the degree values - use a more dramatic scaling for the ego network
        min_size <- 10  # Minimum node size
        max_size <- 40  # Maximum node size
        
        if(length(unique(ego_nodes$degree)) > 1) {
          # Scale the sizes - focal node gets maximum size
          ego_nodes <- ego_nodes %>%
            mutate(
              size = ifelse(
                id == selected_node,
                max_size,  # Focal node is always largest
                min_size + (degree - min(degree)) / (max(degree) - min(degree)) * (max_size - min_size - 5)
              )
            )
        } else {
          # Fallback if all nodes have same degree
          ego_nodes$size <- ifelse(ego_nodes$id == selected_node, 35, 20)
        }
      } else {
        # Default sizing if not using degree centrality
        ego_nodes$size <- ifelse(ego_nodes$id == selected_node, 35, 20)
      }
      
      # Add color coding
      ego_nodes <- ego_nodes %>%
        mutate(
          # Mark the selected node
          is_focal = (id == selected_node),
          # Enhanced styling for better visualization
          color = case_when(
            is_focal ~ "#F3c623",  # Bright green for focal node
            type == "supplier" ~ "#E4003A", 
            type == "agency" ~ "#A6CDC6",    
            TRUE ~ color  # Keep original color otherwise
          ),
          shape = case_when(
            is_focal ~ "star",     # Star shape for the focal node
            type == "supplier" ~ "dot",  
            type == "agency" ~ "diamond",       
            TRUE ~ "dot"          
          )
        )          
        
      # Add safety check for empty results
      if(nrow(ego_nodes) == 0 || nrow(ego_edges) == 0) {
        print("No ego network data found")
        return(NULL)
      }
      
      print(paste("Ego network has", nrow(ego_nodes), "nodes and", nrow(ego_edges), "edges"))
      
      return(list(nodes = ego_nodes, edges = ego_edges))
    })
    
#----
# Network metrics tables (tab 1)
#----    
    
    # Network Summary Statistics
    output$network_summary <- renderPrint({
      # Make sure we have data
      data <- filtered_data()
      req(data)
      req(nrow(data$nodes) > 0)
      
      # Calculate key statistics
      num_agencies <- sum(data$nodes$type == "agency")
      num_suppliers <- sum(data$nodes$type == "supplier") 
      total_contracts <- sum(data$edges$total_contracts)
      total_award_amount <- sum(data$edges$total_award_amount)
      
      # Create summary data frame
      summary_df <- data.frame(
        Metric = c(
          "Total Nodes",
          "Agencies",
          "Suppliers",
          "Total Connections",
          "Total Contracts",
          "Total Award Amount"
        ),
        Value = c(
          nrow(data$nodes),
          num_agencies,
          num_suppliers,
          nrow(data$edges),
          total_contracts,
          paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      )
      
      # Print the summary
      print(summary_df, row.names = FALSE)
    })
    
    # Agency Table
    output$agency_table <- renderDT({
      data <- filtered_data()
      req(data)
      
      # Filter for agencies 
      agencies <- data$nodes %>%
        filter(type == "agency") %>%
        select(name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      # Format for display
      agencies$total_award_amount <- paste0("$", format(agencies$total_award_amount, big.mark = ",", scientific = FALSE))
      
      # Rename columns 
      colnames(agencies) <- c("Agency Name", "Total Contracts", "Total Award Amount")
      
      # Return the data table with options
      datatable(
        agencies,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          scrollX = TRUE
        )
      )
    })
    
    # Supplier Table
    output$supplier_table <- renderDT({
      data <- filtered_data()
      req(data)
      
      # Filter for suppliers 
      suppliers <- data$nodes %>%
        filter(type == "supplier") %>%
        select(name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      suppliers$total_award_amount <- paste0("$", format(suppliers$total_award_amount, big.mark = ",", scientific = FALSE))
      
      colnames(suppliers) <- c("Supplier Name", "Total Contracts", "Total Award Amount")
      
      datatable(
        suppliers,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          scrollX = TRUE
        )
      )
    })
    
    # Top Contracts Table
    output$contract_table <- renderDT({
      data <- filtered_data()
      req(data)
      
      # Check if tender_cat column exists in the edges data
      if ("tender_cat" %in% names(data$edges)) {
        top_contracts <- data$edges %>%
          select(agency, supplier_name, tender_cat, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
      } else {
        # Fallback if tender_cat doesn't exist
        top_contracts <- data$edges %>%
          select(agency, supplier_name, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
        # Add column for tender category
        top_contracts$tender_cat <- "Not available"
      }
      
      top_contracts$total_award_amount <- paste0("$", format(top_contracts$total_award_amount, big.mark = ",", scientific = FALSE))
      
      # Rename columns for better presentation
      colnames(top_contracts) <- c("Agency", "Supplier", "Tender Category", "Contract Count", "Award Amount")
      
      datatable(
        top_contracts,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          scrollX = TRUE
        )
      )
    })   
    #-----------------
    # Ego visual output
    #-----------------
    output$ego_network_plot <- renderVisNetwork({
      ego_data <- ego_network_data()
      
      # Validate we have data
      req(ego_data)
      req(nrow(ego_data$nodes) > 0 && nrow(ego_data$edges) > 0)
      
      print(paste("Rendering ego network with", nrow(ego_data$nodes), "nodes and", nrow(ego_data$edges), "edges"))

      # Add degree info to tooltip if it exists
      if("degree" %in% names(ego_data$nodes)) {
        ego_data$nodes$title <- paste0(
          ego_data$nodes$title,
          "<br>Degree: ", ego_data$nodes$degree
        )
      }
                  
      # Create the visualization
      visNetwork(ego_data$nodes, ego_data$edges) %>%
        visEdges(
          arrows = "to",
          color = list(
            color = "#D3D3D3",
            highlight = "#FF0000"
          ),
          smooth = TRUE
        ) %>%
        visNodes(
          shape = "dot",
          shadow = FALSE  
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          selectedBy = "label"  # Allow selection by node label
        ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = list(enabled = TRUE,
                               iterations = if(input$performance_mode) 100 else 1000
                               ) 
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          dragNodes = TRUE,
          zoomView = TRUE
        ) %>%
        visLayout(randomSeed = 123)  # Consistent layout
    })
    
    # Ego Network Metrics
    output$ego_metrics <- renderPrint({
      ego_data <- ego_network_data()
      
      req(ego_data)  # Ensure ego_data exists
      req(nrow(ego_data$nodes) > 0)
      
      # Get focal node info
      focal_node <- ego_data$nodes$id[ego_data$nodes$id == input$selected_node]
      focal_type <- ego_data$nodes$type[ego_data$nodes$id == input$selected_node]
      
      # Count node types
      num_agencies <- sum(ego_data$nodes$type == "agency")
      num_suppliers <- sum(ego_data$nodes$type == "supplier")
      
      # Calculate totals
      total_contracts <- sum(ego_data$edges$total_contracts)
      total_award_amount <- sum(ego_data$edges$total_award_amount)
      
      # Prepare metrics
      metrics_df <- data.frame(
        Metric = c(
          "Focal Node", 
          "Node Type",
          "Total Connected Nodes",
          "Connected Agencies",
          "Connected Suppliers",
          "Total Contracts",
          "Total Award Amount"
        ),
        Value = c(
          as.character(focal_node),
          as.character(focal_type),
          nrow(ego_data$nodes),
          num_agencies,
          num_suppliers,
          total_contracts,
          paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      )
      # Print the metrics in a nice format
      print(metrics_df, row.names = FALSE)
    })

#----
    # Ego Network Agency Table
#----
    
    output$ego_agency_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      req(nrow(ego_data$nodes) > 0)
      
      # Get selected node info
      selected_node <- input$selected_node
      selected_type <- ego_data$nodes$type[ego_data$nodes$id == selected_node]
      
      # Always include the focal agency
      if (selected_type == "agency") {
        connected_agencies <- ego_data$nodes %>%
          filter(type == "agency")  # Include all agencies, including the selected one
        
        connected_agencies <- connected_agencies %>%
          mutate(is_focal = (id == selected_node))
      } else {
        # If selected node is a supplier, show all connected agencies
        connected_agencies <- ego_data$nodes %>%
          filter(type == "agency")
        
        # Since none of the agencies are the focal node
        connected_agencies$is_focal <- FALSE
      }
      
      if (nrow(connected_agencies) == 0) {
        return(datatable(
          data.frame(Message = "No connected agencies found"),
          options = list(dom = 't')
        ))
      }
      
      # Format agency data
      connected_agencies <- connected_agencies %>%
        select(id, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      # Format currency
      connected_agencies$total_award_amount <- paste0("$", format(connected_agencies$total_award_amount, big.mark = ",", scientific = FALSE))
      
      colnames(connected_agencies) <- c("Agency Name", "Total Contracts", "Total Award Amount")
      
      # datatable
      datatable(
        connected_agencies,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          scrollX = TRUE
        )
      )
    })
    
    # Ego Network Supplier Table
    output$ego_supplier_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      req(nrow(ego_data$nodes) > 0)
      
      selected_node <- input$selected_node
      selected_type <- ego_data$nodes$type[ego_data$nodes$id == selected_node]
      
      # Always include the focal supplier
      if (selected_type == "supplier") {
        # If selected node is a supplier, it should be included in the supplier table
        connected_suppliers <- ego_data$nodes %>%
          filter(type == "supplier")  # Include all suppliers, including the selected one
        
        # Mark the focal supplier for highlighting
        connected_suppliers <- connected_suppliers %>%
          mutate(is_focal = (id == selected_node))
      } else {
        # If selected node is an agency, show all connected suppliers
        connected_suppliers <- ego_data$nodes %>%
          filter(type == "supplier")
        
        # Since none of the suppliers are the focal node
        connected_suppliers$is_focal <- FALSE
      }
      
      # If there are no connected suppliers, return empty table with message
      if (nrow(connected_suppliers) == 0) {
        return(datatable(
          data.frame(Message = "No connected suppliers found"),
          options = list(dom = 't')
        ))
      }
      
      connected_suppliers <- connected_suppliers %>%
        select(id, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      connected_suppliers$total_award_amount <- paste0("$", format(connected_suppliers$total_award_amount, big.mark = ",", scientific = FALSE))
      
      colnames(connected_suppliers) <- c("Supplier Name", "Total Contracts", "Total Award Amount")
      
      datatable(
        connected_suppliers,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          scrollX = TRUE
        )
      )
    })
    
    # Ego Network Connections Table 
    output$ego_connection_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      req(nrow(ego_data$edges) > 0)
      
      connections <- ego_data$edges
      
      # Check if tender_cat exists
      if ("tender_cat" %in% names(connections)) {
        # Select relevant columns including tender category
        connections <- connections %>%
          select(from, to, tender_cat, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
      } else {
        # Select relevant columns without tender category
        connections <- connections %>%
          select(from, to, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
        
        # placeholder column
        connections$tender_cat <- "Not available"
      }

      connections$total_award_amount <- paste0("$", format(connections$total_award_amount, big.mark = ",", scientific = FALSE))
      
      colnames(connections) <- c("From", "To", "Tender Category", "Contract Count", "Award Amount")

      datatable(
        connections,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          scrollX = TRUE
        )
      )
    })
        
    output$no_node_selected_message <- renderUI({
      if(is.null(input$selected_node)) {
        div(
          style = "text-align: center; margin-top: 50px; color: #888;",
          h3("No Node Selected"),
          p("Click on a node in the Network Overview tab to view its ego network.")
        )
      } else {
        return(NULL)  # Return nothing if a node is selected
      }
    })
})
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Procurement Network Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Network Visualization", tabName = "network", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
        /* Main content area - increase max-width */
        .content-wrapper {
          max-width: 100% !important;
        }
        
        /* Make the main container fluid */
        .container-fluid {
          width: 95% !important;
          max-width: 2000px !important;
          margin-left: auto;
          margin-right: auto;
        }
        
        /* Ensure boxes use full width */
        .box {
          width: 100% !important;
        }
        
        /* Make tab content full width */
        .tab-content, .tab-pane {
          width: 100% !important;
        }
        
        /* DataTables width fix */
        .dataTables_wrapper {
          width: 100% !important;
        }
        
        /* Ensure network visualizations use full width */
        .vis-network {
          width: 100% !important;
        }
      ")),
  tabItems(
    tabItem(tabName = "network",
            fluidRow(
              column(width = 12, network_analysis_ui("network_module"))
            )
    )
  )
  )
)

# Server
server <- function(input, output, session) {
  network_analysis_server("network_module")
}

# Run the application 
shinyApp(ui, server)