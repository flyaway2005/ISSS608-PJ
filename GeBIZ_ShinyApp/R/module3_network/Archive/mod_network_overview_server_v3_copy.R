#--------------------------------
# Network Analysis Server Module
#--------------------------------

network_analysis_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    #<---start    
    raw_data <- readRDS("data/m3_processed_network_data.rds")
    str(raw_data)
    print("==== Contents of raw_data ====")
    print(names(raw_data))
    
    # Create reactive list
    network_data <- reactive({
      list(
        edges = raw_data$monthly_edges,        
        nodes = raw_data$nodes,  
        months = raw_data$months,
        agency_types = raw_data$agency_types,
        month_index = raw_data$month_index
      )
    })
    
    # Set default values when the module loads
    observe({
      
      if (is.null(input$max_edges)) {
        updateSliderInput(session, "max_edges", value = 100)
      }
      
      # Set default date range to April 2019 - March 2024
      updateSelectInput(session, "start_year", selected = "2019")
      updateSelectInput(session, "start_month", selected = 4)
      updateSelectInput(session, "end_year", selected = "2024")
      updateSelectInput(session, "end_month", selected = 3)
    })
    
    # Initialize filters when the module loads
    observe({
      req(network_data())
      
      # Update agency type filter
      agency_types <- c("All", unique(network_data()$agency_types$agency_type))
      updateSelectizeInput(session, "agency_type_filter", choices = agency_types, selected = "All")
      
      # Update agency filter 
      agencies <- c("All", sort(unique(network_data()$edges$agency)))
      updateSelectizeInput(session, "agency_filter", choices = agencies, selected = "All")
      
      # Update supplier filter
      suppliers <- c("All", sort(unique(network_data()$edges$supplier_name)))
      updateSelectizeInput(session, "supplier_filter", choices = suppliers, selected = "All")
      
      # Update tender category filter
      if ("tender_cat" %in% names(network_data()$edges)) {
        tender_cats <- c("All", sort(unique(network_data()$edges$tender_cat)))
        updateSelectizeInput(session, "tender_cat_filter", choices = tender_cats, selected = "All")
      }
      
      # Update award amount range
      min_amount <- floor(min(network_data()$edges$total_award_amount, na.rm = TRUE))
      max_amount <- ceiling(max(network_data()$edges$total_award_amount, na.rm = TRUE))
      
      updateSliderInput(session, "award_amount_range", min = min_amount, max = max_amount, 
                        value = c(min_amount, max_amount), step = max(1, (max_amount - min_amount) / 100))
      
      updateNumericInput(session, "min_award_manual", value = min_amount)
      updateNumericInput(session, "max_award_manual", value = max_amount)
      })
    
    # prevent observer loops
    in_update <- reactiveVal(FALSE)
      
      # Observe slider changes and update manual inputs
    observeEvent(input$award_amount_range, {
      if (!in_update()) {
        in_update(TRUE)
        updateNumericInput(session, "min_award_manual", value = input$award_amount_range[1])
        updateNumericInput(session, "max_award_manual", value = input$award_amount_range[2])
        in_update(FALSE)
      }
    }, ignoreInit = TRUE)
    
      
      # Observe manual input changes and update slider
    observeEvent(input$min_award_manual, {
      if (!in_update()) {
        in_update(TRUE)
        # Get the current slider values
        curr_values <- input$award_amount_range
        # Update only the first value of the slider
        curr_values[1] <- input$min_award_manual
        # Update the slider
        updateSliderInput(session, "award_amount_range", value = curr_values)
        in_update(FALSE)
      }
    }, ignoreInit = TRUE)
      
    observeEvent(input$max_award_manual, {
      if (!in_update()) {
        in_update(TRUE)
        # Get the current slider values
        curr_values <- input$award_amount_range
        # Update only the second value of the slider
        curr_values[2] <- input$max_award_manual
        # Update the slider
        updateSliderInput(session, "award_amount_range", value = curr_values)
        in_update(FALSE)
      }
    }, ignoreInit = TRUE)
    
    # Enforce date range restrictions
    observe({
      # Start date restrictions
      if (input$start_year == "2019") {
        # If 2019 is selected, only allow April-December
        updateSelectInput(session, "start_month",
                         choices = c("04" = 4, "05" = 5, "06" = 6, "07" = 7, "08" = 8, 
                                    "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                         selected = ifelse(as.numeric(input$start_month) < 4, 4, input$start_month))
      } else if (input$start_year == "2024") {
        # If 2024 is selected, only allow January-March
        updateSelectInput(session, "start_month",
                         choices = c("01" = 1, "02" = 2, "03" = 3),
                         selected = ifelse(as.numeric(input$start_month) > 3, 3, input$start_month))
      } else {
        # For years 2020-2023, all months are valid
        updateSelectInput(session, "start_month",
                         choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                    "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                         selected = input$start_month)
      }
      
      # End date restrictions
      if (input$end_year == "2019") {
        # If 2019 is selected, only allow April-December
        updateSelectInput(session, "end_month",
                         choices = c("04" = 4, "05" = 5, "06" = 6, "07" = 7, "08" = 8, 
                                    "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                         selected = ifelse(as.numeric(input$end_month) < 4, 4, input$end_month))
      } else if (input$end_year == "2024") {
        # If 2024 is selected, only allow January-March
        updateSelectInput(session, "end_month",
                         choices = c("01" = 1, "02" = 2, "03" = 3),
                         selected = ifelse(as.numeric(input$end_month) > 3, 3, input$end_month))
      } else {
        # For years 2020-2023, all months are valid
        updateSelectInput(session, "end_month",
                         choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                    "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                         selected = input$end_month)
      }
      
      # Ensure start date is not after end date
      start_idx <- as.numeric(input$start_year) * 12 + as.numeric(input$start_month)
      end_idx <- as.numeric(input$end_year) * 12 + as.numeric(input$end_month)
      
      if (start_idx > end_idx) {
        # If start date is after end date, adjust the end date to match start date
        updateSelectInput(session, "end_year", selected = input$start_year)
        updateSelectInput(session, "end_month", selected = input$start_month)
      }
    })
    
    # Define filtered_edges once - consolidated version
    filtered_edges <- eventReactive(input$update_network, {
      tryCatch({
        req(network_data())
        edges <- network_data()$edges
        
        # Enforce date range restriction (April 2019 to March 2024)
        start_idx <- 2019 * 12 + 4  # April 2019
        end_idx <- 2024 * 12 + 3    # March 2024
        
        # Build month index with default values if inputs dont exist
        # Override user inputs with the enforced date range
        start_year <- 2019
        start_month <- 4
        end_year <- 2024
        end_month <- 3
        
        # Filter by time
        filtered <- edges %>%
          filter(month_index >= start_idx & month_index <= end_idx)
        
        # Apply other filters
        if (!"All" %in% input$agency_type_filter) {
          # Join with agency_types to get the agency_type
          agency_types_df <- network_data()$agency_types
          filtered <- filtered %>% 
            left_join(agency_types_df, by = "agency") %>%
            filter(agency_type %in% input$agency_type_filter)
        }
        
        if (!"All" %in% input$agency_filter) {
          filtered <- filtered %>% filter(agency %in% input$agency_filter)
        }
        if (!"All" %in% input$supplier_filter) {
          filtered <- filtered %>% filter(supplier_name %in% input$supplier_filter)
        }
        if ("tender_cat" %in% names(filtered) && !"All" %in% input$tender_cat_filter) {
          filtered <- filtered %>% filter(tender_cat %in% input$tender_cat_filter)
        }
        
        # Filter by award amount range
        filtered <- filtered %>%
          filter(total_award_amount >= input$award_amount_range[1],
                 total_award_amount <= input$award_amount_range[2])
        
        # ADD DEBUG OUTPUT
        print(paste("Filtered edges result:", nrow(filtered), "rows"))
        
        return(filtered)
      }, error = function(e) {
        print(paste("Filter error:", e$message))
        return(data.frame())
      })
    })
    
    # Define network_graph_data once - consolidated version
    network_graph_data <- reactive({
      edges <- filtered_edges()
      req(edges)
      if (nrow(edges) == 0) return(list(nodes = data.frame(), edges = data.frame()))
      
      # Check if required ID columns exist
      if (!"agency_id" %in% colnames(edges)) {
        # Create agency_id using the nodes data
        nodes_all <- network_data()$nodes
        agency_mapping <- data.frame(
          agency = unique(edges$agency),
          agency_id = paste0("agency_", seq_along(unique(edges$agency)))
        )
        edges <- edges %>% left_join(agency_mapping, by = "agency")
      }
      
      if (!"supplier_id" %in% colnames(edges)) {
        # Create supplier_id using the nodes data
        supplier_mapping <- data.frame(
          supplier_name = unique(edges$supplier_name),
          supplier_id = paste0("supplier_", seq_along(unique(edges$supplier_name)))
        )
        edges <- edges %>% left_join(supplier_mapping, by = "supplier_name")
      }
      
      # SAFER SUMMARIZATION WITH ERROR HANDLING
      final_edges <- tryCatch({
        edges %>%
          group_by(agency, supplier_name, agency_id, supplier_id) %>%
          summarise(
            total_contracts = sum(total_contracts, na.rm = TRUE),
            total_award_amount = sum(total_award_amount, na.rm = TRUE),
            # This line problematic now
#            tender_cat = if(has_tender_cat) first(tender_cat) else NA_character_,
            .groups = "drop"
          ) %>%
          mutate(from = agency_id, to = supplier_id) %>%
          select(from, to, agency, supplier_name, 
                 # CONDITIONAL COLUMN SELECTION
#                 if(has_tender_cat) tender_cat else NULL, 
                 total_contracts, total_award_amount)
      }, error = function(e) {
        print(paste("Error creating final edges:", e$message))
        # CREATE MINIMAL VALID EDGE DATA
        data.frame(
          from = character(0),
          to = character(0),
          agency = character(0),
          supplier_name = character(0),
          total_contracts = integer(0),
          total_award_amount = numeric(0),
          stringsAsFactors = FALSE
        )
      })
      
      # EARLY EXIT IF NO EDGES
      if (nrow(final_edges) == 0) {
        print("Warning: No edges after summarization")
        return(list(
          nodes = data.frame(id=character(0), name=character(0), type=character(0)),
          edges = data.frame()
        ))
      }
      
      # Create the nodes dataframe
      # Extract unique agencies and suppliers
      agencies <- data.frame(
        id = unique(final_edges$from),
        name = unique(final_edges$agency),
        type = "agency",
        stringsAsFactors = FALSE
      )
      
      suppliers <- data.frame(
        id = unique(final_edges$to),
        name = unique(final_edges$supplier_name),
        type = "supplier",
        stringsAsFactors = FALSE
      )
      
      # SAFELY COMBINE INTO ONE NODES DATAFRAME
      nodes <- tryCatch({
        bind_rows(agencies, suppliers)
      }, error = function(e) {
        print(paste("Error binding node rows:", e$message))
        # CREATE VALID FALLBACK
        data.frame(
          id = c(agencies$id, suppliers$id),
          name = c(agencies$name, suppliers$name),
          type = c(rep("agency", length(agencies$id)), 
                   rep("supplier", length(suppliers$id))),
          stringsAsFactors = FALSE
        )
      })
      
      # SAFER NODE METRICS CALCULATION
      node_metrics <- tryCatch({
        bind_rows(
          # Agency metrics
          final_edges %>%
            group_by(agency) %>%
            summarise(
              total_contracts = sum(total_contracts, na.rm = TRUE),
              total_award_amount = sum(total_award_amount, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            rename(name = agency),
          
          # Supplier metrics
          final_edges %>%
            group_by(supplier_name) %>%
            summarise(
              total_contracts = sum(total_contracts, na.rm = TRUE),
              total_award_amount = sum(total_award_amount, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            rename(name = supplier_name)
        )
      }, error = function(e) {
        print(paste("Error calculating node metrics:", e$message))
        data.frame(
          name = nodes$name,
          total_contracts = 0,
          total_award_amount = 0
        )
      })
      
      # SAFER JOIN OF METRICS WITH NODES
      nodes <- tryCatch({
        nodes %>% 
          left_join(node_metrics, by = "name")
      }, error = function(e) {
        print(paste("Error joining node metrics:", e$message))
        nodes$total_contracts <- 0
        nodes$total_award_amount <- 0
        nodes
      })
      
      # VERIFY THE TYPE COLUMN EXISTS
      if (!"type" %in% colnames(nodes)) {
        print("ERROR: 'type' column missing from nodes dataframe")
        # ADD TYPE COLUMN IF MISSING
        nodes$type <- ifelse(grepl("^agency_", nodes$id), "agency", "supplier")
      }
      
      # PRINT DEBUG INFO ABOUT RESULT
      print(paste("network_graph_data returning", nrow(nodes), "nodes and", nrow(final_edges), "edges"))
      
      list(nodes = nodes, edges = final_edges)
    })
    
    # Define filtered_data as an alias to network_graph_data for backward compatibility
    filtered_data <- reactive({
      network_graph_data()
    })
    
    # Visualisation preparation
    network_vis_data <- reactive({
      # Get the base data
      data <- network_graph_data()
      req(data)
      req(nrow(data$nodes) > 0, nrow(data$edges) > 0)
      
      # Debug original data
      print(paste("Original network data has", nrow(data$nodes), "nodes and", nrow(data$edges), "edges"))
      
      # Default values if inputs are NULL
      max_edges <- if (is.null(input$max_edges)) 500 else input$max_edges
      edge_metric <- if (is.null(input$edge_metric)) "total_award_amount" else input$edge_metric
      
      # Apply edge limiting if needed
      all_edges <- data$edges
      limited_edges <- all_edges
      
      if (nrow(all_edges) > max_edges) {
        # Sort edges by the chosen metric and take top N
        limited_edges <- all_edges %>%
          arrange(desc(if (edge_metric == "total_award_amount") total_award_amount else total_contracts)) %>%
          slice_head(n = max_edges)
        
        print(paste("Step 1: Limited edges from", nrow(all_edges), "to", nrow(limited_edges), "edges"))
      }
      
      # Get the node IDs that are connected in the limited edge set
      from_nodes <- unique(limited_edges$from)
      to_nodes <- unique(limited_edges$to)
      connected_node_ids <- unique(c(from_nodes, to_nodes))
      
      print(paste("Step 2: Connected node IDs:", length(connected_node_ids), "unique IDs"))
      
      # Filter nodes to only include those that are connected by the limited edges
      filtered_nodes <- data$nodes %>%
        filter(id %in% connected_node_ids)
      
      print(paste("Step 3: Filtered nodes from", nrow(data$nodes), "to", nrow(filtered_nodes), "nodes"))
      
      # Verify that all nodes in filtered_nodes are actually connected by an edge
      node_ids_in_filtered <- filtered_nodes$id
      
      # Double-check all nodes have a connection
      for (node_id in node_ids_in_filtered) {
        has_connection <- any(limited_edges$from == node_id | limited_edges$to == node_id)
        if (!has_connection) {
          print(paste("WARNING: Node", node_id, "has no connections but is included in filtered nodes"))
        }
      }
      
      # Double-check all edges reference valid nodes
      for (i in 1:nrow(limited_edges)) {
        from_id <- limited_edges$from[i]
        to_id <- limited_edges$to[i]
        
        if (!(from_id %in% node_ids_in_filtered)) {
          print(paste("WARNING: Edge references from node", from_id, "that is not in filtered nodes"))
        }
        
        if (!(to_id %in% node_ids_in_filtered)) {
          print(paste("WARNING: Edge references to node", to_id, "that is not in filtered nodes"))
        }
      }
      
      # Prepare nodes for visualization
      nodes_vis <- filtered_nodes %>%
        mutate(
          label = name,
          color = ifelse(type == "supplier", "#E4003A", "#A6CDC6"),
          shape = ifelse(type == "supplier", "dot", "diamond"),
          title = paste0(
            "<strong>", name, "</strong><br>",
            "Type: ", type, "<br>",
            "Total Contracts: ", total_contracts, "<br>",
            "Total Award Amount: $",
            format(total_award_amount, big.mark = ",", scientific = FALSE)
          ),
          size = if (length(unique(total_contracts)) > 1) {
            scales::rescale(total_contracts, to = c(10, 50))
          } else {
            15  # Default size if all values are the same
          }
        )
      
      # Prepare edges for visualization
      edges_vis <- limited_edges %>%
        mutate(
          value = if (edge_metric == "total_award_amount") {
            if (length(unique(total_award_amount)) > 1) {
              scales::rescale(total_award_amount, to = c(1, 10))
            } else { 5 }
          } else {
            if (length(unique(total_contracts)) > 1) {
              scales::rescale(total_contracts, to = c(1, 10))
            } else { 5 }
          },
          title = paste0(
            "Agency: ", agency, "<br>",
            "Supplier: ", supplier_name, "<br>",
            "Total Contracts: ", total_contracts, "<br>",
            "Total Award Amount: $",
            format(total_award_amount, big.mark = ",", scientific = FALSE)
          )
        )
      
      # Final verification
      print(paste("Step 4: Final visualization data has", nrow(nodes_vis), "nodes and", nrow(edges_vis), "edges"))
      
      # Create the result with tracking information
      result <- list(
        nodes = nodes_vis, 
        edges = edges_vis, 
        limited = nrow(all_edges) > max_edges,
        original_edge_count = nrow(all_edges),
        limited_edge_count = nrow(limited_edges),
        original_node_count = nrow(data$nodes),
        limited_node_count = nrow(filtered_nodes)
      )
      
      return(result)
    })
    
    # Update the renderVisNetwork function to use the correct edge data
    # Replace the beginning of the renderVisNetwork function with:
    
    output$network_plot <- renderVisNetwork({
      print("Rendering visNetwork...")
      
      vis_data <- network_vis_data()
      
      # Check if we have data at all
      if (is.null(vis_data)) {
        print("WARNING: network_vis_data() returned NULL")
        # Return a simple message instead of crashing
        return(visNetwork(
          data.frame(id = 1, label = "No data to display with current filters"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
      
      # Check if data has the right structure (nodes and edges components)
      if (!all(c("nodes", "edges") %in% names(vis_data))) {
        print("ERROR: vis_data missing nodes or edges component")
        return(visNetwork(
          data.frame(id = 1, label = "Invalid data structure"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
      
      # Check if there's actually data in the structure
      if (nrow(vis_data$nodes) == 0 || nrow(vis_data$edges) == 0) {
        print("WARNING: Empty nodes or edges in vis_data")
        return(visNetwork(
          data.frame(id = 1, label = "No data to display with current filters"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
    
    # Add an output to display information about edge limiting
      # Update edge_limit_info to include node info
      output$edge_limit_info <- renderUI({
        vis_data <- network_vis_data()
        req(vis_data)
        
        # Show the message when edges are limited
        if (vis_data$limited) {
          total_edges <- vis_data$original_edge_count
          displayed_edges <- vis_data$limited_edge_count
          total_nodes <- vis_data$original_node_count
          displayed_nodes <- vis_data$limited_node_count
          
          div(
            style = "margin-top: 10px; color: #666; font-style: italic;",
            HTML(sprintf("Displaying %d of %d total edges (%.1f%%)<br>Showing %d of %d nodes that are connected by these edges", 
                         displayed_edges, total_edges, 
                         100 * displayed_edges / total_edges,
                         displayed_nodes, total_nodes))
          )
        }
      })
    
    #--------    
    # Network Visualisation - main render function
    #--------
      output$network_plot <- renderVisNetwork({
        print("Rendering visNetwork...")
        
        vis_data <- network_vis_data()
        
        # Debug what we're receiving for visualization
        print(paste("Visualizing network with", 
                    nrow(vis_data$nodes), "nodes and", 
                    nrow(vis_data$edges), "edges"))
        
        # Check if we have data at all
        if (is.null(vis_data)) {
          print("WARNING: network_vis_data() returned NULL")
          return(visNetwork(
            data.frame(id = 1, label = "No data to display with current filters"),
            data.frame(from = integer(0), to = integer(0))
          ))
        }
        
        # Check if there's actually data in the structure
        if (nrow(vis_data$nodes) == 0 || nrow(vis_data$edges) == 0) {
          print("WARNING: Empty nodes or edges in vis_data")
          return(visNetwork(
            data.frame(id = 1, label = "No data to display with current filters"),
            data.frame(from = integer(0), to = integer(0))
          ))
        }
        
        # Build visNetwork - directly using the nodes and edges from vis_data
        network <- visNetwork(
          nodes = vis_data$nodes,
          edges = vis_data$edges
        ) %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
            nodesIdSelection = list(enabled = TRUE)
          ) %>%
          visEdges(
            arrows = "to",
            color = list(color = "#D3D3D3", highlight = "#FF0000")
          ) %>%
          visNodes(
            color = list(background = "#666666", border = "#2B7CE9")
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
        
        # Apply physics layout
        if (input$layout_type == "force") {
          network <- network %>%
            visPhysics(
              solver = "forceAtlas2Based",
              stabilization = list(enabled = TRUE, iterations = 500)
            )
        } else if (input$layout_type == "repulsion") {
          network <- network %>%
            visPhysics(
              solver = "repulsion",
              repulsion = list(nodeDistance = 200),
              stabilization = TRUE
            )
        } else if (input$layout_type == "barnesHut") {
          network <- network %>%
            visPhysics(
              solver = "barnesHut",
              barnesHut = list(gravitationalConstant = -3000),
              stabilization = TRUE
            )
        }
        
        network
      })
    
    #-----------
    # Summary
    #-----------
      output$network_summary <- renderPrint({
        vis_data <- network_vis_data()
        req(vis_data)
        req(nrow(vis_data$nodes) > 0)
        
        # Calculate key statistics based on what's actually being displayed
        num_agencies <- sum(vis_data$nodes$type == "agency")
        num_suppliers <- sum(vis_data$nodes$type == "supplier") 
        total_contracts <- sum(vis_data$edges$total_contracts)
        total_award_amount <- sum(vis_data$edges$total_award_amount)
        
        summary_df <- data.frame(
          Metric = c(
            "Total Nodes",
            "Agencies",
            "Suppliers",
            "Total Connections (Displayed)",
            "Total Contracts (Displayed)",
            "Total Award Amount (Displayed)"
          ),
          Value = c(
            nrow(vis_data$nodes),
            num_agencies,
            num_suppliers,
            nrow(vis_data$edges),
            total_contracts,
            paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
          )
        )
        
        # Add information about edge limiting if applicable
        if (vis_data$limited) {
          extra_row <- data.frame(
            Metric = "Total Available Connections",
            Value = as.character(vis_data$original_edge_count)
          )
          summary_df <- rbind(summary_df, extra_row)
        }
        
        # Print the summary
        print(summary_df, row.names = FALSE)
      })
    
    # Rest of the code remains the same...
    # Agency table, Supplier table, Contract table and Ego network visualizations
    
    #------------
    # Network Metric tables
    #------------
    
    # Agency table
    output$agency_table <- renderDT({
      # ADD DEBUG
      print("Rendering agency table")
      
      data <- filtered_data()
      req(data)
      
      # CHECK FOR EMPTY DATA
      if (is.null(data$nodes) || nrow(data$nodes) == 0) {
        return(datatable(data.frame(Message = "No data available with current filters"), 
                         options = list(dom = 't')))
      }
      
      # VALIDATE REQUIRED COLUMNS EXIST
      if (!"type" %in% colnames(data$nodes)) {
        print("ERROR: 'type' column missing in nodes data")
        return(datatable(data.frame(Message = "Data structure error: missing 'type' column"), 
                         options = list(dom = 't')))
      }
      
      # SAFER FILTERING WITH TRYCATCH
      agencies <- tryCatch({
        data$nodes %>%
          filter(type == "agency") %>%
          select(name, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
      }, error = function(e) {
        print(paste("Agency table error:", e$message))
        data.frame(
          name = character(0),
          total_contracts = integer(0),
          total_award_amount = numeric(0)
        )
      })
      
      # CHECK FOR EMPTY RESULT
      if (nrow(agencies) == 0) {
        return(datatable(data.frame(Message = "No agency data available with current filters"), 
                         options = list(dom = 't')))
      }
      
      # SAFELY FORMAT VALUES
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
    
    # Supplier table
    output$supplier_table <- renderDT({
      # ADD DEBUG
      print("Rendering supplier table")
      
      data <- filtered_data()
      req(data)
      
      # VALIDATE DATA STRUCTURE
      if (is.null(data$nodes) || nrow(data$nodes) == 0) {
        return(datatable(data.frame(Message = "No data available with current filters"), 
                         options = list(dom = 't')))
      }
      
      # VALIDATE REQUIRED COLUMNS
      if (!"type" %in% colnames(data$nodes)) {
        print("ERROR: 'type' column missing in nodes data")
        return(datatable(data.frame(Message = "Data structure error: missing 'type' column"), 
                         options = list(dom = 't')))
      }
      
      # SAFER FILTERING WITH TRYCATCH
      suppliers <- tryCatch({
        data$nodes %>%
          filter(type == "supplier") %>%
          select(name, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
      }, error = function(e) {
        print(paste("Supplier table error:", e$message))
        data.frame(
          name = character(0),
          total_contracts = integer(0),
          total_award_amount = numeric(0)
        )
      })
      
      # CHECK FOR EMPTY RESULT
      if (nrow(suppliers) == 0) {
        return(datatable(data.frame(Message = "No supplier data available with current filters"), 
                         options = list(dom = 't')))
      }
      
      # SAFELY FORMAT VALUES
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
    
    # Contract table
    output$contract_table <- renderDT({
      vis_data <- network_vis_data()
      req(vis_data)
      
      # Check if we have edge data
      if (is.null(vis_data$edges) || nrow(vis_data$edges) == 0) {
        return(datatable(data.frame(Message = "No contract data available with current filters"), 
                         options = list(dom = 't')))
      }
      
      # Debug what we're working with
      print(paste("Contract table: Processing", nrow(vis_data$edges), "edges"))
      
      # Extract the necessary columns - make sure they exist
      needed_cols <- c("agency", "supplier_name", "total_contracts", "total_award_amount")
      missing_cols <- needed_cols[!needed_cols %in% colnames(vis_data$edges)]
      
      if (length(missing_cols) > 0) {
        print(paste("ERROR: Missing columns in edges data:", paste(missing_cols, collapse = ", ")))
        return(datatable(data.frame(Message = "Data structure error - missing required columns"), 
                         options = list(dom = 't')))
      }
      
      # Get top contracts by award amount
      top_contracts <- tryCatch({
        vis_data$edges %>%
          select(agency, supplier_name, total_contracts, total_award_amount) %>%
          arrange(desc(total_award_amount))
      }, error = function(e) {
        print(paste("Error processing contract table:", e$message))
        return(data.frame(
          agency = character(0),
          supplier_name = character(0),
          total_contracts = integer(0),
          total_award_amount = numeric(0)
        ))
      })
      
      # Print the first few rows to debug
      if (nrow(top_contracts) > 0) {
        print("Top 3 contracts by award amount:")
        print(head(top_contracts, 3))
      } else {
        print("No contracts available in the filtered data")
      }
      
      # Format the award amount
      top_contracts$total_award_amount <- paste0("$", format(top_contracts$total_award_amount, big.mark = ",", scientific = FALSE))
      
      # Rename columns for better presentation
      colnames(top_contracts) <- c("Agency", "Supplier", "Contract Count", "Award Amount")
      
      datatable(
        top_contracts,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          scrollX = TRUE
        )
      )
    })
    
    #-------------
    # Ego Network Visualisation
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
      
      # Get the original visualisation data (already prepared with from/to format)
      vis_data <- network_vis_data()
      
      # Make sure we have data to work with
      if(is.null(vis_data) || nrow(vis_data$edges) == 0) {
        print("No visualisation data available")
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
          # Enhanced styling for better visualisation
          color = case_when(
            is_focal ~ "#F3c623",  # Bright yellow for focal node
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
      
      # Create consistent display name for tables
      ego_nodes <- ego_nodes %>%
        mutate(
          # Create a display_name column that uses the best available name value
          display_name = case_when(
            !is.null(name) & !is.na(name) & name != "" ~ name,
            !is.null(label) & !is.na(label) & label != "" ~ label,
            TRUE ~ id
          )
        )
      
      return(list(nodes = ego_nodes, edges = ego_edges))
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
      
      # Create the visualisation
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
      req(ego_data, input$selected_node)
      
      focal_node <- input$selected_node
      focal_type <- ego_data$nodes$type[ego_data$nodes$id == focal_node]
      
      num_agencies <- sum(ego_data$nodes$type == "agency")
      num_suppliers <- sum(ego_data$nodes$type == "supplier")
      total_contracts <- sum(ego_data$edges$total_contracts, na.rm = TRUE)
      total_award_amount <- sum(ego_data$edges$total_award_amount, na.rm = TRUE)
      
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
          focal_node,
          focal_type,
          nrow(ego_data$nodes),
          num_agencies,
          num_suppliers,
          total_contracts,
          paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      )
      
      print(metrics_df, row.names = FALSE)
    })
    
    #----
    # Ego Network Agency Table
    #----
    
    output$ego_agency_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      
      if (is.null(ego_data$nodes) || nrow(ego_data$nodes) == 0) {
        return(datatable(data.frame(Message = "No ego network data available"), options = list(dom = 't')))
      }
      
      # Filter agencies and select the display_name column
      agencies <- ego_data$nodes %>%
        filter(type == "agency") %>%
        select(display_name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      if (nrow(agencies) == 0) {
        return(datatable(data.frame(Message = "No connected agencies found"), options = list(dom = 't')))
      }
      
      agencies$total_award_amount <- paste0("$", format(agencies$total_award_amount, big.mark = ",", scientific = FALSE))
      colnames(agencies) <- c("Agency Name", "Total Contracts", "Total Award Amount")
      
      datatable(agencies, options = list(pageLength = 5, scrollX = TRUE))
    })
    
    # Ego Network Supplier Table
    output$ego_supplier_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      
      if (is.null(ego_data$nodes) || nrow(ego_data$nodes) == 0) {
        return(datatable(data.frame(Message = "No ego network data available"), options = list(dom = 't')))
      }
      
      # Filter suppliers and select the display_name column we created
      suppliers <- ego_data$nodes %>%
        filter(type == "supplier") %>%
        select(display_name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      if (nrow(suppliers) == 0) {
        return(datatable(data.frame(Message = "No connected suppliers found"), options = list(dom = 't')))
      }
      
      suppliers$total_award_amount <- paste0("$", format(suppliers$total_award_amount, big.mark = ",", scientific = FALSE))
      colnames(suppliers) <- c("Supplier Name", "Total Contracts", "Total Award Amount")
      
      datatable(suppliers, options = list(pageLength = 5, scrollX = TRUE))
    })
    
    # Ego Network Connections Table 
    output$ego_connection_table <- renderDT({
      ego_data <- ego_network_data()
      req(ego_data)
      
      if (nrow(ego_data$edges) == 0) {
        return(datatable(data.frame(Message = "No connections found"), options = list(dom = 't')))
      }
      
      connections <- ego_data$edges %>%
        select(agency, supplier_name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      connections$total_award_amount <- paste0("$", format(connections$total_award_amount, big.mark = ",", scientific = FALSE))
      colnames(connections) <- c("Agency", "Supplier", "Contract Count", "Award Amount")
      
      datatable(connections, options = list(pageLength = 5, scrollX = TRUE))
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
  })
}
    