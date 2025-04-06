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
        monthly_edges = raw_data$monthly_edges,
        nodes = raw_data$nodes_df,
        months = raw_data$months,
        agency_types = raw_data$agency_types,
        time_info = raw_data$time_info
      )
    })
    
    filtered_edges <- eventReactive(input$update_network, {
      tryCatch({
        edges <- network_data()$edges
        
        # Build month index
        start_idx <- as.numeric(input$start_year) * 12 + as.numeric(input$start_month)
        end_idx <- as.numeric(input$end_year) * 12 + as.numeric(input$end_month)
        if (start_idx > end_idx) {
          tmp <- start_idx; start_idx <- end_idx; end_idx <- tmp
        }
        
        # Filter by time
        filtered <- edges %>%
          filter(month_index >= start_idx & month_index <= end_idx)
        
        # Apply other filters
        if (!"All" %in% input$agency_type_filter) {
          filtered <- filtered %>% filter(agency_type %in% input$agency_type_filter)
        }
        if (!"All" %in% input$agency_filter) {
          filtered <- filtered %>% filter(agency %in% input$agency_filter)
        }
        if (!"All" %in% input$supplier_filter) {
          filtered <- filtered %>% filter(supplier_name %in% input$supplier_filter)
        }
        if (!"All" %in% input$tender_cat_filter) {
          filtered <- filtered %>% filter(tender_cat %in% input$tender_cat_filter)
        }
        
        # Filter by award amount range
        filtered <- filtered %>%
          filter(total_award_amount >= input$award_amount_range[1],
                 total_award_amount <= input$award_amount_range[2])
        
        return(filtered)
      }, error = function(e) {
        print(paste("Filter error:", e$message))
        return(data.frame())
      })
    })
    
    
    # 2. Grouped edges and matching nodes
    network_graph_data <- reactive({
      edges <- filtered_edges()
      if (nrow(edges) == 0) return(list(nodes = data.frame(), edges = data.frame()))
      
      # Summarise for visualisation and metrics
      grouped <- edges %>%
        group_by(agency, supplier_name, agency_id, supplier_id, tender_cat) %>%
        summarise(
          total_contracts = sum(total_contracts, na.rm = TRUE),
          total_award_amount = sum(total_award_amount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(from = agency_id, to = supplier_id)
      
      # Match node data
      valid_ids <- unique(c(grouped$from, grouped$to))
      nodes <- network_data()$nodes %>% filter(id %in% valid_ids)
      
      # Add metrics to nodes
      node_metrics <- edges %>%
        pivot_longer(c(agency, supplier_name), names_to = "role", values_to = "name") %>%
        group_by(name) %>%
        summarise(
          total_contracts = sum(total_contracts),
          total_award_amount = sum(total_award_amount),
          .groups = "drop"
        )
      
      nodes <- nodes %>% left_join(node_metrics, by = c("name" = "name"))
      
      return(list(nodes = nodes, edges = grouped))
    })
    
#<------------------------    
    # Returns nodes and edges ready for visNetwork
    network_graph_data <- reactive({
      filtered <- filtered_edges()
      nodes_all <- network_data()$nodes
      
      # Group to collapse same (agency, supplier) across months
      edges <- filtered %>%
        group_by(agency, supplier_name, agency_id, supplier_id, tender_cat) %>%
        summarise(
          total_contracts = sum(total_contracts, na.rm = TRUE),
          total_award_amount = sum(total_award_amount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(from = agency_id, to = supplier_id) %>%
        select(from, to, agency, supplier_name, tender_cat, total_contracts, total_award_amount)
      
      valid_node_ids <- unique(c(edges$from, edges$to))
      
      nodes <- nodes_all %>% filter(id %in% valid_node_ids)
      
      # Summarise totals for nodes across all roles
      node_totals <- filtered %>%
        pivot_longer(cols = c("agency", "supplier_name"), names_to = "role", values_to = "name") %>%
        group_by(name) %>%
        summarise(
          total_contracts = sum(total_contracts, na.rm = TRUE),
          total_award_amount = sum(total_award_amount, na.rm = TRUE),
          .groups = "drop"
        )
      
      nodes <- nodes %>% left_join(node_totals, by = c("name" = "name"))
      
      list(nodes = nodes, edges = edges)
    })    
    
 #<----------------------------
## Filter edge data

    filtered_edges <- eventReactive(input$update_network, {
      req(network_data())
      edges <- network_data()$monthly_edges
      
      # Construct month_index range
      start_idx <- as.numeric(input$start_year) * 12 + as.numeric(input$start_month)
      end_idx <- as.numeric(input$end_year) * 12 + as.numeric(input$end_month)
      if (start_idx > end_idx) {
        temp <- start_idx
        start_idx <- end_idx
        end_idx <- temp
      }
      
      # Apply filters
      edges_filtered <- edges %>%
        filter(month_index >= start_idx & month_index <= end_idx)
      
      if (!"All" %in% input$agency_type_filter) {
        edges_filtered <- edges_filtered %>% filter(agency_type %in% input$agency_type_filter)
      }
      if (!"All" %in% input$agency_filter) {
        edges_filtered <- edges_filtered %>% filter(agency %in% input$agency_filter)
      }
      if (!"All" %in% input$supplier_filter) {
        edges_filtered <- edges_filtered %>% filter(supplier_name %in% input$supplier_filter)
      }
      if (!"All" %in% input$tender_cat_filter) {
        edges_filtered <- edges_filtered %>% filter(tender_cat %in% input$tender_cat_filter)
      }
      
      edges_filtered <- edges_filtered %>%
        filter(total_award_amount >= input$award_amount_range[1],
               total_award_amount <= input$award_amount_range[2])
      
      return(edges_filtered)
    })
  
    
    
    ## Graph Construction
    
    ```r
    network_graph_data <- reactive({
      edges <- filtered_edges()
      req(edges)
      
      # Aggregate to get final edges
      final_edges <- edges %>%
        group_by(agency, supplier_name, agency_id, supplier_id, tender_cat) %>%
        summarise(
          total_contracts = sum(total_contracts, na.rm = TRUE),
          total_award_amount = sum(total_award_amount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(from = agency_id, to = supplier_id)
      
      # Extract node IDs used in edges
      node_ids <- unique(c(final_edges$from, final_edges$to))
      node_df <- network_data()$nodes_df %>%
        filter(id %in% node_ids)
      
      # Summarise node-level totals
      node_totals <- edges %>%
        pivot_longer(cols = c("agency", "supplier_name"), names_to = "role", values_to = "name") %>%
        group_by(name) %>%
        summarise(
          total_contracts = sum(total_contracts, na.rm = TRUE),
          total_award_amount = sum(total_award_amount, na.rm = TRUE),
          .groups = "drop"
        )
      
      node_df <- node_df %>% left_join(node_totals, by = c("name" = "name"))
      
      return(list(edges = final_edges, nodes = node_df))
    })
    ```
    
    
    ## Network Summary Metrics
    
    ```r
    network_summary_metrics <- reactive({
      data <- network_graph_data()
      req(data)
      
      total_nodes <- nrow(data$nodes)
      num_agencies <- sum(data$nodes$type == "agency")
      num_suppliers <- sum(data$nodes$type == "supplier")
      total_edges <- nrow(data$edges)
      total_contracts <- sum(data$edges$total_contracts)
      total_award <- sum(data$edges$total_award_amount)
      
      data.frame(
        Metric = c("Total Nodes", "Agencies", "Suppliers", "Total Connections", "Total Contracts", "Total Award Amount"),
        Value = c(total_nodes, num_agencies, num_suppliers, total_edges, total_contracts, 
                  paste0("$", format(total_award, big.mark = ",", scientific = FALSE)))
      )
    })
    ```
    
    
    ## Visualisation Preparation
    network_vis_data <- reactive({
      data <- network_graph_data()
      req(data)
      
      nodes_vis <- data$nodes %>%
        mutate(
          id = name,
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
          size = scales::rescale(total_contracts, to = c(10, 30))
        )
      
      edges_vis <- data$edges %>%
        mutate(
          from = agency,
          to = supplier_name,
          value = if (input$edge_metric == "total_award_amount") {
            scales::rescale(total_award_amount, to = c(1, 10))
          } else {
            scales::rescale(total_contracts, to = c(1, 10))
          },
          title = paste0(
            "Agency: ", agency, "<br>",
            "Supplier: ", supplier_name, "<br>",
            "Total Contracts: ", total_contracts, "<br>",
            "Total Award Amount: $",
            format(total_award_amount, big.mark = ",", scientific = FALSE)
          )
        )
      
      list(nodes = nodes_vis, edges = edges_vis)
    })
  
    
    #--------    
    # Network Visualisation - main render function
    #--------
    output$network_plot <- renderVisNetwork({
      print("🔄 Rendering visNetwork...")
      
      vis_data <- network_vis_data()
      
      # Validate data availability
      req(vis_data)
      if (nrow(vis_data$nodes) == 0 || nrow(vis_data$edges) == 0) {
        return(visNetwork(
          data.frame(id = 1, label = "No data to display with current filters"),
          data.frame(from = integer(0), to = integer(0))
        ))
      }
      
      # Modify node sizes based on degree centrality if enabled
      if (input$size_by_degree) {
        print("📐 Calculating degree centrality...")
        
        graph <- igraph::graph_from_data_frame(
          d = vis_data$edges[, c("from", "to")],
          directed = TRUE
        )
        
        degrees <- igraph::degree(graph, mode = "all")
        degree_vector <- setNames(degrees, names(degrees))
        
        vis_data$nodes$degree <- sapply(vis_data$nodes$id, function(id) {
          degree_vector[[id]] %||% 1  # fallback to 1 if ID not in degree list
        })
        
        # Scale degree to size (log scale for better distribution)
        if (length(unique(vis_data$nodes$degree)) > 1) {
          log_degrees <- log1p(vis_data$nodes$degree)
          vis_data$nodes$size <- round(scales::rescale(log_degrees, to = c(10, 40)), 1)
        } else {
          vis_data$nodes$size <- 20  # uniform fallback
        }
        
      } else {
        vis_data$nodes$size <- 20  # default size
      }
      
      # Build visNetwork
      network <- visNetwork(vis_data$nodes, vis_data$edges) %>%
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
    #Summary
    output$network_summary <- renderPrint({
      vis_data <- network_vis_data()
      req(vis_data)
      
      num_nodes <- nrow(vis_data$nodes)
      num_edges <- nrow(vis_data$edges)
      num_agencies <- sum(vis_data$nodes$type == "agency")
      num_suppliers <- sum(vis_data$nodes$type == "supplier")
      total_contracts <- sum(vis_data$edges$total_contracts, na.rm = TRUE)
      total_award <- sum(vis_data$edges$total_award_amount, na.rm = TRUE)
      
      summary_df <- data.frame(
        Metric = c("Total Nodes", "Agencies", "Suppliers", "Total Connections", "Total Contracts", "Total Award Amount"),
        Value = c(
          num_nodes, num_agencies, num_suppliers, num_edges, total_contracts,
          paste0("$", format(total_award, big.mark = ",", scientific = FALSE))
        )
      )
      
      print(summary_df, row.names = FALSE)
    })
    
    #------------
#    Network Metric table
    #------------
    
    # agency table
    output$agency_table <- renderDT({
      vis_data <- network_vis_data()
      req(vis_data)
      
      agencies <- vis_data$nodes %>%
        filter(type == "agency") %>%
        select(name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount)) %>%
        mutate(
          total_award_amount = paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      
      datatable(
        agencies,
        colnames = c("Agency Name", "Total Contracts", "Total Award Amount"),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # supplier table
    output$supplier_table <- renderDT({
      vis_data <- network_vis_data()
      req(vis_data)
      
      suppliers <- vis_data$nodes %>%
        filter(type == "supplier") %>%
        select(name, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount)) %>%
        mutate(
          total_award_amount = paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      
      datatable(
        suppliers,
        colnames = c("Supplier Name", "Total Contracts", "Total Award Amount"),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # contract table
    output$contract_table <- renderDT({
      vis_data <- network_vis_data()
      req(vis_data)
      
      contracts <- vis_data$edges %>%
        select(agency, supplier_name, tender_cat, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount)) %>%
        mutate(
          total_award_amount = paste0("$", format(total_award_amount, big.mark = ",", scientific = FALSE))
        )
      
      datatable(
        contracts,
        colnames = c("Agency", "Supplier", "Tender Category", "Contract Count", "Award Amount"),
        options = list(pageLength = 10, scrollX = TRUE)
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
      data <- filtered_data()
      req(data)
      req(nrow(data$nodes) > 0)
      
      # Calculate key statistics
      num_agencies <- sum(data$nodes$type == "agency")
      num_suppliers <- sum(data$nodes$type == "supplier") 
      total_contracts <- sum(data$edges$total_contracts)
      total_award_amount <- sum(data$edges$total_award_amount)
      
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
      
      agencies <- ego_data$nodes %>%
        filter(type == "agency") %>%
        select(name = id, total_contracts, total_award_amount) %>%
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
      
      suppliers <- ego_data$nodes %>%
        filter(type == "supplier") %>%
        select(name = id, total_contracts, total_award_amount) %>%
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
        select(from, to, tender_cat, total_contracts, total_award_amount) %>%
        arrange(desc(total_award_amount))
      
      connections$total_award_amount <- paste0("$", format(connections$total_award_amount, big.mark = ",", scientific = FALSE))
      colnames(connections) <- c("From", "To", "Tender Category", "Contract Count", "Award Amount")
      
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
    
    #<--end
  })
}