# network_community_module.R

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(igraph)
library(readr)
library(DT)
library(rlang)
library(visNetwork)
library(shinyjs) 

# Load preprocessed network-community data

community_data_global <- read_csv("data/network_community_data.csv")
#--------
# Debug

print(paste("community_data_global class:", class(community_data_global)))
print(head(community_data_global))
#--------

# UI Function
network_community_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinyjs::useShinyjs(),
      #------------      
      #------------      
      # Left sidebar with controls
      column(width = 3,
             div(class = "control-panel",
                 h4("Procurement Profile"),
                 
                 # Always visible core filters
                 dateRangeInput(ns("date_range"), "Date Range:",
                                start = "2019-04-01", end = "2024-03-31",
                                min = "2019-04-01", max = "2024-03-31"),
                 
                 sliderInput(ns("award_amount"), "Award Amount (S$):", 
                             min = 0, max = 1500000000, 
                             value = c(0, 1500000000), step = 10000,
                             pre = "$", sep = ",", animate = FALSE,
                             width = "100%"),
                 
                 # Container div for the numeric inputs
                 div(
                   style = "display: flex; justify-content: space-between; margin-top: -15px;",
                   div(
                     style = "width: 45%;",
                     numericInput(ns("award_amount_min"), NULL, 
                                  value = 0, min = 0, max = 1500000000, 
                                  step = 10000, width = "100%")
                   ),
                   div(
                     style = "width: 45%;",
                     numericInput(ns("award_amount_max"), NULL, 
                                  value = 1500000000, min = 0, max = 1500000000, 
                                  step = 10000, width = "100%")
                   )
                 ),
                 
                 # Entity Filters section with a toggle button
                 div(
                   class = "panel panel-default",
                   div(
                     class = "panel-heading",
                     style = "cursor: pointer;",
                     HTML('<h4 class="panel-title"><a data-toggle="collapse" href="#entityFiltersPanel">▸ Entity Filters</a></h4>')
                   ),
                   div(
                     id = "entityFiltersPanel",
                     class = "panel-collapse collapse",
                     div(
                       class = "panel-body",
                       selectInput(ns("agency_type"), "Agency Type:", 
                                   choices = c("All" = "All"), 
                                   selected = "All",
                                   multiple = TRUE),
                       
                       selectizeInput(ns("selected_agencies"), "Select Agencies:", 
                                      choices = c("All" = "All"), 
                                      selected = "All",
                                      multiple = TRUE),
                       
                       selectizeInput(ns("selected_suppliers"), "Select Suppliers:", 
                                      choices = c("All" = "All"), 
                                      selected = "All",
                                      multiple = TRUE),
                       
                       selectInput(ns("tender_type"), "Tender Type:", 
                                   choices = c("All" = "All"), 
                                   selected = "All",
                                   multiple = TRUE)
                     )
                   )
                 ),
                 
                 # Network Settings section with a toggle button
                 div(
                   class = "panel panel-default",
                   div(
                     class = "panel-heading",
                     style = "cursor: pointer;",
                     HTML('<h4 class="panel-title"><a data-toggle="collapse" href="#networkSettingsPanel">▸ Network Settings</a></h4>')
                   ),
                   div(
                     id = "networkSettingsPanel",
                     class = "panel-collapse collapse",
                     div(
                       class = "panel-body",
                       sliderInput(ns("max_edges"), "Maximum Edges:", 
                                   min = 100, max = 10000, value = 500, step = 100),
                       
                       h4("Community Methods"),
                       selectInput(ns("layout"), "Network Layout:",
                                   choices = c("Force-directed (FR)" = "fr", 
                                               "Kamada-Kawai" = "kk",
                                               "DrL" = "drl",
                                               "GraphOpt" = "graphopt",
                                               "Community Grouped" = "community_grouped"),
                                   selected = "fr"),                
                       selectInput(ns("algorithm"), "Community Detection Algorithm:",
                                   choices = c("Edge-betweenness", "Louvain", "Walktrap", 
                                               "Label propagation", "InfoMAP"),
                                   selected = "Louvain"),
                       
                       selectInput(ns("centrality_measure"), "Centrality for Node Size:",
                                   choices = c("Degree", "Betweenness", "Closeness", "Eigenvector"),
                                   selected = "Degree")
                     )
                   )
                 ),
                 
                 # Update button (always visible and larger)
                 div(
                   style = "margin-top: 10px; margin-bottom: 10px;",
                   actionButton(ns("update_viz"), "Update Plot", 
                                class = "btn-primary btn-lg",
                                style = "width: 100%; font-size: 14px;")
                 )
             )
      ),
      
      # Right content
      column(width = 9,
             tabsetPanel(
               tabPanel("Community Network Overview",
                        # Metrics at top
                        fluidRow(
                          column(12, uiOutput(ns("community_metrics")))
                        ),
                        # Network visualisation
                        fluidRow(
                          column(12, 
                                 h4("Community Network"),
                                 visNetworkOutput(ns("community_network"), height = "800px"))
                        ),
                        # Community distribution
                        fluidRow(
                          column(12, 
                                 h4("Community Distribution"),
                                 plotOutput(ns("community_barchart"), height = "300px"))
                        )
               ),
               
               tabPanel("Community Explorer",
                        fluidRow(
                          column(12,
                                 selectizeInput(ns("selected_community"), "Select Community to Explore:", 
                                                choices = NULL, width = "50%")
                          )
                        ),
                        fluidRow(
                          column(12, uiOutput(ns("community_detail_metrics")))
                        ),
                        fluidRow(
                          column(12, 
                                 h4("Community Ego Network"),
                                 visNetworkOutput(ns("ego_community_network"), height = "600px"))
                        )
                        # fluidRow(
                        #   column(12, 
                        #          h4("Community Members"),
                        #          DT::dataTableOutput(ns("community_nodes_table")))
                        # )
               ),
               # Tab 3 for the metrics
               tabPanel("Community Members",
                        fluidRow(
                          column(12,
                                 selectizeInput(ns("selected_community_members"), "Select Community:", 
                                                choices = NULL, width = "50%")
                          )
                        ),
                        fluidRow(
                          column(12, 
                                 h4("Community Members"),
                                 DT::dataTableOutput(ns("community_nodes_table")))
               )
             )
      )
    )
  ))
}
#-------------------------------------------------------------------
# Server Function
#-------------------------------------------------------------------
network_community_server <- function(id, dataset) {
  cat("Starting network_community_server\n")
  
  moduleServer(id, function(input, output, session) {
    cat("Starting moduleServer function\n")
    
    cat("Executing line [188]\n")
    
    # Initialise reactive values
    rv <- reactiveValues(
      graph = NULL,
      communities = NULL
    )
    
    cat("Executing line [196]\n")
    
    # For the update button
    observeEvent(input$some_trigger, {
      # Only trigger once when the session starts
      session$onFlushed(function() {
        shinyjs::delay(100, shinyjs::click(ns("update_viz")))
      })
    }, once = TRUE)
    
    # Update UI choices when data changes
    observe({
      #------------------------------------
      # debug
      cat("DEBUG Observer for UI controls is running\n")
      
      # Simply check if dataset exists
      req(dataset)
      
      # Print dataset class for debugging
      cat("DEBUG: Dataset class:", paste(class(dataset), collapse=", "), "\n")
      
      # Print column names to verify structure
      cat("DEBUG: Column names in dataset:", paste(names(dataset), collapse = ", "), "\n")
      
      # Print sample values to verify data
      cat("DEBUG: Sample agencies:", paste(head(unique(dataset$agency)), collapse = ", "), "\n")
      cat("DEBUG: Sample suppliers:", paste(head(unique(dataset$supplier_name)), collapse = ", "), "\n")
      
      cat("Executing line [249]\n")      
      #------------------------------------
      req(dataset)
      
      agency_type_choices <- c("All" = "All", unique(dataset$agency_type))
      agency_choices <- c("All" = "All", unique(dataset$agency))
      supplier_choices <- c("All" = "All", unique(dataset$supplier_name))
      tender_type_choices <- c("All" = "All", unique(dataset$tender_type))      
      
      updateSelectInput(session, "agency_type", 
                        choices = agency_type_choices,
                        selected = "All")
      
      updateSelectizeInput(session, "selected_agencies", 
                           choices = agency_choices,
                           selected = "All",
                           server = TRUE)
      
      updateSelectizeInput(session, "selected_suppliers", 
                           choices = supplier_choices,
                           selected = "All",
                           server = TRUE)
      
      updateSelectInput(session, "tender_type", 
                        choices = tender_type_choices,
                        selected = "All")
      updateSliderInput(session, "max_edges",
                        value = 500)
    })
    
    # Update agencies when agency_type changes
    observeEvent(input$agency_type, {
      req(dataset)
      
      # Filter agencies based on agency_type selection
      filtered_agencies <- if (is.null(input$agency_type) || "All" %in% input$agency_type) {
        c("All" = "All", unique(dataset$agency))
      } else {
        c("All" = "All", 
          filter(dataset, agency_type %in% input$agency_type) %>%
            pull(agency) %>%
            unique())
      }
      
      # Update the agencies dropdown
      selected_value <- if("All" %in% input$selected_agencies || is.null(input$selected_agencies)) "All" else input$selected_agencies
      updateSelectizeInput(session, "selected_agencies", 
                           choices = filtered_agencies,
                           selected = selected_value,
                           server = TRUE)
    })
#-----try slider implementation
    # Sync the slider with numeric inputs
    observeEvent(input$award_amount, {
      updateNumericInput(session, "award_amount_min", value = input$award_amount[1])
      updateNumericInput(session, "award_amount_max", value = input$award_amount[2])
    }, ignoreInit = TRUE)
    
    # Sync numeric inputs with the slider
    observeEvent(input$award_amount_min, {
      if(input$award_amount_min != input$award_amount[1]) {
        updateSliderInput(session, "award_amount", 
                          value = c(input$award_amount_min, input$award_amount[2]))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$award_amount_max, {
      if(input$award_amount_max != input$award_amount[2]) {
        updateSliderInput(session, "award_amount", 
                          value = c(input$award_amount[1], input$award_amount_max))
      }
    }, ignoreInit = TRUE)
#-----    
    cat("Executing line [322]\n")
    
    create_filtered_graph <- eventReactive(input$update_viz, {
      req(dataset)
      cat("Creating filtered graph...\n") 
      
      # TEMPORARY: Skip filtering for debugging
      filtered_data <- dataset
      
      cat("Using all data with dimensions:", nrow(filtered_data), "rows x", ncol(filtered_data), "columns\n")
      
      # Create edges directly without filtering
      edges <- filtered_data %>%
        select(agency, supplier_name, awarded_amt) %>%
        rename(from = agency, to = supplier_name, weight = awarded_amt)
      
      cat("Edge count before limit:", nrow(edges), "\n")
      #------
      # Print filtered data dimensions
      cat("Filtered data dimensions:", nrow(filtered_data), "rows x", ncol(filtered_data), "columns\n")
      cat("Executing line [342]\n")      
      #-----
      
      # Apply filters 
      if (!is.null(input$selected_agencies) && length(input$selected_agencies) > 0 && !("All" %in% input$selected_agencies)) {
        filtered_data <- filtered_data %>% filter(agency %in% input$selected_agencies)
      }
      
      if (!is.null(input$selected_suppliers) && length(input$selected_suppliers) > 0 && !("All" %in% input$selected_suppliers)) {
        filtered_data <- filtered_data %>% filter(supplier_name %in% input$selected_suppliers)
      }
      
      if (!is.null(input$agency_type) && length(input$agency_type) > 0 && !("All" %in% input$agency_type)) {
        filtered_data <- filtered_data %>% filter(agency_type %in% input$agency_type)
      }
      
      if (!is.null(input$tender_type) && length(input$tender_type) > 0 && !("All" %in% input$tender_type)) {
        filtered_data <- filtered_data %>% filter(tender_type %in% input$tender_type)
      }
      # Apply date range filter
      if (!is.null(input$date_range)) {
        filtered_data <- filtered_data %>% 
          filter(award_date >= input$date_range[1] & award_date <= input$date_range[2])
        cat("Filtered by date range:", as.character(input$date_range[1]), "to", as.character(input$date_range[2]), "\n")
        cat("Remaining data after date filter:", nrow(filtered_data), "rows\n")
      }
      
      # Apply award amount filter
      if (!is.null(input$award_amount)) {
        filtered_data <- filtered_data %>% 
          filter(awarded_amt >= input$award_amount[1] & awarded_amt <= input$award_amount[2])
        cat("Filtered by award amount:", input$award_amount[1], "to", input$award_amount[2], "\n")
        cat("Remaining data after award amount filter:", nrow(filtered_data), "rows\n")
      }
      
      
      
      # Create edges
      edges <- filtered_data %>%
        group_by(agency, supplier_name) %>%
        # Summarize to get total award amount
        summarize(weight = sum(awarded_amt, na.rm = TRUE), 
                  # Count number of tenders for tooltip
                  tender_count = n(), 
                  .groups = "drop") %>%
        # Rename columns for visNetwork
        rename(from = agency, to = supplier_name)
      #-----
      cat("Edge count before limit:", nrow(edges), "\n")
      cat("Executing line [332]\n")
      # Debug edge creation
      cat("Edge count after creation:", nrow(edges), "\n")
      if(nrow(edges) > 0) {
        cat("First few edges:\n")
        print(head(edges))
        cat("Any NA values in edges?", 
            any(is.na(edges$from)), any(is.na(edges$to)), any(is.na(edges$weight)), "\n")
        cat("Number of unique 'from' nodes:", length(unique(edges$from)), "\n")
        cat("Number of unique 'to' nodes:", length(unique(edges$to)), "\n")
      }
      #-----
      
      # Limit edges if specified
      if(nrow(edges) > input$max_edges) {
        edges <- edges %>% 
          arrange(desc(weight)) %>%
          slice_head(n = input$max_edges)
        
        cat("Limited edges to:", nrow(edges), "\n")
      }
      #----------      
      # debug: check if we have edges
      #----------
      
      if(nrow(edges) == 0) {
        cat("WARNING: No edges found with current filters!\n")
        return(NULL)
      }      
      
      # Create graph
      # Create a proper nodes dataframe first
      nodes <- data.frame(
        name = unique(c(edges$from, edges$to)),
        stringsAsFactors = FALSE
      )
      
      # Now create the graph with both nodes and edges using the correct syntax for as_tbl_graph
      graph <- tbl_graph(
        nodes = nodes,
        edges = edges,
        directed = FALSE
      )
      cat("Graph created with", igraph::gorder(graph), "nodes and", igraph::gsize(graph), "edges\n")      
      #-------
      # debug: graph summary debug
      # Debug graph creation
      cat("Graph created successfully?\n")
      if(!is.null(graph)) {
        cat("Graph has", igraph::gorder(graph), "nodes and", igraph::gsize(graph), "edges\n")
        # Check if nodes were created properly
        node_df <- graph %>% activate(nodes) %>% as_tibble()
        cat("Node names sample:", paste(head(node_df$name), collapse=", "), "\n")
        # Check if edges were created properly  
        edge_df <- graph %>% activate(edges) %>% as_tibble()
        cat("Edge dataframe columns:", paste(names(edge_df), collapse=", "), "\n")
        if(nrow(edge_df) > 0) {
          cat("First edge:", toString(edge_df[1,]), "\n")
        } else {
          cat("WARNING: No edges in the created graph!\n")
        }
      }      
      
      cat("Graph created with", igraph::gorder(graph), "nodes and", igraph::gsize(graph), "edges\n")
      cat("Executing line [413]\n")
      #-------    
      
      # Add node attributes
      graph <- graph %>%
        activate(nodes) %>%
        mutate(
          type = ifelse(name %in% unique(edges$from), "Agency", "Supplier")
        )
      
      return(graph)
    })
    
    # Apply community detection and update communities
    observe({
      # First check if we have a graph
      graph <- create_filtered_graph()
      
      # Check if graph is NULL
      if(is.null(graph)) {
        cat("Graph before community detection:", igraph::gorder(graph), "nodes and", igraph::gsize(graph), "edges\n")
        cat("WARNING: Graph is NULL in community detection observer\n")
        rv$graph <- NULL
        rv$communities <- NULL
        return()
      }
      
      cat("Executing line [440]\n")           
      # Apply selected community detection algorithm with tryCatch for error handling
      tryCatch({
        cat("Applying community detection algorithm:", input$algorithm, "\n")
        
        graph <- graph %>%
          activate(nodes) %>%
          mutate(community = case_when(
            input$algorithm == "Edge-betweenness" ~ as.factor(group_edge_betweenness()),
            input$algorithm == "Louvain" ~ as.factor(group_louvain()),
            input$algorithm == "Walktrap" ~ as.factor(group_walktrap()),
            input$algorithm == "Label propagation" ~ as.factor(group_label_prop()),
            input$algorithm == "InfoMAP" ~ as.factor(group_infomap()),
            TRUE ~ as.factor(group_louvain()) # Default
          ))
        
        # Store in reactive values
        rv$graph <- graph
        rv$communities <- unique(V(graph)$community)
        
        # Update community selection dropdown
        updateSelectizeInput(session, "selected_community",
                             choices = rv$communities)
        
        updateSelectizeInput(session, "selected_community_members",
                             choices = rv$communities)
        
      }, error = function(e) {
        cat("ERROR in community detection:", e$message, "\n")
        rv$graph <- graph # Still store the graph even if community detection fails
        rv$communities <- NULL
      })
    })
    
    
    # Add observer to handle dropdown selection changes
    observeEvent(input$selected_community, {
      req(input$selected_community, rv$graph)
      
      # Get all nodes in the selected community
      community_nodes <- rv$graph %>%
        activate(nodes) %>%
        filter(community == input$selected_community) %>%
        pull(name)
      
      # If we have nodes, select one to highlight that community
      if (length(community_nodes) > 0) {
        # Use visNetworkProxy to select a node from the community
        visNetworkProxy("community_network") %>%
          visSelectNodes(id = community_nodes[1])
      }
    }, ignoreInit = TRUE)     
    
    # Add observer to capture selection from the built-in visNetwork dropdown
    observeEvent(input$community_network_selectedBy, {
      req(input$community_network_selectedBy, rv$communities)
      
      # Update the Community Explorer dropdown with the current visNetwork selection
      if (!is.null(input$community_network_selectedBy) && 
          input$community_network_selectedBy != input$selected_community) {
        updateSelectizeInput(session, "selected_community", 
                             choices = rv$communities,
                             selected = input$community_network_selectedBy)
      }
    }, ignoreInit = TRUE)       
    
    # Sync selected_community and selected_community_members
    observeEvent(input$selected_community, {
      updateSelectizeInput(session, "selected_community_members", 
                           choices = rv$communities,
                           selected = input$selected_community)
    }, ignoreInit = TRUE)
    
    observeEvent(input$selected_community_members, {
      updateSelectizeInput(session, "selected_community", 
                           choices = rv$communities,
                           selected = input$selected_community_members)
    }, ignoreInit = TRUE)
    #------ 
    
    # Render main network plot
    output$community_network <- renderVisNetwork({
      
      #------            
      # Check if graph is available
      if(is.null(rv$graph)) {
        cat("WARNING: No graph available for rendering\n")
        # Return an empty visNetwork with this message
        return(visNetwork(nodes = data.frame(id = 1, label = "No data"),
                          edges = data.frame(from = 1, to = 1)) %>%
                 visNodes(physics = FALSE) %>%
                 visOptions(highlightNearest = TRUE,
                            nodesIdSelection = TRUE) %>%
                 visLayout(randomSeed = 123) %>%
                 addFontAwesome() %>%
                 visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
                 visLayout(hierarchical = FALSE) %>%
                 visNodes(size = 5, color = list(background = "#ffffff", border = "#ffffff"),
                          shadow = FALSE) %>%
                 visEdges(color = "#697536",
                          smooth = FALSE) %>%
                 visLegend(enabled = FALSE) %>%
                 visLayout(randomSeed = 123))
      }
      
      # Get the graph
      graph <- rv$graph
      
      # Calculate centrality measure based on user selection
      graph <- graph %>%
        activate(nodes) %>%
        mutate(centrality = case_when(
          input$centrality_measure == "Degree" ~ centrality_degree(),
          input$centrality_measure == "Betweenness" ~ centrality_betweenness(),
          input$centrality_measure == "Closeness" ~ centrality_closeness(),
          input$centrality_measure == "Eigenvector" ~ centrality_eigen(),
          TRUE ~ centrality_degree()
        ))
      
      # Prepare nodes and edges dataframes for visNetwork (Community Main)
      nodes_df <- graph %>%
        activate(nodes) %>%
        as_tibble() %>%
        mutate(
          id = name,  # visNetwork requires an 'id' column
          label = name,
          group = as.character(community),  # Group by community for coloring
          value = centrality,  # Node size based on centrality
          title = paste0("<p><strong>", name, "</strong><br>",
                         "Type: ", type, "<br>",
                         "Community: ", community, "<br>",
                         input$centrality_measure, ": ", round(centrality, 3), "</p>")  # Tooltip
        )
      # Extract edges from the graph
      edges_list <- as_edgelist(graph, names = TRUE)
      
      # Create edge dataframe
      edges_df <- data.frame(
        from = edges_list[, 1],
        to = edges_list[, 2],
        stringsAsFactors = FALSE
      )
      
      # Add weights to dataframe
      if("weight" %in% edge_attr_names(graph)) {
        edges_df$weight <- edge_attr(graph, "weight")
      } else {
        # Default weight if not present
        edges_df$weight <- 1
      }
      
      # Calculate absolute width for edges
      edges_df$width <- case_when(
        !length(unique(edges_df$weight)) > 1 ~ 5,  # If all weights are the same
        TRUE ~ 3 + 25 * log10(1 + edges_df$weight/min(edges_df$weight[edges_df$weight > 0])) / 
          max(log10(1 + edges_df$weight/min(edges_df$weight[edges_df$weight > 0])))
      )
      
      edges_df <- data.frame(
        from = edges_list[, 1],
        to = edges_list[, 2],
        stringsAsFactors = FALSE
      )
      
      # Add weights if they exist in the graph
      if("weight" %in% edge_attr_names(graph)) {
        edges_df$weight <- edge_attr(graph, "weight")
      } else {
        # Default weight if not present
        edges_df$weight <- 1
      }
      
      # Debug output
      cat("Edge weight range:", min(edges_df$weight), "to", max(edges_df$weight), "\n")
      cat("Edge width range:", min(edges_df$width), "to", max(edges_df$width), "\n")      
      cat("Edge dataframe has", nrow(edges_df), "rows with columns:", paste(names(edges_df), collapse=", "), "\n")
      
      # Make sure nodes and edges match
      valid_node_ids <- nodes_df$id
      edges_df <- edges_df %>%
        filter(from %in% valid_node_ids & to %in% valid_node_ids)
      
      cat("After validation, edges dataframe has", nrow(edges_df), "rows\n")
      
      # Get unique communities for color assignment
      communities <- unique(nodes_df$group)
      
      # Define a color palette based on the number of communities
      # Using the RColorBrewer palette or any other color scheme
      colors <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                                   "#FF7F00", "#FFFF33", "#A65628", "#B5828C",
                                   "#3E3F5B", "#336D82", "#015551", "#89AC46",
                                   "#E53888", "#003092", "#73C7C7"))(length(communities))
      
      # Create a named vector for community colors
      community_colors <- setNames(colors, communities)
      
      # Calculate layout positions to group nodes by community
      # This step is optional but helps in visualizing communities
      layout_data <- NULL
      if(input$layout == "community_grouped") {
        # Create a layout that groups by community
        layout_data <- layout_with_fr(as.igraph(graph))
        
        # Convert to dataframe
        layout_df <- data.frame(
          id = V(as.igraph(graph))$name,
          x = layout_data[,1],
          y = layout_data[,2]
        )
        
        # Add to nodes dataframe
        nodes_df <- left_join(nodes_df, layout_df, by = "id")
      }
      
      if("tender_count" %in% edge_attr_names(graph)) {
        edges_df$tender_count <- edge_attr(graph, "tender_count")
        edges_df$title <- paste0("Total Award: $", format(round(edges_df$weight, 2), big.mark=","), 
                                 "<br>Tenders: ", edges_df$tender_count)
      } else {
        edges_df$title <- paste0("Total Award: $", format(round(edges_df$weight, 2), big.mark=","))
      }     
      cat("Executing line [685]\n")
      cat("Edge dataframe includes tooltips now\n")
      
      # Generate a visNetwork 
      network <- visNetwork(nodes = nodes_df, edges = edges_df) %>%
        # Apply physics based on layout
        visPhysics(solver = switch(input$layout,
                                   "fr" = "forceAtlas2Based",
                                   "kk" = "barnesHut",
                                   "drl" = "repulsion",
                                   "graphopt" = "forceAtlas2Based",
                                   "community_grouped" = "hierarchicalRepulsion",
                                   "forceAtlas2Based"),
                   forceAtlas2Based = list(gravitationalConstant = -100,
                                           centralGravity = 0.01,
                                           springLength = 150,
                                           springConstant = 0.05),
                   stabilization = list(iterations = 200)) %>%
        # Node styling
        visNodes(scaling = list(min = 10, max = 30),
                 font = list(size = 12),
                 shadow = TRUE) %>%
        # Edge styling
        visEdges(smooth = list(enabled = TRUE, type = "dynamic"),
                 arrows = list(to = FALSE, from = FALSE),
                 color = list(opacity = 0.7),
                 width = "width",
                 hoverWidth = 2,
                 selectionWidth = 2) %>%
        # Interaction options
        visInteraction(navigationButtons = TRUE,
                       keyboard = TRUE,
                       tooltipDelay = 100) %>%
        # Enable clustering by community
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   selectedBy = list(variable = "group", main = "Select Community"),
                   height = "500px") %>%
        # Add legend
        visLegend(enabled = FALSE, 
                  useGroups = TRUE, 
                  position = "right",
                  main = "Node Types") %>%
        # Add title
        visLayout(randomSeed = 123)
      
      # If we have layout data, use it
      if(!is.null(layout_data) && input$ns("layout") == "community_grouped") {
        network <- network %>%
          visLayout(improvedLayout = TRUE)
      }
      
      # Add title via HTML widget
      network %>%
        htmlwidgets::onRender(paste0(
          "function(el, x, data) {
        var title = document.createElement('div');
        title.style.textAlign = 'center';
        title.style.fontWeight = 'bold';
        title.innerHTML = 'Community Detection using ", input$algorithm, 
          "<br>", nrow(nodes_df), " nodes, ", 
          nrow(edges_df), " connections';
        title.style.fontSize = '14px';
        title.style.marginBottom = '10px';
        el.insertBefore(title, el.firstChild);
      }"))
      
      # Event handlers to capture selections and update dropdown
      network <- network %>%
        visEvents(
          selectNode = "function(nodes) {
      if (nodes.nodes.length > 0) {
        // Get the selected node
        var selectedNode = this.body.data.nodes.get(nodes.nodes[0]);
        // Get its community/group
        var selectedCommunity = selectedNode.group;
        // Update the Shiny input
        Shiny.setInputValue('community_module-selected_community', selectedCommunity);
      }
    }"
        )
      return(network)
    })
    
    cat("Executing line [815]\n") 
    
    # Render metrics cards
    output$community_metrics <- renderUI({
      req(rv$graph)
      
      graph <- rv$graph
      
      # Calculate metrics
      n_communities <- length(unique(V(graph)$community))
      
      # Convert to igraph for certain calculations
      igraph_obj <- as.igraph(graph)
      
      # Calculate modularity
      modularity_score <- modularity(
        igraph_obj, 
        membership = as.numeric(as.character(V(igraph_obj)$community))
      )
      
      # Calculate within vs between density
      communities <- as.numeric(as.character(V(igraph_obj)$community))
      community_list <- split(1:vcount(igraph_obj), communities)
      
      within_density <- mean(sapply(community_list, function(nodes) {
        if(length(nodes) <= 1) return(0)
        subg <- induced_subgraph(igraph_obj, nodes)
        edge_density(subg)
      }))
      
      between_density <- edge_density(igraph_obj) - within_density
      
      cat("Executing line [847]\n")
      
      # Create UI cards
      fluidRow(
        shinydashboard::valueBox(
          n_communities, "Communities", icon = shiny::icon("fas fa-network-wired"), 
          color = "light-blue", width = 3
        ),
        shinydashboard::valueBox(
          round(modularity_score, 3), "Modularity", icon = shiny::icon("fas f-project-diagram"), 
          color = "light-blue", width = 3
        ),
        shinydashboard::valueBox(
          round(within_density, 3), "Within Density", icon = shiny::icon("fas fa-connectdevelop"), 
          color = "light-blue", width = 3
        ),
        shinydashboard::valueBox(
          round(between_density, 3), "Between Density", icon = shiny::icon("fas fa-share-alt"), 
          color = "light-blue", width = 3
        )
      )
    })
    cat("Executing line [869]\n")
    
    # Render community structure bar chart
    output$community_barchart <- renderPlot({
      req(rv$graph)
      
      # Get community sizes
      community_sizes <- rv$graph %>%
        activate(nodes) %>%
        as_tibble() %>%
        count(community) %>%
        arrange(desc(n))
      
      # Add percentage
      total_nodes <- sum(community_sizes$n)
      community_sizes <- community_sizes %>%
        mutate(percentage = n / total_nodes * 100)
      
      # Plot
      ggplot(community_sizes, aes(x = reorder(community, -n), y = n, fill = community)) +
        geom_col(fill = "#A5BFCC") +
        geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
        theme_minimal() +
        labs(x = "Community", y = "Number of Nodes") +
        theme(legend.position = "none") 
    })
    
    #-----------    
    # Test render
    output$test_network <- renderVisNetwork({
      nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
      edges <- data.frame(from = c(1,1), to = c(2,3))
      visNetwork(nodes, edges)
    })
    
    #-----------
    # Render ego-centric community network
    output$ego_community_network <- renderVisNetwork({
      req(rv$graph, input$selected_community)
      
      # Get the selected community ID
      selected_community <- input$selected_community
      cat("Selected community for ego network:", selected_community, "\n")
      
      # Extract the nodes in the selected community
      community_nodes <- rv$graph %>%
        activate(nodes) %>%
        filter(community == selected_community) %>%
        pull(name)
      
      if(length(community_nodes) == 0) {
        return(visNetwork(nodes = data.frame(id = 1, label = "No nodes in selected community"),
                          edges = data.frame(from = 1, to = 1)) %>%
                 visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
                 visLayout(randomSeed = 123) %>%
                 visNodes(size = 5, color = list(background = "#ffffff", border = "#ffffff"),
                          shadow = FALSE) %>%
                 visEdges(width = 0) %>%
                 visLegend(enabled = FALSE))
      }
      
      # Get the full graph as igraph object
      ig <- as.igraph(rv$graph)
      
      # Get community node indices
      community_indices <- which(V(ig)$name %in% community_nodes)
      
      # Create a subgraph containing only the community
      community_subgraph <- induced_subgraph(ig, community_indices)
      
      # Prepare nodes dataframe
      nodes_df <- data.frame(
        id = V(community_subgraph)$name,
        label = V(community_subgraph)$name,
        group = V(community_subgraph)$type,  # Use the type as the group for coloring
        type = V(community_subgraph)$type,
        stringsAsFactors = FALSE
      )
      
      # Prepare edges dataframe if there are any edges
      if(ecount(community_subgraph) > 0) {
        # Get edge list
        edge_list <- as_edgelist(community_subgraph, names = TRUE)
        
        # Create edges dataframe
        edges_df <- data.frame(
          from = edge_list[,1],
          to = edge_list[,2],
          stringsAsFactors = FALSE
        )
        
        # Add weight if it exists
        if("weight" %in% edge_attr_names(community_subgraph)) {
          edges_df$weight <- edge_attr(community_subgraph, "weight")
        } else {
          edges_df$weight <- 1
        }
        
        # Add tender_count if it exists
        if("tender_count" %in% edge_attr_names(community_subgraph)) {
          edges_df$tender_count <- edge_attr(community_subgraph, "tender_count")
        }
        
        # Add tooltip
        edges_df$title <- ifelse(
          "tender_count" %in% names(edges_df),
          paste0("Total Award: $", format(round(edges_df$weight, 2), big.mark=","), 
                 "<br>Tenders: ", edges_df$tender_count),
          paste0("Total Award: $", format(round(edges_df$weight, 2), big.mark=","))
        )
        
        # Set width
        edges_df$width <- if(n_distinct(edges_df$weight) > 1) {
          2 + 8 * (edges_df$weight - min(edges_df$weight)) / (max(edges_df$weight) - min(edges_df$weight))
        } else {
          5
        }
        
        # Set color
        edges_df$color <- "#E195AB"
      } else {
        # Create empty edges dataframe
        edges_df <- data.frame(
          from = character(0),
          to = character(0),
          weight = numeric(0),
          width = numeric(0),
          color = character(0),
          title = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      # Calculate node metrics for sizing
      if(vcount(community_subgraph) > 1) {
        nodes_df$value <- switch(input$centrality_measure,
                                 "Degree" = degree(community_subgraph),
                                 "Betweenness" = betweenness(community_subgraph),
                                 "Closeness" = closeness(community_subgraph),
                                 "Eigenvector" = eigen_centrality(community_subgraph)$vector,
                                 degree(community_subgraph))
      } else {
        nodes_df$value <- 1
      }
      
      # Add size based on node type
      nodes_df$size <- ifelse(nodes_df$type == "Agency", 25, 15)
      
      # Add tooltips
      nodes_df$title <- paste0("<p><strong>", nodes_df$label, "</strong><br>",
                               "Type: ", nodes_df$type, "<br>",
                               "Community: ", selected_community, "<br>",
                               input$centrality_measure, ": ", round(nodes_df$value, 3), "</p>")
      
      # Update node counts for metrics
      rv$community_stats <- list(
        community_id = selected_community,
        total_nodes = nrow(nodes_df),
        agencies = sum(nodes_df$type == "Agency"),
        suppliers = sum(nodes_df$type == "Supplier")
      )
      
      # Generate visualNetwork
      visNetwork(nodes = nodes_df, edges = edges_df) %>%
        visPhysics(solver = "forceAtlas2Based",
                   forceAtlas2Based = list(gravitationalConstant = -50,
                                           centralGravity = 0.01,
                                           springLength = 100,
                                           springConstant = 0.08),
                   stabilization = list(iterations = 100)) %>%
        visGroups(groupname = "Agency", 
                  color = list(background = "#39B5E0", border = "#E3F6FF"),
                  shape = "diamond") %>%
        visGroups(groupname = "Supplier", 
                  color = list(background = "#E4003A", border = "#E195AB"),
                  shape = "dot") %>%
        visNodes(scaling = list(min = 10, max = 30),
                 font = list(size = 12),
                 shadow = TRUE) %>%
        visEdges(smooth = list(enabled = TRUE, type = "continuous"),
                 arrows = list(to = FALSE, from = FALSE),
                 hoverWidth = 3) %>%
        visInteraction(navigationButtons = TRUE,
                       keyboard = TRUE,
                       tooltipDelay = 100) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   height = "500px") %>%
        visLegend(enabled = TRUE, 
                  useGroups = TRUE, 
                  position = "right") %>%
        visLayout(randomSeed = 123) %>%
        addFontAwesome() %>%
        htmlwidgets::onRender(paste0(
          "function(el, x, data) {
        var title = document.createElement('div');
        title.style.textAlign = 'center';
        title.style.fontWeight = 'bold';
        title.innerHTML = 'Community ", selected_community, " Network<br>",
          nrow(nodes_df), " nodes and ", nrow(edges_df), " connections';
        title.style.fontSize = '14px';
        title.style.marginBottom = '10px';
        el.insertBefore(title, el.firstChild);
      }"))
    })
    
    
    # Render community detail metrics
    output$community_detail_metrics <- renderUI({
      req(rv$graph, input$selected_community)
      
      # Debug log
      cat("Rendering metrics for community:", input$selected_community, "\n")
      
      # Get the selected community
      selected_community <- input$selected_community
      
      # Use the community_stats set by the ego_community_network 
      if(!is.null(rv$community_stats) && rv$community_stats$community_id == selected_community) {
        n_nodes <- rv$community_stats$total_nodes
        n_agencies <- rv$community_stats$agencies
        n_suppliers <- rv$community_stats$suppliers
        
        # Debug
        cat("Using stored community stats - Nodes:", n_nodes, 
            "Agencies:", n_agencies, "Suppliers:", n_suppliers, "\n")
      } else {
        # If community_stats isn't available, calculate metrics directly
        community_nodes <- rv$graph %>%
          activate(nodes) %>%
          filter(community == selected_community) %>%
          as_tibble()
        
        # Calculate metrics for the selected community
        n_nodes <- nrow(community_nodes)
        n_agencies <- sum(community_nodes$type == "Agency")
        n_suppliers <- sum(community_nodes$type == "Supplier")
        
        # Debug output
        cat("Calculating new community stats - Nodes:", n_nodes, 
            "Agencies:", n_agencies, "Suppliers:", n_suppliers, "\n")
      }
      
      # Calculate density
      igraph_obj <- as.igraph(rv$graph)
      community_indices <- which(V(igraph_obj)$community == selected_community)
      
      # Safe density calculation
      if(length(community_indices) > 1) {
        subgraph <- induced_subgraph(igraph_obj, community_indices)
        density <- edge_density(subgraph)
      } else {
        density <- 0  # Default for communities with 0-1 nodes
      }
      
      cat("Calculated density:", density, "\n")
      
      # Create the UI with icons
      fluidRow(
        valueBox(
          n_nodes, "Community Members", icon = shiny::icon("fa-users", lib = "font-awesome"), 
          color = "light-blue", width = 3
        ),
        valueBox(
          n_agencies, "Agencies", icon = shiny::icon("fa-building", lib = "font-awesome"), 
          color = "light-blue", width = 3
        ),
        valueBox(
          n_suppliers, "Suppliers", icon = shiny::icon("fa-truck", lib = "font-awesome"), 
          color = "light-blue", width = 3
        ),
        valueBox(
          round(density, 3), "Internal Density", icon = shiny::icon("fa-project-diagram", lib = "font-awesome"), 
          color = "light-blue", width = 3
        )
      )
    })
    
    # Render community nodes table
    output$community_nodes_table <- renderDT({
      # Get the selected community from either input
      selected_community <- input$selected_community_members
      if (is.null(selected_community)) {
        selected_community <- input$selected_community
      }
      
      req(rv$graph, selected_community)
      
      # Get nodes in the selected community with metrics
      community_nodes <- rv$graph %>%
        activate(nodes) %>%
        filter(community == selected_community) %>%
        mutate(
          degree = centrality_degree(),
          betweenness = centrality_betweenness(),
          closeness = centrality_closeness(),
          eigenvector = centrality_eigen()
        ) %>%
        as_tibble() %>%
        select(name, type, degree, betweenness, closeness, eigenvector) %>%
        arrange(desc(!!rlang::sym(tolower(input$centrality_measure))))
      
      # Format for display
      community_nodes %>%
        rename(
          Node = name,
          Type = type,
          Degree = degree,
          Betweenness = betweenness,
          Closeness = closeness,
          Eigenvector = eigenvector
        ) %>%
        DT::datatable(options = list(pageLength = 10))
    })
  })
  
}

#---------------------------------
# Define the main UI and server
#---------------------------------

# UI
ui <- network_community_ui("community_module")

server <- function(input, output, session) {
  # Process the data once, not as a reactive

  has_tender_cat <- "tender_cat" %in% names(community_data_global)
  has_tender_type <- "tender_type" %in% names(community_data_global)
  
  # Process data with appropriate column handling
  community_data_processed <- community_data_global %>%
    mutate(
      award_date = as.Date(award_date),
      awarded_amt = as.numeric(awarded_amt)
    )
  
  # Only rename if needed
  if (has_tender_cat && !has_tender_type) {
    community_data_processed <- community_data_processed %>%
      rename(tender_type = tender_cat)
  }
  
  # debug
  cat("Processed data dimensions:", nrow(community_data_processed), "rows\n")
  
  # Pass to module
  network_community_server("community_module", community_data_processed)
}

# Run the application 
shinyApp(ui = ui, server = server)