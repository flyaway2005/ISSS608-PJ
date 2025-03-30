
#------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(networkD3)
library(igraph)
library(DT)
library(visNetwork)
library(RColorBrewer)

#------------------------------------------------------------------------------
# Set global options

# Increase file upload size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Configure default data folder
data_folder <- "data"

# Set default timeout
options(shiny.fullstacktrace = TRUE)
options(shiny.useragg = TRUE)  # For improved plot rendering

#------------------------------------------------------------------------------
# App-wide theme settings and styling

# Define the color palette
app_theme <- list(
  # Color palette - use this consistently across modules
  colors = list(
    primary = "#B7CADB",       
    primary_dark = "#A7BDCD",  
    primary_light = "#D7EAFB", 
    secondary = "#F7E3C1",     
    background = "#FDF6EC",    
    text = "#444444",          
    success = "#91ac8f",       
    warning = "#F0AD4E",       
    danger = "#D9534F",        
    info = "#5c7285",       
    neutral = "#f4f4f4",    # Light gray background
    # Add more colors as needed
    light_gray = "#f9f9f9",
    dark_gray = "#444444",
    highlight = "#605ca8"  
  ),
  
  # Font settings
  fonts = list(
    main = "Helvetica Neue, Helvetica, Arial, sans-serif",
    headers = "Helvetica Neue, Helvetica, Arial, sans-serif",
    code = "Consolas, Monaco, monospace"
  ),
  
  # Existing custom CSS
  css = "
    /* Header background color */
    .skin-blue .main-header .navbar {
      background-color: #B7CADB;
    }
    
    /* Logo background color */
    .skin-blue .main-header .logo {
      background-color: #A7BDCD;
    }
    
    /* Logo hover color */
    .skin-blue .main-header .logo:hover {
      background-color: #97ADBD;
    }
    
    /* Sidebar background color */
    .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
      background-color: #FDF6EC;
    }
    
    /* Sidebar text color */
    .skin-blue .sidebar a {
      color: #444;
    }
    
    /* Sidebar width */
    .main-sidebar {
      width: 208px;
    }
    
    /* Force consistent sidebar width */
    .sidebar-panel, .well {
    width: 100% !important;
    max-width: 100% !important;
    }
    
    /* Ensure accordion doesn't overflow */
    .panel-group {
    width: 100% !important;
    }
    
    /* Ensure inputs don't cause width changes */
    .selectize-control, .form-control, .shiny-input-container {
    width: 100% !important;
    max-width: 100% !important;
    }
    
    /* Content wrapper adjustment */
    .content-wrapper, .right-side {
      margin-left: 200px;
    }
    
    /* Active sidebar item */
    .skin-blue .sidebar-menu > li.active > a,
    .skin-blue .sidebar-menu > li:hover > a {
      color: #000;
      background: #F7E3C1;
      border-left-color: #B7CADB;
    }
    
    /* Box borders */
    .box.box-primary {
      border-top-color: #B7CADB;
    }
    
    /* Button colors */
    .btn-primary {
      background-color: #B7CADB;
      border-color: #A7BDCD;
    }
    
    .btn-primary:hover {
      background-color: #A7BDCD;
    }
    
    /* Control panel styling */
    .control-panel {
      background-color: #F8F8F8;
      border-right: 1px solid #ddd;
      padding: 15px;
      margin-bottom: 15px;
    }
    
    /* Main layout with flexbox */
    .tab-content {
      display: flex;
      flex-direction: row;
    }
    
    /* Control panel width */
    .control-section {
      width: 250px;
      flex-shrink: 0;
    }
    
    /* Content section */
    .content-section {
      flex-grow: 1;
      padding-left: 15px;
    }
    
    /* Force consistent sidebar width across all modules */
.sidebar-panel {
  width: 33.33% !important;
  max-width: 33.33% !important;
  flex: 0 0 33.33% !important;
}

/* Main panel consistency */
.main-panel {
  width: 66.67% !important;
  max-width: 66.67% !important;
  flex: 0 0 66.67% !important;
}

/* Ensures the sidebarLayout container itself is full width */
.shiny-tab-content .sidebarLayout {
  width: 100% !important;
}

/* Ensure all boxes in main content expand properly */
.shiny-tab-content .box {
  width: 100% !important;
  max-width: 100% !important;
}

/* Fix for network visualization containers */
.visNetwork {
  width: 100% !important;
  height: 100% !important;
}

/* Ensure all inputs maintain consistent width */
.sidebar-panel .shiny-input-container {
  width: 100% !important;
}

/* Fix accordion width */
.sidebar-panel .panel-group {
  width: 100% !important;
}

/* Force ALL sidebar panels to be exactly the same width */
.sidebar-panel, .well, .shiny-split-layout > div:first-child {
  width: 33.33% !important;
  max-width: 33.33% !important;
  min-width: 33.33% !important;
  flex: 0 0 33.33% !important;
  box-sizing: border-box !important;
}

/* Fix ALL main panels */
.main-panel, .shiny-split-layout > div:last-child {
  width: 66.67% !important;
  max-width: 66.67% !important;
  min-width: 66.67% !important;
  flex: 0 0 66.67% !important;
  box-sizing: border-box !important;
}

/* Fix the tabset container */
.tab-content, .tab-pane {
  width: 100% !important;
  max-width: 100% !important;
  box-sizing: border-box !important;
}

/* Target specific tab layouts */
#network_tabs-Ego-Network .sidebar-panel,
#network_tabs-Ego-Metrics .sidebar-panel,
#network_tabs-Network-Metrics .sidebar-panel,
#network_tabs-Network-Overview .sidebar-panel {
  width: 33.33% !important;
  max-width: 33.33% !important;
  flex: 0 0 33.33% !important;
}

/* Override any Bootstrap column widths in the sidebar */
.sidebar-panel .col-sm-1, .sidebar-panel .col-sm-2, .sidebar-panel .col-sm-3,
.sidebar-panel .col-sm-4, .sidebar-panel .col-sm-5, .sidebar-panel .col-sm-6,
.sidebar-panel .col-sm-7, .sidebar-panel .col-sm-8, .sidebar-panel .col-sm-9,
.sidebar-panel .col-sm-10, .sidebar-panel .col-sm-11, .sidebar-panel .col-sm-12 {
  width: 100% !important;
  max-width: 100% !important;
  flex: 0 0 100% !important;
}

/* Fix the sidebar layout container and all its children */
.sidebarLayout, .sidebarLayout > * {
  width: 100% !important;
  max-width: 100% !important;
  box-sizing: border-box !important;
}

/* Fix panel display issues */
.panel-body {
  width: 100% !important;
  padding: 5px !important;
  box-sizing: border-box !important;
}

/* Ensure consistent layout for network module */
.network-module-container {
  width: 100% !important;
  max-width: 100% !important;
}

.network-sidebar {
  width: 33.33% !important;
  max-width: 33.33% !important;
  flex: 0 0 33.33% !important;
}

.network-main-panel {
  width: 66.67% !important;
  max-width: 66.67% !important;
  flex: 0 0 66.67% !important;
}

.network-container {
  height: calc(100vh - 250px);
  min-height: 600px;
}

/* Force taller visualization containers */
.visNetwork, .visNetwork > div, .visNetwork canvas {
  min-height: 500px !important;
  height: 500px !important;
}

/* Target the specific network plot element */
#network_module-network_plot, 
#network_module-network_plot .visNetwork, 
#network_module-network_plot .htmlwidget, 
#network_module-network_plot .htmlwidget-container {
  height: 400px !important;
  min-height: 400px !important;
  max-height: 400px !important;
}

/* Force the container to be tall enough */
.shiny-output-error, 
.visNetwork-container,
.htmlwidget-container {
  min-height: 500px !important; 
  height: 500px !important;
}

/* Make sure the visualization canvas is in the right place */
canvas.vis-network-canvas {
  position: relative !important;
  height: 500px !important;
}

/* Make sure visualizations use full width */
.network-main-panel .visNetwork,
.network-main-panel .box {
  width: 100% !important;
}

/* Target the ego network plot specifically */
#network_module-ego_network_plot,
#network_module-ego_network_plot .visNetwork,
#network_module-ego_network_plot .htmlwidget,
#network_module-ego_network_plot .htmlwidget-container {
  height: 390px !important;
  min-height: 390px !important;
  max-height: 390px !important;
}

/* Also target the community module */
#community_module-ego_network_plot,
#community_module-ego_network_plot .visNetwork,
#community_module-ego_network_plot .htmlwidget,
#community_module-ego_network_plot .htmlwidget-container {
  height: 390px !important;
  min-height: 390px !important;
  max-height: 390px !important;
}

/* Debug outline to see containers - remove after fixing
.sidebar-panel { border: 2px solid red !important; }
.main-panel { border: 2px solid blue !important; }
*/
  "
)

  
  # Text sizes
  text_sizes = list(
    title = 20,
    subtitle = 16,
    header = 14,
    body = 12,
    caption = 10
  )

# Standard ggplot2 theme function to be used across modules
theme_dashboard <- function() {
  theme_minimal() +
    theme(
      # Text elements
      plot.title = element_text(
        size = app_theme$text_sizes$title, 
        face = "bold", 
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = app_theme$text_sizes$subtitle,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      axis.title = element_text(size = app_theme$text_sizes$header),
      axis.text = element_text(size = app_theme$text_sizes$body),
      legend.title = element_text(size = app_theme$text_sizes$header, face = "bold"),
      legend.text = element_text(size = app_theme$text_sizes$body),
      
      # Grid elements
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_line(color = "#f0f0f0"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Legend position
      legend.position = "bottom"
    )
}

# Standard color scales for different variable types
discrete_color_scale <- scale_color_brewer(palette = "Set1")
continuous_color_scale <- scale_color_viridis_c()

# Custom themed box function
themed_box <- function(title, width = 12, status = "primary", solidHeader = FALSE, 
                       collapsible = TRUE) {
  box(
    title = title,
    width = width,
    status = status,
    solidHeader = solidHeader,
    collapsible = collapsible,
  )
}

#------------------------------------------------------------------------------
# Data loading and preprocessing

# Function to load sample data 
load_sample_data <- function() {
  # Static sample dataset, load it here
  # data <- read_csv("data/sample_data.csv")
  
  # For demonstration, use built-in datasets
  data <- mtcars
  
  # Add date columns for time series analysis
  data$date <- seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = nrow(data))
  
  return(data)
}

#------------------------------------------------------------------------------
# Shared helper functions for analysis
#------------------------------------------------------------------------------
# Format numbers with commas
format_number <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}

# Format percentages
format_percent <- function(x, digits = 1) {
  paste0(format(round(x * 100, digits), nsmall = digits), "%")
}

# Format currency values
format_currency <- function(x, symbol = "$", digits = 2) {
  paste0(symbol, format(round(x, digits), big.mark = ",", nsmall = digits))
}

# Function to generate summary statistics for a numeric variable
generate_summary_stats <- function(data, variable) {
  if (!variable %in% names(data) || !is.numeric(data[[variable]])) {
    return(data.frame(
      Statistic = "Error",
      Value = "Not a numeric variable"
    ))
  }
  
  # Remove NAs for calculation
  values <- data[[variable]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(data.frame(
      Statistic = "Error",
      Value = "No non-NA values"
    ))
  }
  
  # Calculate statistics
  stats <- data.frame(
    Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", 
                  "Maximum", "Standard Deviation", "Count", "Missing"),
    Value = c(
      min(values),
      quantile(values, 0.25),
      median(values),
      mean(values),
      quantile(values, 0.75),
      max(values),
      sd(values),
      length(values),
      sum(is.na(data[[variable]]))
    )
  )
  
  return(stats)
}

#------------------------------------------------------------------------------
# Load any global data that will be shared across sessions

# Pre-load any reference data that all users will share
sample_data <- load_sample_data()

# If you have any lookup tables or configuration data, load them here
# config_data <- read_csv("data/config.csv")
# lookup_tables <- list(
#   countries = read_csv("data/countries.csv"),
#   categories = read_csv("data/categories.csv")
# )

#------------------------------------------------------------------------------
# Custom error handling

# Function to safely run code with error handling
safely_run <- function(expr, default = NULL, error_message = "An error occurred") {
  result <- tryCatch(
    {
      expr
    },
    error = function(e) {
      message(paste0(error_message, ": ", e$message))
      return(default)
    },
    warning = function(w) {
      message(paste0("Warning: ", w$message))
      expr
    }
  )
  return(result)
}

#------------------------------------------------------------------------------
# Debug mode and logging

# Set debug mode (TRUE/FALSE)
DEBUG_MODE <- TRUE

# Custom logging function
log_message <- function(message, level = "INFO") {
  if (DEBUG_MODE || level %in% c("ERROR", "WARNING")) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste0("[", level, " ", timestamp, "] ", message))
  }
}