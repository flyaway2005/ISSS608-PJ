# Module 2-1 

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(shinyWidgets)
library(plotly)
library(ggstatsplot)


# Load preprocessed network data
ts_overview_data <- read_csv("data/GeBiz_add_y_m.csv")
head(ts_overview_data)

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
time_series_ui <- function(id) {
  ns <- NS(id)

fluidPage(
  titlePanel("Procurement Trends Overview"),
  
  sidebarLayout(
    sidebarPanel(

      # Date range filter
      dateRangeInput(ns("date_range"), "Date Range",
                     start = as.Date("2019-04-01"),
                     end = as.Date("2024-03-31"),
                     min = as.Date("2019-04-01"),
                     max = as.Date("2024-03-31")),
      
      # Agency selection (multi-select)
      selectizeInput(ns("agency"), "Select Agency",
                     choices = "All",
                     multiple = TRUE,
                     selected = "All"),
      
      # Supplier selection (multi-select)
      selectizeInput(ns("supplier"), "Select Supplier",
                     choices = NULL,
                     multiple = TRUE,
                     selected = "All"),
      
      # Tender type selection 
      selectizeInput(ns("tender_type"), "Tender Status",
                     choices = NULL, # Will be set in server
                     multiple = TRUE,
                     selected = "All"),
      
      # Stats Tests control 
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Stats Tests'", ns("tab_selected")),
        div(
          style = "border-top: 1px solid #ddd; padding-top: 10px; margin-top: 10px;",
          h4("Stats Test Control"),
          radioButtons(
            ns("test_type"), 
            "Statistical test type:",
            choices = c("Parametric (ANOVA/t-test)" = "parametric", 
                        "Non-parametric (Kruskal-Wallis/Wilcoxon)" = "nonparametric"),
            selected = "parametric"
          )
        )
      ),
      
      # Update button
      actionButton(ns("update_btn"), "Update Visualisation", 
                   class = "btn-primary btn-block",
                   style = "margin-top: 20px;")
    ),
    
    mainPanel(
      tabsetPanel(
        id = ns("tab_selected"),
        
        tabPanel("Overview", 
                 fluidRow(

                   column(12,
                          div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px; margin-bottom: 15px;",
                              h4("Procurement Trend", style = "margin: 0;"),
                              div(
                                prettyRadioButtons(
                                  inputId = ns("time_period"),
                                  label = "View by:",
                                  choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month"),
                                  selected = "year",
                                  inline = TRUE,
                                  status = "primary",
                                  fill = TRUE)
                              )
                          )
                 )),
                 plotlyOutput(ns("time_series_plot")),
        
                 # Metrics table below the plot
                 h4("Key Metrics", style = "margin-top: 30px;"),
                 div(style = "overflow-x: auto;",
                    tableOutput(ns("metrics_table"))
                 )
        ),
      
        tabPanel("Seasonal Patterns",
                 fluidRow(
                   column(12, 
                          div(style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
                              prettyRadioButtons(
                                inputId = ns("seasonal_view"),
                                label = "View seasonal pattern by:",
                                choices = c("Quarter" = "quarter", "Month" = "month"),
                                selected = "quarter",
                                inline = TRUE,
                                status = "primary",
                                fill = TRUE))
                   )),
                 # Two season visualizations stacked
                 fluidRow(
                   column(12, 
                          h4("Seasonal Cycle Plot"),
                             plotlyOutput(ns("seasonal_cycle_plot"), height = "280px")
                          )
                             ),
                   fluidRow(
                     column(12,
                            h4("Seasonal Trends by Year"),
                            plotlyOutput(ns("seasonal_line_plot"), height = "250px")
                            )
                 ),
                 
                 # Seasonal patterns explanation
                 fluidRow(
                   column(12, 
                          div(style = "margin-top: 20px;",
                              h4("Seasonal Analysis"),
                              textOutput(ns("seasonal_analysis"))
                          )
                   )
                 )
        ),
        tabPanel("ANOVA Test",
                 fluidRow(
                   column(12, 
                          div(style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
                              prettyRadioButtons(
                                inputId = ns("stat_time_period"),
                                label = "Compare by:",
                                choices = c("Year" = "award_year", "Quarter" = "quarter"),
                                selected = "award_year",
                                inline = TRUE,
                                status = "primary",
                                fill = TRUE)
                          ))
                 ),
                 fluidRow(
                   column(7,
                          h4("One-Way ANOVA Test"),
                          plotOutput(ns("stats_plot"))
                   ),
                   column(5,
                          h4("Test Results"),
                          verbatimTextOutput(ns("stats_results"))
                   )
                 ))
      )
    )
  )
)
}

#-----------------------------------------------
# Server for time series analysis
#-----------------------------------------------

time_series_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Initialize a reactive value to store the current filter settings
    filter_settings <- reactiveVal(list(
      date_range = NULL,
      agency = "All",
      supplier = "All",
      tender_type = "All",
      time_period = "year",
      seasonal_view = "quarter"
    ))
    
    # Flag to track if initial data load has occurred
    initial_load <- reactiveVal(FALSE)

    # Initialise UI elements once data is available
    observe({
      # Update date range input
      updateDateRangeInput(session, "date_range",
                           start = min(data$award_date),
                           end = max(data$award_date),
                           min = as.Date("2019-04-01"),
                           max = as.Date("2024-03-31"))
      
      # Update agency selection
      agency_choices <- c("All", sort(unique(data$agency)))
      updateSelectizeInput(session, "agency",
                           choices = agency_choices,
                           selected = "All")
      
      # Update supplier selection
      supplier_choices <- c("All", sort(unique(data$supplier_name)))
      updateSelectizeInput(session, "supplier",
                           choices = supplier_choices,
                           selected = "All")
      
      # Update tender type selection
      if("tender_detail_status" %in% names(data)) {
        tender_choices <- c("All", sort(unique(data$tender_detail_status)))
        updateSelectizeInput(session, "tender_type",
                             choices = tender_choices,
                             selected = "All")
      }
      
      initial_load(TRUE)
    })    
  
    # Only update filter settings when update button is clicked
    observeEvent(input$update_btn, {
      # Store current filter settings
      filter_settings(list(
        date_range = input$date_range,
        agency = input$agency,
        supplier = input$supplier,
        tender_type = input$tender_type,
        time_period = input$time_period,
        seasonal_view = input$seasonal_view
      ))
    })
    
  # Reactive filtered dataset based on user inputs
  filtered_data <- reactive({
    req(initial_load())
    
    # Get current filter settings
    current_filters <- filter_settings()
    filtered <- data
    
    tryCatch({
    # Filter by date range
    date_range <- if (!is.null(current_filters$date_range)) {
      current_filters$date_range
    } else {
      c(min(data$award_date), max(data$award_date))
    }
    
    filtered <- filtered %>%
      filter(award_date >= date_range[1] & award_date <= date_range[2])
    
    # Filter by agency if not "All"
    if (!"All" %in% input$agency && length(input$agency) > 0) {
      filtered <- filtered %>% filter(agency %in% input$agency)
    } else {
      # If "All" is selected, don't filter
      print("All agencies selected, no filtering applied")
    }
    
    # Filter by supplier if not "All"
    if (!"All" %in% input$supplier && length(input$supplier) > 0) {
      filtered <- filtered %>% filter(supplier_name %in% input$supplier)
    } else {
      # If "All" is selected, don't filter
      print("All suppliers selected, no filtering applied")
    }
    print("After supplier filter:")
    print(dim(filtered))
    
    # Filter by tender type if not "All"
    if ("tender_detail_status" %in% names(filtered) && 
        !"All" %in% input$tender_type && 
        length(input$tender_type) > 0) {
      filtered <- filtered %>% filter(tender_detail_status %in% input$tender_type)
    } else {
      print("All tender types selected or column not found, no filtering applied")
    }
    print("After tender type filter:")
    print(dim(filtered))
    
    return(filtered)
    }, error = function(e) {
      print("Error in filtered_data reactive: ")
      print(e)
      return(data)
    })
  })
  
  # Get the currently active time period 
  current_time_period <- reactive({
    return(filter_settings()$time_period)
  })
  
  # Get the currently active seasonal view 
  current_seasonal_view <- reactive({
    return(filter_settings()$seasonal_view)
  })  
  
  # Aggregate data based on selected time period
  time_aggregated_data <- reactive({
    req(filtered_data())
    aggregate_by_time(filtered_data(), input$time_period)
  })
  
#-----  
  # Create metrics from the aggregated data
#-----
  metrics_data <- reactive({
    req(time_aggregated_data())
    
    plot_data <- time_aggregated_data()
    
    # Calculate key metrics
    total <- sum(plot_data$total_awarded, na.rm = TRUE)
    avg <- mean(plot_data$total_awarded, na.rm = TRUE)
    median_val <- median(plot_data$total_awarded, na.rm = TRUE)
    max_val <- max(plot_data$total_awarded, na.rm = TRUE)
    max_period <- plot_data$time_label[which.max(plot_data$total_awarded)]
    min_val <- min(plot_data$total_awarded, na.rm = TRUE)
    min_period <- plot_data$time_label[which.min(plot_data$total_awarded)]
    
    # Calculate growth metrics if there are at least 2 periods
    if(nrow(plot_data) >= 2) {
      # Sort by time label to ensure chronological order
      if(current_time_period() == "year") {
        plot_data <- plot_data %>% arrange(as.numeric(time_label))
      } else if(current_time_period() == "quarter") {
        plot_data <- plot_data %>% arrange(time_label)
      } else { # month
        plot_data <- plot_data %>% arrange(time_label)
      }
      
      first_period_value <- plot_data$total_awarded[1]
      last_period_value <- plot_data$total_awarded[nrow(plot_data)]
      total_growth <- last_period_value - first_period_value
      pct_growth <- (total_growth / first_period_value) * 100
      
      # Calculate average period-over-period growth
      pop_growth <- NA
      if(nrow(plot_data) > 2) {
        # Calculate period-over-period percentage changes
        plot_data <- plot_data %>%
          mutate(pop_pct = (total_awarded / lag(total_awarded) - 1) * 100)
        pop_growth <- mean(plot_data$pop_pct, na.rm = TRUE)
      }
    } else {
      total_growth <- NA
      pct_growth <- NA
      pop_growth <- NA
    }
    
    # Create a data frame for the metrics table
    metrics_df <- data.frame(
      Metric = c(
        paste0("Total Awarded (All ", tools::toTitleCase(current_time_period()), "s)"),
        paste0("Average per ", tools::toTitleCase(current_time_period())),
        paste0("Median per ", tools::toTitleCase(current_time_period())),
        paste0("Highest Award Period"),
        paste0("Lowest Award Period"),
        "Total Growth (First to Last Period)",
        "Percentage Growth (First to Last Period)",
        "Average Period-over-Period Growth"
      ),
      Value = c(
        paste0("S$", format(round(total), big.mark = ",")),
        paste0("S$", format(round(avg), big.mark = ",")),  
        paste0("S$", format(round(median_val), big.mark = ",")),
        paste0(max_period, " (S$", format(round(max_val), big.mark = ","), ")"),
        paste0(min_period, " (S$", format(round(min_val), big.mark = ","), ")"),
        ifelse(is.na(total_growth), "N/A", 
               paste0("S$", format(round(total_growth), big.mark = ","))), 
        ifelse(is.na(pct_growth), "N/A", 
               paste0(round(pct_growth, 1), "%")),
        ifelse(is.na(pop_growth), "N/A", 
               paste0(round(pop_growth, 1), "%"))
      )
    )
    
    return(metrics_df)
  })
  
  #--------
  # Reactive for seasonal data aggregation
  #--------
  seasonal_data <- reactive({
    req(filtered_data())
    filtered <- filtered_data()
#--------    
    # debug
    print("Filtered data dimensions for seasonal view:")
    print(dim(filtered))
    
    if (nrow(filtered) == 0) {
      # Return a skeleton data frame to prevent errors
      return(data.frame(
        season = factor(c("Q1", "Q2", "Q3", "Q4"), levels = c("Q1", "Q2", "Q3", "Q4")),
        award_year = rep(2022, 4),
        total_awarded = rep(0, 4)
      ))
    }
#--------
    
    if (current_seasonal_view() == "quarter") {
      result <- filtered %>%
        mutate(season = sub(".*-(Q[1-4])", "\\1", quarter)) %>%
        group_by(season, award_year) %>%
        summarise(total_awarded = sum(awarded_amt, na.rm = TRUE), .groups = "drop") %>%
        # Make sure quarters are properly ordered
        mutate(season = factor(season, levels = c("Q1", "Q2", "Q3", "Q4")))
      
      print("Created quarter seasonal data with dimensions:")
      print(dim(result))
      return(result)
      
    } else { # month view - Extract just the month and convert to month name
      result <- filtered %>%
        mutate(
          month_num = sub(".*-(\\d{2})", "\\1", year_month),
          season = month.abb[as.numeric(month_num)]
        ) %>%
        group_by(season, award_year) %>%
        summarize(total_awarded = sum(awarded_amt, na.rm = TRUE), .groups = "drop") %>%
        mutate(season = factor(season, levels = month.abb)) # Make sure months are properly ordered
      
      print("Created month seasonal data with dimensions:")
      print(dim(result))
      return(result)
    }
  })
  
  # Aggregated seasonal data (across all years)
  aggregated_seasonal <- reactive({
    req(seasonal_data())
    
    seasonal_data() %>%
      group_by(season) %>%
      summarise(
        total_awarded = sum(total_awarded, na.rm = TRUE),
        avg_awarded = mean(total_awarded, na.rm = TRUE),
        .groups = "drop"
      )
  })
#---------------------  
  # Render the cycle plot 
#--------------------- 
  output$seasonal_cycle_plot <- renderPlotly({
    # Debug
    print("Seasonal Data:")
    print(seasonal_data())
    
    req(seasonal_data())
    
    # For cycle plots, need data in wide format 
    cycle_data <- seasonal_data()
    
#------
    # Debug: print what we got
    print("Cycle data dimensions:")
    print(dim(cycle_data))
    print("Column names:")
    print(names(cycle_data))
    
    if (nrow(cycle_data) == 0 || all(cycle_data$total_awarded == 0)) {
      # Return an empty plot with a message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available with current filters") +
        theme_void()
      return(ggplotly(p))
    }
#------
    # Calculate averages for each season
    season_avgs <- cycle_data %>%
      group_by(season) %>%
      summarise(avg_total = mean(total_awarded, na.rm = TRUE))
    
    
    # Create the cycle plot
    p <- ggplot(data = cycle_data, 
                aes(x = award_year, y = total_awarded, group = 1)) +
      geom_line(color = "black", size = 0.3) +
      geom_hline(data = season_avgs, 
                 aes(yintercept = avg_total), 
                 linetype = "dashed", color = "red", size = 0.3) +
      facet_wrap(~ season, nrow = 1) +
      labs(
        x = "Year",
        y = "Total Awarded Amount (S$ Billions)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 7),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        strip.background = element_rect(fill = "#C4E1F6", color = "gray"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.margin = margin(t = 20, r = 25, b = 20, l = 25, unit = "pt") 
      ) +
      scale_y_continuous(labels = function(x) {
        paste0(format(round(x/1e9), big.mark = ","))
      })
    
    # Plotly
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
#---------------------------------
  # Render the seasonal line chart 
#---------------------------------
  output$seasonal_line_plot <- renderPlotly({
    req(seasonal_data())
    
#   unique_years <- sort(unique(seasonal_data()$award_year))
    
    p <- ggplot(seasonal_data(), 
           aes(x = season, y = total_awarded / 1e9, 
               group = factor(award_year), 
               color = factor(award_year),
               text = paste(
                 "Year:", award_year,
                 "<br>Season:", season,
                 "<br>Total Awarded: S$",
                 format(round(total_awarded / 1e6), big.mark = ","), "Mil"
               ))) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1) +
      scale_color_manual(values = c("2019" = "#37B7C3", "2020" = "#708871", "2021" = "#FFAF45",
                                    "2022" = "#B7CADB", "2023" = "#B5828C", "2024" = "#8967B3")) +
      labs(
        x = ifelse(current_seasonal_view() == "quarter", "Quarter", "Month"),
        y = "Total Awarded Amount \n(S$ Billion)",
        color = "Year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 7),    
        legend.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)
      )
    
    # ggplotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 80, r = 50, b = 80, t = 50, pad = 4)
      )
  })
  
  # Seasonal analysis text
  output$seasonal_analysis <- renderText({
    
    req(aggregated_seasonal())
    
    agg_data <- aggregated_seasonal()
    
    if (nrow(agg_data) == 0 || !all(c("season", "total_awarded") %in% names(agg_data))) {
      return("Insufficient data to perform seasonal analysis. Try adjusting your filters.")
    }
    
    # Check for non-NA values
    if (all(is.na(agg_data$total_awarded))) {
      return("Cannot perform analysis: All values are NA.")
    }
    
    # Find highest and lowest seasons
    highest_season <- agg_data$season[which.max(agg_data$total_awarded)]
    lowest_season <- agg_data$season[which.min(agg_data$total_awarded)]
    
    # Calculate coefficient of variation to measure seasonality strength
    cv <- sd(agg_data$total_awarded) / mean(agg_data$total_awarded) * 100
    
    # Generate analysis text
    paste0(
      "Based on historical data, ", highest_season, " shows the highest procurement activity, ",
      "while ", lowest_season, " shows the lowest. ",
      "The seasonal variation is ", 
      ifelse(cv < 10, "relatively low (less than 10% variation)", 
             ifelse(cv < 25, "moderate", "strong (greater than 25% variation)")),
      ", with a coefficient of variation of ", round(cv, 1), "%. ",
      "The line chart shows how these seasonal patterns have evolved year by year.",
      " "
    )
  })
  
  
  # Render the metrics table
  output$metrics_table <- renderTable({
    metrics_data()
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  
  # Render the time series plot
  output$time_series_plot <- renderPlotly({
    req(time_aggregated_data())
    plot_data <- time_aggregated_data()
    
    # Get the detailed data for tooltips
    detailed_data <- filtered_data()
    
    # Create a data frame with tooltip information
    tooltip_data <- plot_data %>%
      mutate(
        # Count agencies, suppliers, and tenders for each time period
        agency_count = sapply(time_label, function(tl) {
          if(current_time_period() == "year") {
            period_data <- detailed_data %>% filter(award_year == tl)
          } else if(current_time_period() == "quarter") {
            period_data <- detailed_data %>% filter(quarter == tl)
          } else { # month
            period_data <- detailed_data %>% filter(year_month == tl)
          }
          return(length(unique(period_data$agency)))
        }),
        supplier_count = sapply(time_label, function(tl) {
          if(current_time_period() == "year") {
            period_data <- detailed_data %>% filter(award_year == tl)
          } else if(current_time_period() == "quarter") {
            period_data <- detailed_data %>% filter(quarter == tl)
          } else { # month
            period_data <- detailed_data %>% filter(year_month == tl)
          }
          return(length(unique(period_data$supplier_name)))
        }),
        tender_count = sapply(time_label, function(tl) {
          if(current_time_period() == "year") {
            period_data <- detailed_data %>% filter(award_year == tl)
          } else if(current_time_period() == "quarter") {
            period_data <- detailed_data %>% filter(quarter == tl)
          } else { # month
            period_data <- detailed_data %>% filter(year_month == tl)
          }
          return(nrow(period_data))
        }),
        # Create formatted text for tooltip
        tooltip_text = paste0(
          time_label, "<br>",
          "Total Awarded: S$", format(round(total_awarded / 1e6), big.mark = ","), " Million<br>",
          "# of Agencies: ", agency_count, "<br>",
          "# of Suppliers: ", supplier_count, "<br>",
          "# of Tenders: ", tender_count
        )
      )
    
# Dynamic title
    filters <- filter_settings()
    agency_filter <- if("All" %in% filters$agency || length(filters$agency) == 0) 
      "All Agencies" else paste(filters$agency, collapse=", ")
    
# ggplot
    p <- ggplot(tooltip_data, aes(x = time_label, y = total_awarded, 
                                  text = tooltip_text)) +
      geom_bar(stat = "identity", fill = "#9BB8CD") +
      labs(
        title = paste0("Total Awarded Amount by ", 
                       tools::toTitleCase(current_time_period())),
        subtitle = paste0("Period: ", format(filters$date_range[1], "%b %Y"), 
                          " to ", format(filters$date_range[2], "%b %Y")),
        x = tools::toTitleCase(current_time_period()),
        y = "Total Awarded Amount (S$ Billion)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10, face = "bold", color = "#51829B"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.margin = margin(t = 20, r = 25, b = 20, l = 25, unit = "pt") 
      ) +
      scale_y_continuous(labels = function(x) {
        paste0("$", format(x / 1e9, digits = 1, nsmall = 1))
      })
    
    # Convert to plotly with custom tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Force an initial update when the module loads
  observe({
    # Trigger this once after initial UI setup
    req(initial_load())
    
    # Set initial filter settings (only run once)
    isolate({
      if (is.null(filter_settings()$date_range)) {
        # Update the date range in the filter settings
        current_settings <- filter_settings()
        current_settings$date_range <- c(min(data$award_date), max(data$award_date))
        filter_settings(current_settings)
      }
    })
  })

#-----------------------------
#Test analysis section
#----------------------------
  observe({
    years <- sort(unique(data$award_year))
    # Default to comparing the latest three years
    default_years <- tail(years, min(3, length(years)))
    
    updateSelectizeInput(session, "compare_years",
                         choices = years,
                         selected = default_years)
  })
  
  # Create data for statistical comparison
  stats_data <- reactive({
    req(filtered_data())
    
    filtered <- filtered_data() %>%

#-----
#debug

    print("Creating stats data...")
    print(paste("Selected time period:", input$stat_time_period))
    print(paste("Filtered data has rows:", nrow(filtered)))    
#-----
    
    # For time periods other than year, we need to aggregate
    if(input$stat_time_period == "award_year") {
      if(length(unique(filtered$award_year)) < 2) {
        return(NULL) # Not enough years to compare
      }
      
      # Group by year and calculate statistics
      result <- filtered %>% 
        group_by(award_year) %>%
        summarize(
          mean_awarded = mean(awarded_amt, na.rm = TRUE),
          median_awarded = median(awarded_amt, na.rm = TRUE),
          n_awards = n(),
          total_awarded = sum(awarded_amt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(award_year = as.factor(award_year))
      
      print("Created year stats data")
      print(paste("Number of years:", nrow(result)))
      return(result)
      
    } else if(input$stat_time_period == "quarter") {
      # First check if quarter column exists
      if(!"quarter" %in% names(filtered)) {
        # Create quarter if it doesn't exist
        filtered <- filtered %>%
          mutate(quarter = paste0(award_year, "-Q", ceiling(as.numeric(month)/3)))
      }
      
      # Extract quarter (Q1, Q2, Q3, Q4) from the quarter column
      filtered <- filtered %>% 
        mutate(quarter_only = ifelse(grepl("-Q", quarter),
                                     sub(".*-(Q[1-4])", "\\1", quarter),
                                     paste0("Q", ceiling(as.numeric(month)/3))))
      
      # Make sure at least two quarters to compare
      if(length(unique(filtered$quarter_only)) < 2) {
        return(NULL) # Not enough quarters to compare
      }
      
      # For quarter comparisons across all years
      result <- filtered %>% 
        group_by(quarter_only) %>%
        summarize(
          mean_awarded = mean(awarded_amt, na.rm = TRUE),
          median_awarded = median(awarded_amt, na.rm = TRUE),
          n_awards = n(),
          total_awarded = sum(awarded_amt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(quarter_only = factor(quarter_only, levels = c("Q1", "Q2", "Q3", "Q4")))
      
      print("Created quarter stats data")
      print(paste("Number of quarters:", nrow(result)))
      print(paste("Quarters:", paste(result$quarter_only, collapse=", ")))
      return(result)
    }
  })

  # Render the statistical plot
  output$stats_plot <- renderPlot({
    req(filtered_data())
    
    # Prepare data for distribution comparison
    plot_data <- filtered_data() %>%
      mutate(award_year = as.factor(award_year))
    
    # Using tryCatch to capture any errors in plot creation
    tryCatch({
      if(input$stat_time_period == "award_year") {
        # For year comparisons, use ggbetweenstats
        p <- ggbetweenstats(
          data = plot_data,
          x = award_year,
          y = awarded_amt,
          title = "Comparison of Award Amounts by Year",
          xlab = "Year",
          ylab = "Award Amount (S$ Billion)",
          type = input$test_type,
          bf.message = FALSE,
          mean.ci = TRUE, 
          pairwise.comparisons = TRUE, 
          pairwise.display = "s",
          p.adjust.method = "fdr",
          messages = FALSE
        ) +
          scale_y_continuous(labels = scales::dollar_format(prefix = "S$", 
                                                            big.mark = ",")) +
          theme(plot.margin = margin(t = 20, r = 25, b = 20, l = 25, unit = "pt"))
        
        return(p)
        
      } else if(input$stat_time_period == "quarter") {
        # For quarter comparisons
        plot_data <- plot_data %>%
          mutate(quarter_only = sub(".*-(Q[1-4])", "\\1", quarter))
        
        p <- ggbetweenstats(
          data = plot_data,
          x = quarter_only,
          y = awarded_amt,
          title = "Distribution of Award Amounts by Quarter",
          xlab = "Quarter",
          ylab = "Award Amount (S$ Billion)",
          type = input$test_type,
          bf.message = FALSE,
          mean.ci = TRUE, 
          pairwise.comparisons = TRUE, 
          pairwise.display = "s",
          p.adjust.method = "fdr",
          messages = FALSE
        ) +
          scale_y_continuous(labels = scales::dollar_format(prefix = "S$", 
                                                            big.mark = ",")) +
          theme(plot.margin = margin(t = 20, r = 25, b = 20, l = 25, unit = "pt"))
        
        return(p)
      }
    }, error = function(e) {
      # Return error message as a plot
      plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
      text(0, 0, paste("Error creating plot:", e$message), col = "red")
    })
  })
 
  
  # Display detailed statistical results
  output$stats_results <- renderPrint({
    req(filtered_data())

    # Validate data
    if(is.null(data) || nrow(data) < 2) {
      cat("Insufficient data for statistical analysis. Please select a wider date range.")
      return(NULL)
    } 
    
  tryCatch({   
    cat("Statistical Analysis Results\n")
    cat("===========================\n\n")
    
    # Prepare data for statistical analysis
    if(input$stat_time_period == "award_year") {
      # Year comparison code (as in previous example)
      plot_data <- filtered_data() %>%
        mutate(award_year = as.factor(award_year))
      
      if(input$test_type == "parametric") {
        # One-way ANOVA for individual award amounts
        result <- aov(awarded_amt ~ award_year, data = plot_data)
        cat("One-way ANOVA results (Individual Award Amounts):\n")
        print(summary(result))
        
        # Post-hoc tests
        if(length(unique(plot_data$award_year)) > 2) {
          cat("\nTukey HSD Post-hoc Test:\n")
          print(TukeyHSD(result))
        }
      } else {
        # Kruskal-Wallis for non-parametric
        cat("Kruskal-Wallis Rank Sum Test (Individual Award Amounts):\n")
        print(kruskal.test(awarded_amt ~ award_year, data = plot_data))
        
        # Pairwise Wilcoxon tests
        if(length(unique(plot_data$award_year)) > 2) {
          cat("\nPairwise Wilcoxon Rank Sum Tests:\n")
          print(pairwise.wilcox.test(plot_data$awarded_amt, plot_data$award_year, 
                                     p.adjust.method = "bonferroni"))
        }
      }
    } else if(input$stat_time_period == "quarter") {
      # Quarter comparison modifications
      plot_data <- filtered_data() %>%
        mutate(
          # Ensure quarter is extracted consistently
          quarter_only = ifelse(grepl("-Q", quarter),
                                sub(".*-(Q[1-4])", "\\1", quarter),
                                paste0("Q", ceiling(as.numeric(month)/3))),
          quarter_only = factor(quarter_only, levels = c("Q1", "Q2", "Q3", "Q4"))
        )
      
      if(input$test_type == "parametric") {
        # One-way ANOVA for quarters
        result <- aov(awarded_amt ~ quarter_only, data = plot_data)
        cat("One-way ANOVA results (Individual Award Amounts by Quarter):\n")
        print(summary(result))
        
        # Post-hoc tests
        if(length(unique(plot_data$quarter_only)) > 2) {
          cat("\nTukey HSD Post-hoc Test:\n")
          print(TukeyHSD(result))
        }
      } else {
        # Kruskal-Wallis for non-parametric
        cat("Kruskal-Wallis Rank Sum Test (Individual Award Amounts by Quarter):\n")
        print(kruskal.test(awarded_amt ~ quarter_only, data = plot_data))
        
        # Pairwise Wilcoxon tests
        if(length(unique(plot_data$quarter_only)) > 2) {
          cat("\nPairwise Wilcoxon Rank Sum Tests:\n")
          print(pairwise.wilcox.test(plot_data$awarded_amt, plot_data$quarter_only, 
                                     p.adjust.method = "bonferroni"))
        }
      }
    }
  })
  })
  })
}
#---------------------------------
# Define the main UI and server
#---------------------------------

# UI
ui <- time_series_ui("time_series_module")

server <- function(input, output, session) {
  time_series_server("time_series_module", ts_overview_data)
}

shinyApp(ui, server)
