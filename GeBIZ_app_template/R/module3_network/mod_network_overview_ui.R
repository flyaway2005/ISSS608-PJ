# network_module/ui.R

network_analysis_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Control Panel", style = "margin-top: 5px; margin-bottom: 10px;"),
        
        div(class = "panel-group", id = ns("accordion"), role = "tablist",
            # Date Range Filter
            div(class = "panel panel-default",
                div(class = "panel-heading", role = "tab", id = ns("headingOne"),
                    h5(class = "panel-title",
                       tags$a("Date Range Filter",
                              style = "text-decoration: none; display: block;",
                              `data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = paste0("#", ns("collapseOne"))
                       )
                    )
                ),
                div(id = ns("collapseOne"), class = "panel-collapse collapse in", role = "tabpanel",
                    div(class = "panel-body", style = "padding: 5px;",
                        fluidRow(style = "margin: 0px;",
                                 column(2, h6("Start", style = "margin-top: 7px; text-align: right; font-size: 12px;")),
                                 column(5, style = "padding: 0px 5px;",
                                        selectInput(ns("start_year"), "Year", 
                                                    choices = c("2019", "2020", "2021", "2022", "2023", "2024"),
                                                    selected = "2019",
                                                    width = "100%")
                                 ),
                                 column(5, style = "padding: 0px 5px;",
                                        selectInput(ns("start_month"), "Month",
                                                    choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                                                "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                                                    selected = 1,
                                                    width = "100%")
                                 )
                        ),
                        fluidRow(style = "margin: 0px;",
                                 column(2, h6("End", style = "margin-top: 7px; text-align: right; font-size: 12px;")),
                                 column(5, style = "padding: 0px 5px;",
                                        selectInput(ns("end_year"), "",
                                                    choices = c("2019", "2020", "2021", "2022", "2023", "2024"),
                                                    selected = "2024",
                                                    width = "100%")
                                 ),
                                 column(5, style = "padding: 0px 5px;",
                                        selectInput(ns("end_month"), "",
                                                    choices = c("01" = 1, "02" = 2, "03" = 3, "04" = 4, "05" = 5, "06" = 6,
                                                                "07" = 7, "08" = 8, "09" = 9, "10" = 10, "11" = 11, "12" = 12),
                                                    selected = 12,
                                                    width = "100%")
                                 )
                        )
                    )
                )
            ),
            
            # Filters
            div(class = "panel panel-default",
                div(class = "panel-heading", role = "tab", id = ns("headingTwo"),
                    h5(class = "panel-title",
                       tags$a("Filter Data Range",
                              style = "text-decoration: none; display: block;",
                              `data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = paste0("#", ns("collapseTwo"))
                       )
                    )
                ),
                div(id = ns("collapseTwo"), class = "panel-collapse collapse", role = "tabpanel",
                    div(class = "panel-body", style = "padding: 5px;",
                        selectizeInput(ns("agency_type_filter"), "Agency Types", choices = NULL, multiple = TRUE,
                                       options = list(placeholder = 'Select agency types', plugins = list('remove_button'))),
                        selectizeInput(ns("agency_filter"), "Agencies", choices = NULL, multiple = TRUE,
                                       options = list(placeholder = 'Select agencies', plugins = list('remove_button'))),
                        selectizeInput(ns("supplier_filter"), "Suppliers", choices = NULL, multiple = TRUE,
                                       options = list(placeholder = 'Select suppliers', plugins = list('remove_button'))),
                        selectizeInput(ns("tender_cat_filter"), "Tender Categories", choices = NULL, multiple = TRUE,
                                       options = list(placeholder = 'Select tender categories', plugins = list('remove_button'))),
                        sliderInput(ns("award_amount_range"), "Award Amount Range", min = 0, max = 1, value = c(0, 1), step = 1),
                        fluidRow(style = "margin: -15px 0px 10px 0px;",
                                 column(6, numericInput(ns("min_award_manual"), NULL, value = 100000, min = 0, max = 1500000000)),
                                 column(6, numericInput(ns("max_award_manual"), NULL, value = 1000000, min = 0, max = 1500000000))),
                        fluidRow(style = "margin: -15px 0px 0px 0px;",
                                 column(6, tags$small("Min", style = "font-size: 10px;")),
                                 column(6, tags$small("Max", style = "font-size: 10px;"))
                        )
                    )
                )
            ),
            
            # Network Options
            div(class = "panel panel-default",
                div(class = "panel-heading", role = "tab", id = ns("headingThree"),
                    h5(class = "panel-title",
                       tags$a("Network Options",
                              style = "text-decoration: none; display: block;",
                              `data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = paste0("#", ns("collapseThree"))
                       )
                    )
                ),
                div(id = ns("collapseThree"), class = "panel-collapse collapse", role = "tabpanel",
                    div(class = "panel-body", style = "padding: 5px;",
                        sliderInput(ns("max_edges"), "Maximum Edges to Display", min = 10, max = 10000, value = 500, step = 10),
                        radioButtons(ns("edge_metric"), "Edge Thickness Based On",
                                     choices = c("Award Amount" = "total_award_amount", "Contract Count" = "total_contracts"),
                                     inline = TRUE),
                        selectInput(ns("layout_type"), "Network Layout",
                                    choices = c("Force-Directed" = "force", "Repulsion" = "repulsion", "Barnes-Hut" = "barnesHut"),
                                    selected = "force"),
                        fluidRow(
                          column(6, checkboxInput(ns("size_by_degree"), "Size by Degree", TRUE)),
                          column(6, checkboxInput(ns("performance_mode"), "Performance Mode", TRUE))
                        )
                    )
                )
            )
        ),
        
        div(style = "position: sticky; bottom: 10px; margin-top: 10px;",
            actionButton(ns("update_network"), "Update Plot", class = "btn-primary", width = "100%")
        ),
        
        width = 4
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          id = ns("network_tabs"),
          tabPanel("Network Overview",
                   fluidRow(column(12, box(title = "Network Visualisation", width = NULL, solidHeader = TRUE, status = "primary", visNetworkOutput(ns("network_plot"), height = "400px")))),
                   fluidRow(column(12, box(title = "Network Summary", width = NULL, solidHeader = TRUE, status = "info", verbatimTextOutput(ns("network_summary")))))
          ),
          tabPanel("Network Metrics",
                   fluidRow(column(12, tabBox(title = "Network Details", width = NULL,
                                              tabPanel("Agencies", DTOutput(ns("agency_table"))),
                                              tabPanel("Suppliers", DTOutput(ns("supplier_table"))),
                                              tabPanel("Top Contracts", DTOutput(ns("contract_table")))
                   )))
          ),
          tabPanel("Ego Network",
                   fluidRow(column(12, box(title = "Ego Network", width = NULL, solidHeader = TRUE, visNetworkOutput(ns("ego_network_plot"), height = "390px")))),
                   fluidRow(column(12, box(title = "Ego Network Metrics", width = NULL, solidHeader = TRUE, status = "info", verbatimTextOutput(ns("ego_metrics"))))),
                   uiOutput(ns("no_node_selected_message"))
          ),
          tabPanel("Ego Metrics",
                   fluidRow(column(12, tabBox(title = "Ego Network Details", width = NULL,
                                              tabPanel("Ego Agencies", DTOutput(ns("ego_agency_table"))),
                                              tabPanel("Ego Suppliers", DTOutput(ns("ego_supplier_table"))),
                                              tabPanel("Connections", DTOutput(ns("ego_connection_table")))
                   ))),
                   uiOutput(ns("no_node_selected_metrics_message"))
          )
        )
      )
    )
  )
}
