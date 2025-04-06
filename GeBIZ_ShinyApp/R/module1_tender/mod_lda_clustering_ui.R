mod_lda_clustering_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("LDA Clustering"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput(ns("num_topics"), "Number of Topics:", min = 2, max = 10, value = 10, step = 1),
        numericInput(ns("num_clusters"), "Number of Clusters:", min = 2, max = 10, value = 5, step = 1),
        actionButton(ns("run_unsupervised"), "Run Clustering", class = "btn-primary"),
        selectInput(ns("select_cluster"), "Select Cluster:", choices = NULL)
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("All Clusters", plotlyOutput(ns("cluster_plot"), height = "500px")),
          tabPanel("Single Cluster View", plotlyOutput(ns("single_cluster_plot"), height = "400px"))
        )
      )
    )
  )
}