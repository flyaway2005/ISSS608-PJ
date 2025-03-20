library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Clustering Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Unsupervised", tabName = "unsupervised", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "unsupervised",
              fluidPage(
                titlePanel("Unsupervised Clustering Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("k", "Number of Clusters:", min = 2, max = 10, value = 6, step = 1)
                  ),
                  mainPanel(
                    plotlyOutput("topicPlot")
                  )
                )
              )
      )
    )
  )
)
