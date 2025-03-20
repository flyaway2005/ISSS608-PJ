server <- function(input, output, session) {
  source("unsupervised.R")  # 確保載入 unsupervised.R
  
  output$topicPlot <- renderPlotly({
    req(input$k)  # 確保有選擇 K 值
    topic_cluster_plot(input$k)  # 傳遞 K 值給 K-means 分群
  })
}

