library(shiny)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    # 產生直方圖
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "skyblue", border = "white",
         xlab = "等待時間 (分鐘)",
         main = "Old Faithful 間歇泉噴發等待時間")
  })
} 