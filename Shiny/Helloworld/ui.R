library(shiny)

ui <- fluidPage(
  titlePanel("我的 Shiny 應用"),
  
  sidebarLayout(
    sidebarPanel(
      h3("控制面板"),
      sliderInput("bins",
                  "直方圖的分組數量:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    mainPanel(
      h3("結果顯示"),
      plotOutput("distPlot")
    )
  )
) 