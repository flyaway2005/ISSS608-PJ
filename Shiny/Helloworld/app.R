# 載入必要的套件
library(shiny)
source("ui.R")
source("server.R")

# 執行應用程式
shinyApp(ui = ui, server = server) 