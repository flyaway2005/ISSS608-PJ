# app.R

library(shiny)
library(shinydashboard)



source("ui.R")
source("server.R")

# 啟動 Shiny 應用
shinyApp(ui = ui, server = server)


