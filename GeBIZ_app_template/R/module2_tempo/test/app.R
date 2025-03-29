library(shiny)

# Create the simplest possible app to test dropdowns
ui <- fluidPage(
  titlePanel("Minimal Dropdown Test"),
  
  # Static dropdown - should always work
  selectInput("test_select", "Regular Select:", 
              choices = c("All", "Test1", "Test2"),
              selected = "All"),
  
  # Selectize dropdown - static
  selectizeInput("test_selectize", "Selectize Static:", 
                 choices = c("All", "Test1", "Test2"),
                 selected = "All"),
  
  # Dynamic dropdown to be updated
  selectizeInput("dynamic", "Dynamic Options:",
                 choices = c("All"),
                 selected = "All"),
  
  # Buttons for testing
  actionButton("update_btn", "Update Dynamic Dropdown"),
  
  # Output for debugging
  verbatimTextOutput("debug_output")
)

server <- function(input, output, session) {
  # Print session info
  cat("Session information:\n")
  print(sessionInfo())
  
  # Update dynamic dropdown when button is clicked
  observeEvent(input$update_btn, {
    cat("Update button clicked\n")
    
    # Create test data
    test_values <- c("All", paste0("Option", 1:5))
    cat("Test values:", paste(test_values, collapse=", "), "\n")
    
    # Try to update the dropdown
    tryCatch({
      updateSelectizeInput(session, "dynamic", 
                           choices = test_values,
                           selected = "All")
      cat("Update completed\n")
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
    })
  })
  
  # Debug output
  output$debug_output <- renderPrint({
    cat("Debug Information:\n")
    cat("- Static select value:", input$test_select, "\n")
    cat("- Static selectize value:", input$test_selectize, "\n")
    cat("- Dynamic selectize value:", input$dynamic, "\n")
    
    if (input$update_btn > 0) {
      cat("\nUpdate button has been clicked", input$update_btn, "times\n")
    }
  })
}

# Run the application
shinyApp(ui, server)