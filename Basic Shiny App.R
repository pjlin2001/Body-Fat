# Load necessary libraries
library(shiny)
library(ggplot2)

# Load the data from the CSV file
data <- read.csv("C:/Users/phili/Downloads/CleanedData.csv")  # Adjust the file path

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Linear Regression App"),
  sidebarLayout(
    sidebarPanel(
      textInput("density", "DENSITY:", value = "0.5"),
      textInput("height", "HEIGHT:", value = "65"),
      textInput("adiposity", "ADIPOSITY:", value = "25"),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      h4("Predicted Value:"),
      verbatimTextOutput("predicted_value"),
      plotOutput("regression_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Linear regression coefficients
  coefficients <- c(-4.09853504e+02, -1.12866868e-02, 1.03324522e-02)
  
  # Perform linear regression based on user inputs
  regression_result <- eventReactive(input$calculate, {
    density <- as.numeric(input$density)
    height <- as.numeric(input$height)
    adiposity <- as.numeric(input$adiposity)
    
    # Check if input values are valid numbers
    if (is.numeric(density) && is.numeric(height) && is.numeric(adiposity)) {
      # Calculate the predicted value
      predicted_value <- coefficients[1] * density +
        coefficients[2] * height + coefficients[3] * adiposity + 452.04779969074616
      return(list(predicted_value = predicted_value, density = density, height = height, adiposity = adiposity))
    } else {
      return("Invalid input. Please enter valid numbers.")
    }
  })
  
  # Display the predicted value or error message
  output$predicted_value <- renderText({
    predicted_value <- regression_result()
    if (is.numeric(predicted_value$predicted_value)) {
      paste("Predicted Value:", predicted_value$predicted_value)
    } else {
      predicted_value
    }
  })
  
  # Create a scatter plot with regression line and input/predicted points
  output$regression_plot <- renderPlot({
    result <- regression_result()
    if (is.numeric(result$predicted_value)) {
      data$PREDICTED <- coefficients[1] * data$DENSITY +
        coefficients[2] * data$HEIGHT + coefficients[3] * data$ADIPOSITY + 452.04779969074616
      
      p <- ggplot(data, aes(x = DENSITY, y = PREDICTED)) +
        geom_point(color = "blue", size = 3) +  # Data points in blue
        geom_abline(intercept = 452.04779969074616, slope = coefficients[1], color = "black", linetype = "dashed") +  # Line in black
        geom_point(data = data.frame(DENSITY = result$density, PREDICTED = result$predicted_value), aes(x = DENSITY, y = PREDICTED), color = "red", size = 3) +  # Input/Predicted points in red
        labs(x = "DENSITY", y = "Predicted Value", title = "Linear Regression Plot")
      print(p)
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)