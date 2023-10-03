library(shiny)

# Coefficients
coefficients <- c(
  ADIPOSITY = 0.0975,
  KNEE = -0.0750,
  CHEST = -0.0099,
  ABDOMEN = 0.0381,
  DENSITY = -396.4177,
  BICEPS = -0.0007,
  NECK = 0.0498,
  WEIGHT = -0.0081,
  HIP = -0.0128,
  HEIGHT = 0.0084,
  Intercept = 435.3523
)

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Linear Regression Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("ADIPOSITY", "ADIPOSITY:", value = "0"),
      textInput("KNEE", "KNEE:", value = "0"),
      textInput("CHEST", "CHEST:", value = "0"),
      textInput("ABDOMEN", "ABDOMEN:", value = "0"),
      textInput("DENSITY", "DENSITY:", value = "0"),
      textInput("BICEPS", "BICEPS:", value = "0"),
      textInput("NECK", "NECK:", value = "0"),
      textInput("WEIGHT", "WEIGHT:", value = "0"),
      textInput("HIP", "HIP:", value = "0"),
      textInput("HEIGHT", "HEIGHT:", value = "0"),
      actionButton("predictButton", "Predict"),
      helpText("Note: Enter values for predictor variables.")
    ),
    mainPanel(
      h3("Body Fat Prediction:"),
      textOutput("prediction")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Perform linear regression when the "Predict" button is clicked
  observeEvent(input$predictButton, {
    # Convert text inputs to numeric values
    predictor_values <- sapply(names(coefficients)[-11], function(var_name) {
      as.numeric(input[[var_name]])
    })
    
    # Check if the conversion was successful
    if (all(is.numeric(predictor_values))) {
      # Calculate the prediction using coefficients
      prediction <- coefficients["Intercept"] +
        sum(predictor_values * coefficients[names(predictor_values)])
      
      # Display the prediction
      output$prediction <- renderText({
        paste("Predicted Percentage:", round(prediction, 2), "%")
      })
    } else {
      # Handle invalid input
      output$prediction <- renderText({
        "Invalid input. Please enter valid numeric values."
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
