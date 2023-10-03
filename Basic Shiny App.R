library(shiny)

# Coefficients
coefficients <- c(
  KNEE = 0.1089,
  DENSITY = -354.8705,
  CHEST = 0.0936,
  ADIPOSITY = -0.2026,
  ABDOMEN = 0.0528,
  NECK = 0.1080,
  HIP = 0.0677,
  WEIGHT = -0.0133,
  BICEPS = -0.0787,
  FOREARM = -0.0313,
  Intercept = 375.1030
)

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Linear Regression Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("KNEE", "KNEE:", value = "0"),
      textInput("DENSITY", "DENSITY:", value = "0"),
      textInput("CHEST", "CHEST:", value = "0"),
      textInput("ADIPOSITY", "ADIPOSITY:", value = "0"),
      textInput("ABDOMEN", "ABDOMEN:", value = "0"),
      textInput("NECK", "NECK:", value = "0"),
      textInput("HIP", "HIP:", value = "0"),
      textInput("WEIGHT", "WEIGHT:", value = "0"),
      textInput("BICEPS", "BICEPS:", value = "0"),
      textInput("FOREARM", "FOREARM:", value = "0"),
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
