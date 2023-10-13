library(shiny)
library(ggplot2)
library(DT)

# #### Load data for Bodyfat Prediction App ####
bodyfat_data <- read.csv("cleaned_data.csv")
# #### Load data for Outliers Detection using Z-Scores ####
outliers_data <- read.csv("BodyFat.csv")
outliers_data<- outliers_data[, c( 'BODYFAT','ABDOMEN', 'CHEST', 'WRIST')]
outliers_data$outliers=0
outliers_data$outliers[39]=1
outliers_data$outliers[41]=1
outliers_data$outliers[216]=1
outliers_data$outliers
outliers_data$Status <- ifelse(outliers_data$outliers, "Outlier", "Normal")

#### UI Definition ####
ui <- fluidPage(
  titlePanel("Combined Shiny App"),
  tabsetPanel(
    tabPanel("Bodyfat Prediction App", 
             fluidPage(
               tags$head(
                 tags$script(HTML('
      $(document).ready(function() {
        // Initialize image visibility
        $("#abdomen_image").hide();
        $("#chest_image").hide();
        $("#wrist_image").hide();

        // Show/hide images based on button clicks
        $("#show_abdomen").click(function() {
          $("#abdomen_image").show();
          $("#chest_image").hide();
          $("#wrist_image").hide();
        });

        $("#show_chest").click(function() {
          $("#abdomen_image").hide();
          $("#chest_image").show();
          $("#wrist_image").hide();
        });

        $("#show_wrist").click(function() {
          $("#abdomen_image").hide();
          $("#chest_image").hide();
          $("#wrist_image").show();
        });

        $("#calculate").click(function() {
          $("#abdomen_image").hide();
          $("#chest_image").hide();
          $("#wrist_image").hide();
        });
      });
    '))
               ),
               titlePanel("Bodyfat Prediction App"),
               fluidRow(
                 column(3,
                        numericInput("abdomen", "ABDOMEN (inches):", value = 35.43),
                        numericInput("chest", "CHEST (inches):", value = 42),
                        numericInput("wrist", "WRIST (inches):", value = 8.5),
                        numericInput("adiposity", "ADIPOSITY:", value = 30),
                        actionButton("calculate", "Calculate"),
                        actionButton("show_abdomen", "Show Abdomen Image"),
                        actionButton("show_chest", "Show Chest Image"),
                        actionButton("show_wrist", "Show Wrist Image")
                 ),
                 column(9,
                        mainPanel(
                          h4("Predicted Bodyfat:"),
                          verbatimTextOutput("predicted_bodyfat"),
                          h4("Predicted Density:"),
                          verbatimTextOutput("predicted_density"),
                          plotOutput("regression_plot", brush = brushOpts(id = "regression_plot_brush")),
                          conditionalPanel(
                            condition = "input.show_abdomen",
                            imageOutput("abdomen_image")
                          ),
                          conditionalPanel(
                            condition = "input.show_chest",
                            imageOutput("chest_image")
                          ),
                          conditionalPanel(
                            condition = "input.show_wrist",
                            imageOutput("wrist_image")
                          )
                        )
                 )
               ),
           fluidRow(
                 column(12,
                        dataTableOutput("user_info_table")
                 )
               )
             )
    ),
    tabPanel("Outliers Detection using Z-Scores",
             fluidPage(
               titlePanel("Outliers Detection using Z-Scores"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("xvar", "X-axis:", choices = setdiff(colnames(outliers_data), c("BODYFAT", "Status")), selected = "AGE")
                 ),
                 mainPanel(
                   plotOutput("scatter", brush = brushOpts(id = "scatter_brush")),
                   DTOutput("table")
                 )
               )
             )
    )
  )
)
#### Server Logic ####
server <- function(input, output, session) {
  # Linear regression coefficients for the model to predict density
  coefficients_density <- c(-0.00527214, 0.00045475, 0.01166926)
  intercept_density <- 1.1193190404360203
  
  # Linear regression coefficients for the model to predict bodyfat
  coefficients_bodyfat <- c(2.08827449e-01, -3.23329199e+02, 1.80207842e-01)
  intercept_bodyfat <- 348.0722797501468
  
  # Create a reactiveValues object to store brushed points
  brushed_points_data <- reactiveValues(data = NULL)
  
  # Error handling for invalid input
  observe({
    if (input$calculate > 0) {
      validate(
        need(is.numeric(input$abdomen) && is.numeric(input$chest) && is.numeric(input$wrist) && is.numeric(input$adiposity), 
             "Please enter valid numbers.")
      )
    }
  })
  
  # Perform density prediction based on user inputs
  density_prediction_result <- eventReactive(input$calculate, {
    abdomen <- as.numeric(input$abdomen)
    chest <- as.numeric(input$chest)
    wrist <- as.numeric(input$wrist)
    adiposity <- as.numeric(input$adiposity)
    
    if (is.numeric(abdomen) && is.numeric(chest) && is.numeric(wrist) && is.numeric(adiposity)) {
      # Calculate the predicted density
      predicted_density <- intercept_density +
        coefficients_density[1] * abdomen +
        coefficients_density[2] * chest +
        coefficients_density[3] * wrist
      
      return(predicted_density)
    } else {
      return("Invalid input. Please enter valid numbers.")
    }
  })
  
  # Perform bodyfat prediction based on the predicted density
  bodyfat_prediction_result <- eventReactive(input$calculate, {
    predicted_density <- density_prediction_result()
    
    if (is.numeric(predicted_density)) {
      abdomen <- as.numeric(input$abdomen) # Retrieve abdomen value from input
      
      # Calculate the predicted bodyfat using the specified coefficients
      predicted_bodyfat <- coefficients_bodyfat[1] * abdomen +
        coefficients_bodyfat[2] * predicted_density +
        coefficients_bodyfat[3] * input$adiposity +
        intercept_bodyfat
      
      return(list(predicted_density = predicted_density, predicted_bodyfat = predicted_bodyfat))
    } else {
      return("Invalid input. Please enter valid numbers.")
    }
  })
  
  # Display the predicted bodyfat and predicted density or error messages
  output$predicted_bodyfat <- renderText({
    result <- bodyfat_prediction_result()
    if (is.numeric(result$predicted_bodyfat)) {
      paste("Predicted Bodyfat:", result$predicted_bodyfat)
    } else {
      result$predicted_bodyfat
    }
  })
  
  output$predicted_density <- renderText({
    result <- bodyfat_prediction_result()
    if (is.numeric(result$predicted_density)) {
      paste("Predicted Density:", result$predicted_density)
    } else {
      result$predicted_density
    }
  })
 predicted_bodyfat_data <- coefficients_bodyfat[1] * bodyfat_data$ABDOMEN +
    coefficients_bodyfat[2] * bodyfat_data$DENSITY +
    coefficients_bodyfat[3] * bodyfat_data$ADIPOSITY +
    intercept_bodyfat
  
  # Create data frame for data points
  data_data <- data.frame(
    DENSITY = bodyfat_data$DENSITY,
    PREDICTED_BODYFAT = predicted_bodyfat_data,
    SOURCE = "Data"
  )
  # Create a scatter plot with predicted bodyfat for user input and all data points
  output$regression_plot <- renderPlot({
    abdomen <- as.numeric(input$abdomen)
    adiposity <- as.numeric(input$adiposity)
    
    if (is.numeric(abdomen) && is.numeric(adiposity)) {
      # Calculate predicted bodyfat for all data points
      predicted_bodyfat_data <- coefficients_bodyfat[1] * bodyfat_data$ABDOMEN +
        coefficients_bodyfat[2] * bodyfat_data$DENSITY +
        coefficients_bodyfat[3] * bodyfat_data$ADIPOSITY +
        intercept_bodyfat
      
      # Create data frame for data points
      data_data <- data.frame(
        DENSITY = bodyfat_data$DENSITY,
        PREDICTED_BODYFAT = predicted_bodyfat_data,
        SOURCE = "Data"
      )
      
      # Combine user input and data points
      user_input <- data.frame(
        DENSITY = density_prediction_result(),
        PREDICTED_BODYFAT = bodyfat_prediction_result()$predicted_bodyfat,
        SOURCE = "User Input"
      )
      
      p <- ggplot() +
        geom_point(data = data_data, aes(x = DENSITY, y = PREDICTED_BODYFAT, color = SOURCE), size = 3) +
        geom_point(data = user_input, aes(x = DENSITY, y = PREDICTED_BODYFAT, color = SOURCE), size = 3) +
        labs(x = "Density", y = "Predicted Bodyfat", title = "Bodyfat Prediction") +
        scale_color_manual(values = c("User Input" = "red", "Data" = "blue"))
      p
    }
  })
  # Create a reactive dataset that depends on brushed points
  # Define reactive_data
  reactive_data <- reactive({
    brushed_data <- brushedPoints(data_data, input$regression_plot_brush, xvar = "DENSITY", yvar = "PREDICTED_BODYFAT")
    
    # Check if brushed_data has any rows. If not, return the entire data_data.
    if (nrow(brushed_data) == 0) {
      return(data_data)
    } else {
      return(brushed_data)
    }
  })
  
  # Render the user information table based on the reactive_data
  output$user_info_table <- renderDT({
    datatable(reactive_data())
  }, options = list(searching = FALSE, pageLength = 10))
  # Dynamically set image sources based on input buttons
  output$abdomen_image <- renderImage({
    if (input$show_abdomen) {
      list(src = "Abs.png", width = "500px", height = "auto")
    } else {
      list(src = NULL, width = "500px", height = "auto")
    }
  }, deleteFile = FALSE)
  
  output$chest_image <- renderImage({
    if (input$show_chest) {
      list(src = "Chest.png", width = "500px", height = "auto")
    } else {
      list(src = NULL, width = "500px", height = "auto")
    }
  }, deleteFile = FALSE)
  
  output$wrist_image <- renderImage({
    if (input$show_wrist) {
      list(src = "Wrist.png", width = "500px", height = "auto")
    } else {
      list(src = NULL, width = "500px", height = "auto")
    }
  }, deleteFile = FALSE)
# Run the Shiny app
  brushed_data2 <- reactive({
    brushedPoints(outliers_data, input$scatter_brush, xvar = input$xvar, yvar = "BODYFAT")
  })
  
  output$scatter <- renderPlot({
    ggplot(outliers_data, aes_string(x = input$xvar, y = "BODYFAT")) +
      geom_point(aes(color = Status), size = 3) +
      scale_color_manual(values = c("Outlier" = "red", "Normal" = "blue")) +
      labs(title = "Scatter Plot", y = "BODYFAT", color = "Status") +
      theme_minimal()
  })
  
  output$table <- renderDT({
    if (nrow(brushed_data2()) == 0) return(NULL)
    brushed_data2()[order(brushed_data2()$BODYFAT), ]
  }, options = list(searching = FALSE, pageLength = 10))
}
shinyApp(ui = ui, server = server)
