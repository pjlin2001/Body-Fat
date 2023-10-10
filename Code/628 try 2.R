library(shiny)
library(DT)
library(ggplot2)

data <- read.csv("BodyFat.csv")
data$IDNO <- NULL
z_scores <- scale(data)
row_z_scores <- rowSums(z_scores^2)
outliers <- row_z_scores > 60

# Add a new column to indicate whether each row is an outlier
data$Status <- ifelse(outliers, "Outlier", "Normal")

# Rearrange columns to put Status as the second column
data <- data[, c(1, ncol(data), 2:(ncol(data)-1))]

ui <- fluidPage(
  titlePanel("Outliers Detection using Z-Scores"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis:", choices = setdiff(colnames(data), c("BODYFAT", "Status")), selected = "AGE")
    ),
    mainPanel(
      plotOutput("scatter", brush = brushOpts(id = "brush")),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  brushed_data <- reactive({
    brushedPoints(data, input$brush, xvar = input$xvar, yvar = "BODYFAT")
  })
  
  output$scatter <- renderPlot({
    ggplot(data, aes_string(x = input$xvar, y = "BODYFAT")) +
      geom_point(aes(color = Status), size = 3) +
      scale_color_manual(values = c("Outlier" = "red", "Normal" = "blue")) +
      labs(title = "Scatter Plot", y = "BODYFAT", color = "Status") +
      theme_minimal()
  })
  
  output$table <- renderDT({
    if (nrow(brushed_data()) == 0) return(NULL)
    brushed_data()[order(brushed_data()$BODYFAT), ]
  }, options = list(searching = FALSE, pageLength = 10))
}

shinyApp(ui = ui, server = server)
