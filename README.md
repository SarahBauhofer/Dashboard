# Install and load necessary libraries
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(ggplot2)

# Read my dataset
my_data <- read.csv("https://raw.githubusercontent.com/SarahBauhofer/Dashboard/main/StudentsPerformance-3.csv")

# Remove "lunch" and "race/ethnicity" columns
my_data <- my_data[, !(names(my_data) %in% c("lunch", "race.ethnicity"))]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Place your UI components here
    dataTableOutput("table"),
    plotOutput("scatterplot")
  )
)

# Define server logic
server <- function(input, output) {
  # Display the dataset in a table
  output$table <- renderDataTable({
    my_data
  })
  
  # Create a scatterplot using the first two numeric columns
  output$scatterplot <- renderPlot({
    ggplot(my_data, aes(x = my_data[[1]], y = my_data[[2]])) +
      geom_point() +
      labs(title = "Scatterplot")
  })
}

# Run the application
shinyApp(ui, server)
