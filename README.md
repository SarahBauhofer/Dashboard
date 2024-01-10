# Install and load necessary libraries
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(ggplot2)

# Read your dataset
StudentsPerformance_3 <- read.csv("https://raw.githubusercontent.com/SarahBauhofer/Dashboard/main/StudentsPerformance-3.csv")

# Print the structure of the dataset
print(str(StudentsPerformance_3))

# Remove "lunch" and "race/ethnicity" columns
StudentsPerformance_3 <- StudentsPerformance_3[, !(names(StudentsPerformance_3) %in% c("lunch", "race.ethnicity"))]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Students Performance Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Place your UI components here
    dataTableOutput("table"),
    plotOutput("scatterplot"),
    plotOutput("average_scores")  # Add plotOutput for the bar chart
  )
)

# Define server logic
server <- function(input, output) {
  # Display the modified dataset in a table
  output$table <- renderDataTable({
    StudentsPerformance_3
  })
  
  # Create a scatterplot using the "gender" and "parental.level.of.education" columns
  output$scatterplot <- renderPlot({
    ggplot(StudentsPerformance_3, aes(x = gender, y = `parental.level.of.education`)) +
      geom_point() +
      ggtitle("Scatterplot") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(y = gsub("\\.", " ", "parental.level.of.education")) +
      ggtitle("Scatterplot - Parental Level of Education")
  })
  
  # Calculate average scores
  avg_scores <- colMeans(StudentsPerformance_3[, c("math.score", "reading.score", "writing.score")])
  
  # Display average scores in a bar chart
  output$average_scores <- renderPlot({
    bar_data <- data.frame(subject = c("Math", "Reading", "Writing"), average_score = avg_scores)
    ggplot(bar_data, aes(x = subject, y = average_score, fill = subject)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Scores in Each Subject", x = "Subject", y = "Average Score") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
