# Install and load necessary libraries
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")
if (!require(reshape2)) install.packages("reshape2")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(corrplot)
library(reshape2)

# Read your dataset
StudentsPerformance_3 <- read.csv("https://raw.githubusercontent.com/SarahBauhofer/Dashboard/main/StudentsPerformance-3.csv")

# Remove "lunch" and "race/ethnicity" columns
StudentsPerformance_3 <- StudentsPerformance_3[, !(names(StudentsPerformance_3) %in% c("lunch", "race.ethnicity"))]

# Remove "." between words in column names
colnames(StudentsPerformance_3) <- gsub("\\.", "", colnames(StudentsPerformance_3))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Students Performance Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Research Question", tabName = "researchQuestionTab"),
      menuItem("Table", tabName = "tableTab"),
      menuItem("Boxplot", tabName = "boxplotTab"),
      menuItem("Average Scores", tabName = "averageScoresTab"),
      menuItem("Test Preparation", tabName = "testPreparationTab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "researchQuestionTab",
              fluidPage(
                box(title = "Research Question",
                    "Do various factors such as parental education, test preparation courses, and gender contribute to better student performance in mathematics, reading, and writing?"
                )
              )
      ),
      tabItem(tabName = "tableTab",
              dataTableOutput("table")
      ),
      tabItem(tabName = "boxplotTab",
              plotOutput("groupedBoxplot")
      ),
      tabItem(tabName = "averageScoresTab",
              plotOutput("average_scores")
      ),
      tabItem(tabName = "testPreparationTab",
              fluidPage(
                plotOutput("scatterplot_test_prep_math"),
                plotOutput("scatterplot_test_prep_reading"),
                plotOutput("scatterplot_test_prep_writing")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Display the modified dataset in a table with options
  output$table <- renderDataTable({
    StudentsPerformance_3
  }, options = list(
    pageLength = 10,  # Number of rows per page
    lengthMenu = c(5, 10, 20),  # Rows per page options
    dom = 'Bfrtip',  # Buttons and other elements to be displayed
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Buttons to export data
    searching = TRUE  # Enable or disable search feature
  ))
  
  # Create a grouped Boxplot using the "average scores" and "parental.level.of.education" columns
  output$groupedBoxplot <- renderPlot({
    ggplot(StudentsPerformance_3, aes(x = `parental.level.of.education`, y = math.score)) +
      geom_boxplot(aes(fill = "Math"), position = position_dodge(width = 0.8), width = 0.7) +
      geom_boxplot(aes(y = reading.score, fill = "Reading"), position = position_dodge(width = 0.8), width = 0.7) +
      geom_boxplot(aes(y = writing.score, fill = "Writing"), position = position_dodge(width = 0.8), width = 0.7) +
      labs(title = "Grouped Boxplot - Average Scores by Parental Level of Education",
           x = "Parental Level of Education", y = "Score") +
      theme_minimal() +
      scale_fill_manual(values = c("Math" = "skyblue", "Reading" = "lightgreen", "Writing" = "lightcoral"))
  })
  
  # Calculate average scores
  avg_scores <- colMeans(StudentsPerformance_3[, c("math.score", "reading.score", "writing.score")])
  
  # Display average scores in a grouped bar chart
  output$average_scores <- renderPlot({
    bar_data <- data.frame(
      subject = rep(c("Math", "Reading", "Writing"), each = 2),
      gender = rep(c("Male", "Female"), times = 3),
      value = rep(avg_scores, times = 2)
    )
    
    ggplot(bar_data, aes(x = subject, y = value, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Scores by Subject and Gender", x = "Subject", y = "Average Score") +
      theme_minimal()
  })
  
  
  # Display scatter plot for test preparation - Math Score
  output$scatterplot_test_prep_math <- renderPlot({
    ggplot(StudentsPerformance_3, aes(x = test.preparation.course, y = math.score, color = test.preparation.course)) +
      geom_point() +
      ggtitle("Scatterplot - Test Preparation and Math Score") +
      theme_minimal() +
      labs(x = "Test Preparation Course", y = "Math Score")
  })
  
  # Display scatter plot for test preparation - Reading Score
  output$scatterplot_test_prep_reading <- renderPlot({
    ggplot(StudentsPerformance_3, aes(x = test.preparation.course, y = reading.score, color = test.preparation.course)) +
      geom_point() +
      ggtitle("Scatterplot - Test Preparation and Reading Score") +
      theme_minimal() +
      labs(x = "Test Preparation Course", y = "Reading Score")
  })
  
  # Display scatter plot for test preparation - Writing Score
  output$scatterplot_test_prep_writing <- renderPlot({
    ggplot(StudentsPerformance_3, aes(x = test.preparation.course, y = writing.score, color = test.preparation.course)) +
      geom_point() +
      ggtitle("Scatterplot - Test Preparation and Writing Score") +
      theme_minimal() +
      labs(x = "Test Preparation Course", y = "Writing Score")
  })
  
}

# Run the application
shinyApp(ui, server)
