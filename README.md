# Install and load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, ggplot2, corrplot, reshape2, plotly, DT, shinythemes, shinyjs, shinydashboardPlus, RColorBrewer)

# Read dataset
StudentsPerformance_3 <- read.csv("https://raw.githubusercontent.com/SarahBauhofer/Dashboard/main/StudentsPerformance-3.csv")

# Data preprocessing
StudentsPerformance_3 <- StudentsPerformance_3[, !(names(StudentsPerformance_3) %in% c("lunch", "race.ethnicity"))]
passing_threshold <- 50
StudentsPerformance_3$math_pass_fail <- ifelse(StudentsPerformance_3$math.score >= passing_threshold, "Pass", "Fail")
StudentsPerformance_3$writing_pass_fail <- ifelse(StudentsPerformance_3$writing.score >= passing_threshold, "Pass", "Fail")
StudentsPerformance_3$reading_pass_fail <- ifelse(StudentsPerformance_3$reading.score >= passing_threshold, "Pass", "Fail")

avg_scores <- aggregate(cbind(math.score, reading.score, writing.score) ~ test.preparation.course, 
                        data = StudentsPerformance_3, FUN = function(x) round(mean(x, na.rm = TRUE), 2))
colnames(avg_scores) <- c("Test Preparation Course", "Math", "Reading", "Writing")
avg_scores_long <- reshape2::melt(avg_scores, id.vars = "Test Preparation Course", variable.name = "Subject", value.name = "Average Score")


# Define UI
ui <- fluidPage(
  titlePanel(
    div(
      style = "background-color: #343a40; padding: 10px; margin-bottom: 15px;",
      tags$h1(
        style = "color: white; margin: 0;",
        "Test Preparation Course Evaluation Dashboard"
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"), # Include Font Awesome stylesheet
    tags$style(
      HTML("
      .card {
        border-radius: 10px;
        background-color: #adb5bd;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        padding: 15px;
        margin: 15px;
        transition: background-color 0.3s;
        text-align: center;
      }
      
      ")
    ),
    tags$script("
      $(document).ready(function(){
        $('#show_kpis').change(function(){
          var isChecked = $(this).prop('checked');
          if (isChecked) {
            $('.section1').show();
          } else {
            $('.section1').hide();
          }
        });
      });
    ")
  ),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar {
        background-color: #adb5bd !important;
        border-right: 1px solid #adb5bd !important;
      }
      .sidebar .text-light {
        color: #adb5bd !important;
      }
    ")),
    fluidRow(
      column(2,
             checkboxInput("show_kpis", label = "Show KPIs", value = TRUE)
      )
    ),
    fluidRow(
      column(2,
             div(class = "card section1",
                 h4("Total Participants"),
                 div(textOutput("total_participants_text"), style = "font-size: 24px; color: #adb5bd;"),  
                 div(class = "icon", HTML('<i class="fas fa-users"></i>'))  # Using a Font Awesome icon
             )
      ),
      column(2,
             div(class = "card section1",
                 h4("Participation Rate"),
                 div(textOutput("participant_rate_text"), style = "font-size: 24px; color: #adb5bd;"),
                 div(class = "icon", HTML('<i class="fas fa-chart-pie"></i>'))  # Using a Font Awesome icon
             )
      ),
      column(2,
             div(class = "card section1",
                 h4("Failure Rate"),
                 div(textOutput("failure_rate_text"), style = "font-size: 24px; color: #adb5bd;"),
                 div(class = "icon", HTML('<i class="fas fa-chart-line"></i>'))  # Using a Font Awesome icon
             )
      ),
      column(2,
             div(class = "card section1",
                 h4("Preparation Course Fail"),
                 div(textOutput("preparation_course_fail_text"), style = "font-size: 24px; color: #adb5bd;"),
                 div(class = "icon", HTML('<i class="fas fa-times-circle"></i>'))  # Using a Font Awesome icon for failure
             )
      ),
      column(2,
             div(class = "card section1",
                 h4("Preparation Course Pass"),
                 div(textOutput("passing_students_count_text"), style = "font-size: 24px; color: #adb5bd;"),
                 div(class = "icon", HTML('<i class="fas fa-check-circle"></i>'))  # Using a Font Awesome icon
             )
      )
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #adb5bd !important;
      }
      .card {
        background-color: #343a40 !important;
        border: 1px solid #6c757d !important;
        color: #adb5bd !important;
      }
      .card h3 {
        margin-top: 0;
      }
    ")),
    fluidRow(
      column(12,
             div(class = "card section2",
                 selectizeInput("subject_dropdown", "Select Subject", 
                                choices = c("All", "Math", "Reading", "Writing"), 
                                selected = "All",
                                options = list(
                                  placeholder = 'Select a subject',
                                  liveSearch = TRUE,
                                  style = 'btn-primary'
                                )
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "card section2 bar-chart-card",
                 h3("Failure Rate by Parental Level of Education", style = "text-align: center;"),
                 selectInput("education_dropdown", "Select Parental Level of Education", 
                             choices = unique(StudentsPerformance_3$parental.level.of.education),
                             selected = NULL),
                 fluidRow(
                   column(4, plotlyOutput("bar_chart_test_prep_math")),
                   column(4, plotlyOutput("bar_chart_test_prep_reading")),
                   column(4, plotlyOutput("bar_chart_test_prep_writing"))
                 )
             )
      )
    ),
    fluidRow(
      column(6,
             div(class = "card section3 average-score-table-card",
                 h3("Average Scores", style = "text-align: center; color: #adb5bd;"),
                 div(style = "background-color: white; text-align: left",
                     dataTableOutput("average_scores_table"))
             )
      ),
      column(6,
             div(class = "card section4 scatter-plot-card",
                 h3("Scores of Each Student", style = "text-align: center; color: #adb5bd;"),
                 selectInput("cluster_dropdown", "Clustering Criterion:",
                             choices = c("All", "Top 25%", "Bottom 25%", "Higher than 50", "Lower than or equal to 50")),
                 plotlyOutput("scatter_plot")  
             )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  total_participants <- nrow(StudentsPerformance_3)
  
  # Inside server() function
  selected_subject <- reactive(input$subject_dropdown)
  
  StudentsPerformance_3$overall_score <- rowSums(StudentsPerformance_3[, c("math.score", "reading.score", "writing.score")])
  StudentsPerformance_3$pass_fail <- ifelse(StudentsPerformance_3$overall_score >= 150, "Pass", "Fail")
  
  # Initialize shinyjs
  shinyjs::useShinyjs()
  
  # Add custom CSS for fade-in effect
  shinyjs::inlineCSS("
    .fade-in {
      opacity: 0;
      transition: opacity 0.3s;
    }
    .fade-in.active {
      opacity: 1;
    }
  ")
  
  # Section 1: Key Performance Indicators (KPIs)
  output$total_participants_text <- renderText({
    paste(total_participants)
  })
  
  failure_rate <- function() {
    participants_failed <- sum(StudentsPerformance_3$pass_fail == "Fail")
    failure_rate_value <- participants_failed / total_participants * 100
    return(paste(round(failure_rate_value, 2), "%"))
  }
  
  preparation_course_fail_count <- function() {
    preparation_course_fail <- sum(StudentsPerformance_3$test.preparation.course == "completed" & StudentsPerformance_3$pass_fail == "Fail")
    return(preparation_course_fail)
  }
  
  output$participant_rate_text <- renderText({
    participants_completed <- sum(StudentsPerformance_3$test.preparation.course == "completed")
    participant_rate_value <- participants_completed / total_participants * 100
    paste(round(participant_rate_value, 2), "%")
  })
  
  output$failure_rate_text <- renderText({
    failure_rate()
  })
  
  output$preparation_course_fail_text <- renderText({
    preparation_course_fail_count()
  })
  
  # Passing Students Count who participated in the preparation course
  output$passing_students_count_text <- renderText({
    passing_students_count <- sum(StudentsPerformance_3$pass_fail == "Pass" & StudentsPerformance_3$test.preparation.course == "completed")
    paste(passing_students_count)
  })
  
  
  # Section 2: Overview
  output$average_scores_table <- renderDT({
    if (input$subject_dropdown == "All") {
      datatable(avg_scores, 
                options = list(pageLength = 10, lengthMenu = c(10), lengthChange = FALSE, search = NULL, dom = 't'),
                rownames = FALSE,
                colnames = c("Test Preparation Course", "Math", "Reading", "Writing"))
    } else {
      filtered_avg_scores_long <- avg_scores_long[avg_scores_long$Subject == input$subject_dropdown, ]
      datatable(filtered_avg_scores_long, 
                options = list(pageLength = 10, lengthMenu = c(10), lengthChange = FALSE, search = NULL, dom = 't'),
                rownames = FALSE,
                colnames = c("Test Preparation Course", "Subject", "Average Score"))
    }
  })
  
  # Section 3: Test Preparation Bar Charts 
  # Function to render test preparation bar chart for a specific subject and education level
  render_test_prep_bar_chart <- function(selected_subject, selected_education) {
    pass_fail_data <- table(StudentsPerformance_3$test.preparation.course,
                            StudentsPerformance_3[[paste0(tolower(selected_subject), "_pass_fail")]],
                            StudentsPerformance_3$parental.level.of.education)
    
    pass_fail_df <- as.data.frame(pass_fail_data)
    names(pass_fail_df) <- c("Test Preparation Course", "Pass/Fail", "Education Level", "Count")
    
    pass_fail_df <- pass_fail_df[pass_fail_df$`Education Level` == selected_education, ]
    pass_fail_df$Percentage <- ifelse(pass_fail_df$`Pass/Fail` == "Fail",
                                      (pass_fail_df$Count / sum(pass_fail_df$Count)) * 100, 0)
    
    difference <- pass_fail_df$Percentage[1] - pass_fail_df$Percentage[2]
    
    p <- ggplot(pass_fail_df, aes(x = `Test Preparation Course`, y = Percentage, fill = `Pass/Fail`,
                                  text = paste("Failures: ", pass_fail_df$Count, "<br>",
                                               "Fail Percentage: ", sprintf("%.2f", pass_fail_df$Percentage), "%"))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_segment(aes(x = 1, xend = 1.55, y = pass_fail_df$Percentage[1], yend = pass_fail_df$Percentage[2], text = NA),
                   arrow = arrow(type = "closed", angle = 20, length = unit(0.1, "inches")), color = "black", size = 0.5) +
      geom_text(aes(x = 1.275, y = mean(pass_fail_df$Percentage), label = sprintf("%.2f", difference)),
                color = "black", size = 4, vjust = 0.5, hjust = -0.5) +
      labs(title = paste(selected_subject, "-", selected_education),
           x = "Test Preparation Course", y = "Fail Rate (%)") +
      theme_minimal() +
      guides(fill = FALSE)
    
    ggplotly(p, tooltip = "text")
  }
  
  # Render Math test preparation bar chart
  output$bar_chart_test_prep_math <- renderPlotly({
    if (selected_subject() %in% c("All", "Math")) {
      shinyjs::addClass(selector = "subject_dropdown", class = "fade-in")
      render_test_prep_bar_chart("Math", input$education_dropdown)
    }
  })
  
  # Render Reading test preparation bar chart
  output$bar_chart_test_prep_reading <- renderPlotly({
    if (selected_subject() %in% c("All", "Reading")) {
      shinyjs::addClass(selector = "subject_dropdown", class = "fade-in")
      render_test_prep_bar_chart("Reading", input$education_dropdown)
    }
  })
  
  # Render Writing test preparation bar chart
  output$bar_chart_test_prep_writing <- renderPlotly({
    if (selected_subject() %in% c("All", "Writing")) {
      shinyjs::addClass(selector = "subject_dropdown", class = "fade-in")
      render_test_prep_bar_chart("Writing", input$education_dropdown)
    }
  })
  
  
  # Section 4: Scatter Plot (Continued)
  # Combine all individual scores into one dataset
  all_scores <- data.frame(
    Subject = rep(c("Math", "Reading", "Writing"), each = nrow(StudentsPerformance_3)),
    Score = c(StudentsPerformance_3$math.score, StudentsPerformance_3$reading.score, StudentsPerformance_3$writing.score),
    Student = rep(1:nrow(StudentsPerformance_3), times = 3)
  )
  
  # Render scatter plot
  output$scatter_plot <- renderPlotly({
    # Define data based on selected subject
    if (input$subject_dropdown == "All") {
      filtered_scores <- all_scores
    } else {
      filtered_scores <- all_scores[all_scores$Subject == input$subject_dropdown, ]
    }
    
    # Apply clustering based on selected criterion
    if (!is.null(input$cluster_dropdown)) {
      switch(input$cluster_dropdown,
             "Top 25%" = {
               score_threshold <- quantile(filtered_scores$Score, 0.75)
               filtered_scores <- filtered_scores[filtered_scores$Score >= score_threshold, ]
             },
             "Bottom 25%" = {
               score_threshold <- quantile(filtered_scores$Score, 0.25)
               filtered_scores <- filtered_scores[filtered_scores$Score <= score_threshold, ]
             },
             "Higher than 50" = {
               filtered_scores <- filtered_scores[filtered_scores$Score > 50, ]
             },
             "Lower than or equal to 50" = {
               filtered_scores <- filtered_scores[filtered_scores$Score <= 50, ]
             }
      )
    }
    
    # Create the scatter plot
    p <- ggplot(filtered_scores, aes(x = Student, y = Score, color = Subject)) +
      geom_point() +
      labs(
        x = "Student",
        y = "Score") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +  # Using a color palette suitable for colorblind users
      facet_wrap(~ Subject, scales = "free_y") +  # Facet by subject
      guides(color = FALSE)  # Remove legend
    
    # Convert ggplot to plotly
    ggplotly(p, dynamicTicks = TRUE)
  })
  
}

shinyApp(ui, server)
