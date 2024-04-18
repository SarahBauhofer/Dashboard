# Create a Dashboard with ChatGPT
The task here was to let ChatGPT create a Dashboard and document the process with Opportunities and Obstacles of the work with ChatGPT. For this purpose, a data set was used that was to be visualized on a dashboard. The challenge was to find out if and how ChatGPT could work with visualizations. 

# Requirements
- R-Studio
- ChatGPT 3.5
- Dataset [(StudentsPerformance-3.csv)](https://github.com/SarahBauhofer/Dashboard/blob/78aa55dc48a6bdb0b06922171257ba7195693ca9/StudentsPerformance-3.csv)
- Shiny

# Get Started 
- install R and R-Studio (https://cran.r-project.org/)
- Create a new Project
- Convert Dataset to .csv
- Upload Dataset in R Studio
- Open and register for ChatGPT 3.5 

# About the Dataset
Student performance. This data set contains information about 1000 students about the points achieved in various tests (math, writing, reading). It is not clear which group of students (student, primary school, secondary school) this is. Various influencing factors are included in the data set. Demographic data (gender, ethnicity, parental education level), lunch and participation in the test preparation course. 

# Data Analysis 
The data set is well structured and structured. Data cleaning (searching for duplicates or outliers) was nevertheless carried out. In addition, the influencing factors lunch and ethnicity were not taken into account when creating the dashboard because, on the one hand, they were not suitable for the selected scenario and, on the other hand, the ethnicity was not justifiable for use.

# Datastructure 
Listening on http://127.0.0.1:7376 Unique values for math.score: 72 69 90 47 76 71 88 40 64 38 58 65 78 50 18 46 54 66 44 74 73 67 70 62 63 56 97 81 75 57 55 53 59 82 77 33 52 0 79 39 45 60 61 41 49 30 80 42 27 43 68 85 98 87 51 99 84 91 83 89 22 100 96 94 48 35 34 86 92 37 28 24 26 95 36 29 32 93 19 23 8 Unique values for reading.score: 72 90 95 57 78 83 43 64 60 54 52 81 53 75 89 32 42 58 69 73 71 74 70 65 87 56 61 84 55 44 41 85 59 17 39 80 37 63 51 49 26 68 45 47 86 34 79 66 67 91 100 76 77 82 92 93 62 88 50 28 48 46 23 38 94 97 99 31 96 24 29 40 Unique values for writing.score: 74 88 93 44 75 78 92 39 67 50 52 43 73 70 58 86 28 46 61 63 53 80 72 55 65 38 82 79 83 59 57 54 68 66 62 76 48 42 87 49 10 34 71 37 56 41 22 81 45 36 89 47 90 100 64 98 51 40 84 69 33 60 85 91 77 27 94 95 19 35 32 96 97 99 15 30 23 Unique values for test.preparation.course: none completed 
str(StudentsPerformance_3) command: [1] "gender" "parental.level.of.education" "test.preparation.course" [4] "math.score" "reading.score" "writing.score" [1] "gender" "parental.level.of.education" "test.preparation.course" [4] "math.score" "reading.score" "writing.score" 'data.frame': 1000 obs. of 8 variables: $ gender : chr "female" "female" "female" "male" ... $ race.ethnicity : chr "group B" "group C" "group B" "group A" ... $ pa-rental.level.of.education: chr "bachelor's degree" "some college" "master's degree" "associate's degree" ... $ lunch : chr "standard" "standard" "standard" "free/reduced" ... $ test.preparation.course : chr "none" "completed" "none" "none" ... $ math.score : int 72 69 90 47 76 71 88 40 64 38 ... $ reading.score : int 72 90 95 57 78 83 95 43 64 60 ... $ writing.score : int 74 88 93 44 75 78 92 39 67 50 ...

# First impressions of working with ChatGPT (testing around)
In the beginning, there was a lot of testing and creating various visualizations, questions, scenarios and dashboards.
ChatGPT can help to suggest research questions, scenarios and suitable visualizations for these questions based on the data structure. For example, he can use the data set to create suitable connections, calculate them and suggest a visualization for connections. However, the other way around it is also possible to say, I want to establish a connection between x and y using a specific visualization. Shiny also offers examples of visualizations that can be copied. ChatGPT is then able to adapt the code (with a lot of patience).

# Prompting 
It was important to work out when ChatGPT understands is meant. To do this, the commands had to be tried out and generated. The commands usually included a description of the situation, a description of the troubleshooting or a description of the plan/goal.
A prompt might look like this: You are a teacher…. You have this data set (data structure). You want to use this visualization. Please create a suitable scaling for this visualization. In summary, the commands had to be specified so that details about visualizations could be described. This included data mapping, diagram typologies, stylistics
Configurations, interactivity features and layout design.

# Work with ChatGPT
If you tell ChatGPT that you want to create a dashboard with R, he can immediately list different ways to make it happen. ChatGPT even has access to R data sets that you can use to create a dashboard. So it's not even necessary to try to upload a data set. You need a 
lot of patience though and you constantly have to keep an eye to the generated code. Here are some points that are already known in working with ChatGPT: 
- Pay attention to word choice
- Short and precise expression. Tackle problems step by step, not all at once
- You have to have knowledge in order to be able to correct mistakes yourself
- It helps to generate ideas, for example with the research question or with adding interactions that are possible with the existing visualizations
- Pay attention to the length of the text, avoid long and convoluted sentences
- It often helped to simply have him check the code. If I entered the code and he said the code looked right, that was enough
- If the history is too long, ChatGPT will become slow. This means you often have to open new chats and start a new conversation. This can lead to him coming up with new ideas that improve the code. But it also takes time to constantly have to explain what you are 
  trying to achieve
- It is often helpful that he starts coding straight away, but sometimes it is also annoying when you try to explain something to him. For example, if you explicitly tell him to wait or NOT to code, he still starts coding straight away.
- Has difficulty processing large data sets (scores of all students)

# The Work with ChatGPT and Coding
- Sometimes he doesn't understand exactly what you want. For example, you tell him to remove the “_” between words. It generates the code, but the _ are still visible.
- There were problems creating a table with gender, score and subject because gender has 2 variables and the other 3 variables. He wasn't able to adjust it.
- If you made small changes in the code, you had to remember them or always send ChatGPT the current code so that no “old” lines of your code would continue to be edited (e.g. if lines were swapped, or the number of columns has been changed, or if a text output has 
  been changed.
- He sometimes has problems generating errors. You then have to recognize for yourself where the error lies and explicitly tell ChatGPT what the problem is and what you want as a solution (e.g. send the line in the code in which the error must lie, point out to him 
  that the structure of the codes doesn't make sense)
- Sometimes less would be more. Often it outputs all the code, takes a lot of time and then something is wrong. In some places it would make more sense if he would change the codes on the corresponding lines
- Be careful not to create too many codes or bug fixes in a chat history. This confuses ChatGPT
- You also have to pay close attention to what he changes and where, because he forgets parts of the code or changes the order of the code logic
- Sometimes it's a bit of tinkering if only certain sections of the code need to be changed or adjusted
- Sometimes the desired goals compete with Shiny's presets (red bars, layout dashboard)

# The Work with R-Studio
- Crashed often
- Initial problems uploading the dataset. I had to experiment several times until the .csv file format was recognized correctly and the variables were processed correctly.
- You can work with the data set within R, which helped with calculations
- I was unable to remove columns from dataset in R (ethnicity, lunch)
- Practical that several folders can be attached with Shinyapp, so you can test, compare and edit different versions

# The Work with ChatGPT and R-Studio
- You can paste the complete R Console error message into ChatGPT. He recognizes the problem and generates the code. However, if the same error message keeps coming, he sometimes gets into a loop and always writes the same codes that are supposed to help solve the 
  error. If you don't recognize that, you'll be stuck there forever. You can't ask him about it either, so he doesn't realize how often or that he's using the same code over and over again
- He cannot access links or templates. So you cannot format the dashboard according to a template
- No access to templates or codes from other Github codes (you have to copy and search for them yourself)
- You can say what you want to visualize, he then gives suggestions and codes the whole thing
- The update of ChatGPT must match the current versions of the programs. In some cases he used packages from Shiny that are not compatible with the current version of R.
- He doesn't have access to data sets and can't upload them, but you can send the data structure to the chat and he can work with it.

# The Work with ChatGPT and the Dataset
- The data set is easy to work with, with Chat GPT the data set can also be changed, e.g. removing columns that I don't want to edit in the data set
- The data set is rather static and not particularly suitable for showing developments, for example, so visualizations that show developments or filters with time options are not possible.
- ChatGPT is not able to translate the data record into another language. The variables are no longer recognized either.

# ChatGPT and Interactions in the Dashboard
- ChatGPT can use various interactions from Shiny
- It is detailed work and very time-consuming to adapt the interactions as desired
- It is important to recognize where which interaction makes sense
- However, some interactions did not work: - creating customizability through control menus (e.g. color, font size), -removing or elaborating the tooltip interactions in the visualizations, -automatic adjustments to the visualizations, changing the code as a responsive 
  design, without calculations change or existing theme

# ChatGPT and Formatting the Dashboard
- ChatGPT is able to use different color codes and palettes
- Sometimes the entire layout gets blown away or presets don't allow the colors to be changed
- You can tell ChatGPT what is important to you when designing (e.g. color blindness)
- It is possible to add images, icons or emojis

# Scenario and Task Analysis
To test the usability of the Dashboard, created with ChatGPT, it was important to create Tasks and Scenarios [(https://github.com/SarahBauhofer/Dashboard/blob/7c0f578df4a0b04407a78bc8aa4aaeefa95c561d/Szenarios)](https://github.com/SarahBauhofer/Dashboard/blob/4db1fba0c91741d0865aca09df5fc2d53097f4e5/Szenarios). 
Because the usability test was carried out in Germany, the scenarios are in German. An English translation is also listed.
  
# Conclusion
ChatGPT can be a useful aid for idea and code generation. However, you will never be able to achieve your perfect goal(s) with ChatGPT alone.

# All These lead to the final Code: 

```
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
  
  #Determine which students participated in the test preparation course
  students_participated <- reactive({
    StudentsPerformance_3$student_id %in% StudentsPerformance_3$test.preparation.course
  })
  
  # Inside server() function
  selected_subject <- reactive(input$subject_dropdown)
  
  StudentsPerformance_3$overall_score <- rowSums(StudentsPerformance_3[, c("math.score", "reading.score", "writing.score")])
  StudentsPerformance_3$pass_fail <- ifelse(StudentsPerformance_3$overall_score >= 150, "Pass", "Fail")
  
  
  #Initialize shinyjs
  shinyjs::useShinyjs()
  
  #Add custom CSS for fade-in effect
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
    Student = rep(1:nrow(StudentsPerformance_3), times = 3),
    Preparation = rep(StudentsPerformance_3$test.preparation.course, each = 3)
  )
  
  # Function to check if a student participated in preparation course
  participated_in_prep <- function(student_id) {
    prep_status <- unique(StudentsPerformance_3$test.preparation.course[student_id])
    if (prep_status == "completed") {
      return("completed")
    } else {
      return("none")
    }
  }
  
  # Add column indicating participation in preparation course
  all_scores$Preparation_Check <- sapply(all_scores$Student, participated_in_prep)
  
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
    p <- ggplot(filtered_scores, aes(x = Student, y = Score, color = Preparation_Check)) +
      geom_point() +
      labs(
        x = "Student",
        y = "Score") +
      theme_minimal() +
      facet_wrap(~ Subject, scales = "free_y") +  # Facet by subject
      guides(color = guide_legend(title = "Preparation Course")) +  # Add legend title
      scale_color_manual(values = c("completed" = "blue", "none" = "red"))  # Define colo
    
    # Convert ggplot to plotly
    ggplotly(p, dynamicTicks = TRUE)
  })
  
}
# Run Application
shinyApp(ui, server)


