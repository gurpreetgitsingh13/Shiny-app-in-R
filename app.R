# Define UI
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(datasets)
library(shinythemes)
library(shiny)
library(rsconnect)
if (!requireNamespace("shinydashboardPlus", quietly = TRUE)) {
  install.packages("shinydashboardPlus")
}
library(shinydashboardPlus)

rsconnect::deployApp("/Users/gurpreetsingh/Desktop/R_Renders/Intro to R, Geoff/App/Final_App")
ui <- (
  dashboardPage(
    dashboardHeader(title = "Grades on Track",titleWidth = 600),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("HomePage", tabName = "about", icon = icon("clipboard")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("charts", tabName = "chart", icon = icon("dashboard")),
        menuItem("Add Assessment & Grades", tabName = "assessment_details",
                 selectInput("subj", "Subject", choices = c("Maths", "Geography", "Statistics", "Computers")),
                 selectInput("assessment_type", "Type of Assessment",
                             choices = c("Quiz", "Assignment", "Exam"),
                             selected = NA),
                 numericInput("total_marks", "Total Marks", value = 100),
                 numericInput("marks_obtained", "Marks Obtained", value = 0),
                 actionButton("submit", "Submit"),
                 textInput("new_subject", "Add New Subject"),
                 actionButton("add_subject", "Add Subject")
        )
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                fluidRow(
                  infoBox("Select the subject","Using the Add Assessments and Grades tab",icon = icon("thumbs-up")),
                  infoBox("Choose the Chart","To see visuals",icon = icon("percentage"))
                ),
                fluidRow(box(title = "bar plot",plotlyOutput("plot4",height = 250)),
                         box(title = "Pie chart",plotlyOutput("plot5")))),
        
        
        tabItem(tabName = "data",
                dataTableOutput("mydatatable"),
                downloadButton("downloadData", "Download Data")
        ),
        
        #within tabitem(), define the pages for sidebar menu items
        
        tabItem(tabName = "chart",
                ##using box to display plot 
                ##first row
                fluidRow(box(title = "Scatter Plot",plotlyOutput("plot1",height = 250)),
                         box(title = "Histogram ",plotlyOutput("plot2",height = 250)),
                         
                         ## Second row     
                         fluidRow(box(title = "Head  of the data used",tableOutput("data"),width = 7),
                                  box(title = "frequency of cylinder With Bar Plot", plotlyOutput("plot3"),width = 5))
                )
        )
      )
    )
  ) 
)

server <- function(input, output, session) {
  
  Grades <- reactiveVal(data.frame(
    Subject = character(),
    Type_of_assessment = character(),
    total_grades = numeric(),
    obtained_grades = numeric()
  ))
  subjects <- reactiveVal(c("Maths", "Geography", "Statistics", "Computers"))
  
  observeEvent(input$submit, {
    new_data <- data.frame(
      Subject = input$subj,
      Type_of_assessment = input$assessment_type,
      total_grades = input$total_marks,
      obtained_grades = input$marks_obtained
    )
    Grades(rbind(Grades(), new_data))
  })
  
  observeEvent(input$add_subject, {
    # Add the new subject to the subjects reactive value
    subjects(c(subjects(), input$new_subject))
  })
  
  output$mydatatable <- renderDataTable({ Grades() })
  
  output$data = renderTable({
    Grades()
  })
  
  output$plot1 = renderPlotly({
    plot_ly(data = Grades(),
            x = ~total_grades,
            y = ~obtained_grades,
            type = "scatter",
            mode = "markers",
            marker = list(color = ~Subject, size = ~total_grades),
            text = ~paste("Subject: ", Subject, "<br>Total Grades: ", total_grades, "<br>Obtained Grades: ", obtained_grades)
    ) %>%
      layout(
        title = "Scatter Plot of Obtained Grades vs. Total Grades",
        xaxis = list(title = "Total Grades"),
        yaxis = list(title = "Obtained Grades"),
        showlegend = TRUE
      )
  })
  
  
  output$plot2 = renderPlotly({
    plot_ly(data = Grades(), 
            x = ~obtained_grades, 
            type = "histogram", 
            nbinsx = 20,
            marker = list(color = "rgba(100, 200, 255, 0.7)")
    ) %>%
      layout(
        title = "Distribution of Obtained Grades",
        xaxis = list(title = "Obtained Grades"),
        yaxis = list(title = "Frequency"),
        showlegend = FALSE
      )
  })
  
  
  
  output$plot3 = renderPlotly({
    # Filter data for exams only
    exams_data <- subset(Grades(), Type_of_assessment == "Exam")
    
    # Create a box plot with different colors for each subject
    plot_ly(data = exams_data, x = ~Subject, y = ~obtained_grades, type = "box", color = ~Subject)
  })
  
  output$plot4 = renderPlotly({
    data <- table(Grades()$obtained_grades)
    plot_ly(x = names(data), y = data, type = "bar",
            marker = list(color = "rgba(255, 150, 100, 0.7)"),
            text = ~paste("Exam number", data)
    ) %>%
      layout(
        title = "Distribution of Obtained Grades",
        xaxis = list(title = "Obtained Grades"),
        yaxis = list(title = "Grades in exam"),
        showlegend = FALSE
      )
  })
  
  
  
  
  
  output$plot5 = renderPlotly({
    plot_ly(Grades(), labels = ~obtained_grades, values = ~obtained_grades, type = "pie") %>%
      layout(title = 'Pie chart of distribution of marks',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$ApprovedSales = renderInfoBox({
    infoBox("Approval Sales", max(Grades()$obtained_grades), icon = icon("bar-chart-o"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("grades_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(Grades(), file, sheetName = "Grades Data", row.names = FALSE)
    }
  )
  # Update the choices in the subject selectInput dynamically
  observe({
    updateSelectInput(session, "subj", choices = subjects())
  })
}



# Run the app
shinyApp(ui, server)