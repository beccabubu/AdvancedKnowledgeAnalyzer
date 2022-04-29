library(shiny)
library(shinyjs)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Advanced Knowledge Analyzer"),
  sidebarLayout(
    sidebarPanel(
      #-------------------------------
      # test on starting page
      radioButtons("RBstart","Welcome to the test",
                   selected = T,
                   choiceNames = c("Data Analyst",
                     "Data Science",
                     "Information Security",
                     "Project Manager"),
                   choiceValues = 1:4),
      actionButton("start", "Start test"),
      
      # ------------------------------
      
      shinyjs::hidden(actionButton("backButton","Prev")),
      shinyjs::hidden(actionButton("goButton", "Next")),
      shinyjs::hidden(actionButton('submitButton', 'Submit')),
      shinyjs::hidden(actionButton("toMain","Back")),
      actionButton("scoreButton", "Test Result")
      ),
      mainPanel(shinyjs::hidden(textOutput("question")),
                shinyjs::hidden(uiOutput("answers")),
                shinyjs::hidden(tableOutput("grade")),
                shinyjs::hidden(textOutput("test")),
                shinyjs::hidden(textOutput("test2")),
                shinyjs::hidden(tableOutput("scoretable")),
                shinyjs::hidden(downloadButton("download","download the table")),
                shinyjs::hidden(uiOutput("testResult")),
                shinyjs::hidden(textInput("nameInput","name")),
                shinyjs::hidden(textInput("passwordInput", "password")),
                shinyjs::hidden(actionButton("loginButton","login")),
                # create two plots
                shinyjs::hidden(plotOutput("histogram")),
                shinyjs::hidden(plotOutput("bp")),
                shinyjs::hidden(tableOutput("testDetail")) # add test detail info table.
                )
      )
)
