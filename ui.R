library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("WordCloud"),
  sidebarPanel(
    fileInput("file", label=h3("Upload A Text File")),
    actionButton("goButton", "Create the WordCloud")
    ),
  mainPanel(
    plotOutput("results")
    
    )
))