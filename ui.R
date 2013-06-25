library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("CORD (Chronologically-Ordered Representation of Discourse)"),

  sidebarPanel(

    wellPanel(
      h4("Select Range:"),
      uiOutput("x_range_slider")
    ),
    
    wellPanel(
      h4("Choose categories:"),
      uiOutput("choose_categories")
    )
  ),

  mainPanel(
    plotOutput(outputId = "main_plot", height="600px")
  )
))