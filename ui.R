library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("CORDTRA"),

  sidebarPanel(
    
    wellPanel(
      h4("Select Range:"),
      uiOutput("x_range_slider")
    ),
    
    wellPanel(
      h4("Choose categories:"),
      uiOutput("choose_categories")
    ),
    
    HTML(paste(textOutput("hits"), "by <a href ='http://bodongchen.com/' target='_blank'>Bodong Chen</a>"))
  ),

  mainPanel(
    tabsetPanel(
      # CORDTRA
      tabPanel("CORDTRA Diagram", 
               plotOutput(outputId = "main_plot", height="550px")),
      
      # Co-occurrence matrix"
      tabPanel("Co-occurrence", 
               plotOutput(outputId = "cooccurrence_plot"),
               h4("Matrix:"),
               htmlOutput("cooccurrence_matrix")),
      
      # Motion chart
      tabPanel("Motion Chart",
               tableOutput("gvMotion"))
    )
    
  )
))
