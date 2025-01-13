# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  title="Arrow v dart",
  
  # title
  titlePanel(h1("Classify arrow versus dart for a North American projectile point",align = "center")),
  
  # Sidebar with input variables 
  sidebarLayout(
    sidebarPanel(
       numericInput(inputId="Length", label="Length of point in mm", 
                    value=48.5, step = .1, min=0, max=500),
       numericInput(inputId="Width", label="Width of point in mm", 
                    value=20.9, step = .1, min=0, max=200),
       selectInput(inputId="Southwest", label="Is it from the Southwest?", 
                    c("No" = "0", "Yes" = "1")),
       selectInput(inputId="Cutoff", label="Cutoff (0.15 recommended; more sensitive to dart)", 
                   c("0.15", '0.3', "0.5")),
       
       headerPanel(""),
       headerPanel(""),
       headerPanel(""),
       headerPanel(""),
       headerPanel(""),
      
       h4("Is your length missing, requiring imputation?"),
       
       #numericInput(inputId="Length2", label="Length of point in mm", 
        #            value=0,min=0, max=100),
       
       numericInput(inputId="Width2", label="Width of point in mm", 
                    value=NA, 
                    min=0, step = .1, max=100),
       
       width = 2
       ),
 
    
    # get prediction
    mainPanel(
      actionButton("Enter", "Get prediction"),
      headerPanel(""),
      htmlOutput("text"),
      htmlOutput("text4"),
      #headerPanel(""),
      plotOutput("plot_foo"),
      #headerPanel(""),
      #htmlOutput("text3"),
      headerPanel(""),
      #headerPanel(""),
      htmlOutput("text2")
      
    )
  )
))
