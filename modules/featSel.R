#Feature Engineering-Feature Selection Module

# UI-===============================================================================================
featSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Selection",id="selFea04",
    titlePanel(title="Feature Selection"),
    h4("After transforming your data, extracting potential variables, and creating potential variables, you have
      the opportunity to select a final set of variables for modeling. Look at the variables once more before
      making your final"),
    sidebarLayout(
      sidebarPanel(
       #CONSIDER A FUNCTION HERE??
       # #select input for all variables--choose one predictor which will output plots
       # selectInput01(id="sel_exp1_selFea04",label="Create a group size variable that uses...",
       #               choices=""),
       # #select input for final set of variables
       # selectizeInput(inputId="sel_exp2_selFea04",label="Create a luxury expense variable that uses the sum of",
       #                multiple=TRUE,choices=c("Choose at least two"="",creFea04_luxVec))
      ),
      mainPanel(
      )
    )
  )
}


# Server============================================================================================
featSelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #need code
  
  
  
  })
}



