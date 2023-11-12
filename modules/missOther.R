# Missingness-Other Module


# UI================================================================================================
missOtherUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Explore Missingness",
    titlePanel("Exploring Other Missing Data"),
    sidebarLayout(
      sidebarPanel(width=3,
        h4("Let's visualize missingness in all non-character variables."),
        selectInput01(ID=ns("sel_exp"),label="",choices=nchrMis03_expVec),
        br(),
        h4("Which variable pairs exhibit missingness at random (MAR)?. Compare each variable with missing data to the
        remaining set of variables."),
        selectInput01(ID=ns("sel_compare"),label="",choices=nchrPreds),
        br(),
        #selectInput01(id="sel_imp_nchrMis03",label="",choices=)
      ),
      mainPanel(width=9,
        h3(textOutput(ns("text_sel_exp"))),
        plotOutput(ns("plot_sel_exp")),
        br(),
        h3(textOutput(ns("text_sel_compare"))),
        DTOutput(ns("tab_sel_compare"))
      )
    )
  )
}


# Server============================================================================================
missOtherServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ## Exploration
    ### Text output
    output$text_sel_exp <- renderText({
      switch(input$sel_exp,
             miss_occur=paste("Missing Values Occurrences Plot"),
             miss_var=paste("Missing Values per Variable Plot"),
             miss_obs=paste("Missing Values per Observation Plot"),
             miss_patt=paste("Missing Pattern Plot")
      )
    })
    
    ### Plot output
    output$plot_sel_exp <- renderPlot({
      switch(input$sel_exp,
             miss_occur=df_train_nvI() %>% missing_plot(depVar, nchrPreds),
             miss_var=df_train_nvI() %>% select(all_of(nchrVars)) %>% gg_miss_var(),
             miss_obs=df_train_nvI() %>% select(all_of(nchrVars)) %>% gg_miss_case(),
             miss_patt=df_train_nvI() %>% select(all_of(nchrVars)) %>% gg_miss_upset()
      )
    })
  
    
    ## Statistical comparisons
    ### Text output
    output$text_sel_compare <- renderText({
      req(input$sel_compare)
      paste("Missing Data Analysis of",input$sel_compare)
    })
    
    
    ### Table output
    #### Create reactive
    dat <- reactive({
      req(input$sel_compare)
      if(input$sel_compare %in% cabinVars){
        sel_vars<-setdiff(nchrVars, cabinVars)
      }
      else{sel_vars<-setdiff(nchrVars, input$sel_compare)}
      missing_compare(df_train_nvI(), dependent=input$sel_compare, explanatory=sel_vars
      )
    })
  
  
    #### Output reactive
    output$tab_sel_compare <- renderDT(
      dat(), options=list(scrollX="400px")
    )
    
  })
}




