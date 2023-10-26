# Missingness-Other Module


# UI================================================================================================
missOtherUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Explore Missingness",
    titlePanel("Exploring Other Missing Data"),
    sidebarLayout(
      sidebarPanel(
        h4("Let's visualize missingness in all non-character variables."),
        selectInput01(ID=ns("sel_exp_nchrMis03"),label="",choices=nchrMis03_expVec),
        br(),
        h4("Which variable pairs exhibit missingness at random (MAR)?. Compare each variable with missing data to the
        remaining set of variables."),
        selectInput01(ID=ns("sel_compare_nchrMis03"),label="",choices=trainDF_nchrPreds),
        br(),
        #selectInput01(id="sel_imp_nchrMis03",label="",choices=)
      ),
      mainPanel(
        htmlOutput(ns("text_sel_exp_nchrMis03")),
        plotOutput(ns("plot_sel_exp_nchrMis03")),
        br(),
        htmlOutput(ns("text_sel_compare_nchrMis03")),
        DTOutput(ns("tab_sel_compare_nchrMis03"))
      )
    )
  )
}


# Server============================================================================================
missOtherServer <- function(id, trainDF_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ## Exploration
    ### Text output
    output$text_sel_exp_nchrMis03<-renderUI({
      switch(input$sel_exp_nchrMis03,
             miss_occur=h3(paste("Missing Values Occurrences Plot")),
             miss_var=h3(paste("Missing Values per Variable Plot")),
             miss_obs=h3(paste("Missing Values per Observation Plot")),
             miss_patt=h3(paste("Missing Pattern Plot"))
      )
    })
    
    ### Plot output
    output$plot_sel_exp_nchrMis03<-renderPlot({
      switch(input$sel_exp_nchrMis03,
             miss_occur=trainDF_nvI() %>% missing_plot(depVar,trainDF_nchrPreds),
             miss_var=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_var(),
             miss_obs=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_case(),
             miss_patt=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_upset()
      )
    })
  
    
    ## Statistical comparisons
    ### Text output
    output$text_sel_compare_nchrMis03<-renderUI({
      req(input$sel_compare_nchrMis03)
      h3(paste("Missing Data Analysis of",input$sel_compare_nchrMis03))
    })
    
    
    ### Table output
    #### Create reactive
    dat_nchrMis03<-reactive({
      req(input$sel_compare_nchrMis03)
      if(input$sel_compare_nchrMis03 %in% cabinVars){
        sel_vars<-setdiff(trainDF_nchrVars,cabinVars)
      }
      else{sel_vars<-setdiff(trainDF_nchrVars,input$sel_compare_nchrMis03)}
      missing_compare(trainDF_nvI(),dependent=input$sel_compare_nchrMis03,explanatory=sel_vars
      )
    })
  
  
    #### Output reactive
    output$tab_sel_compare_nchrMis03<-renderDT(
      dat_nchrMis03(),options=list(scrollX="400px")
    )
    
  })
}




