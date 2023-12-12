# Missingness-Other Module


# UI================================================================================================
missOtherUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Explore Missingness",
    titlePanel("Exploring Other Missing Data"),
    sidebarLayout(
      sidebarPanel(width=2,
        #missingness exploration (visualizations)
        h5("Let's visualize missingness in all non-character variables."),
        selectInput01(ID=ns("sel_exp"), label="", choices=ch_exp_nnm_missOther),
        br(),
        
        #test MCAR
        h5("Let's test for missingness completely at random (MCAR) of non-character, predictor variables"),
        radioButtons(ns("rad_mcar"), label="Run test", inline=TRUE, choices=c("Yes", "No"), selected="No"),
        br(),

        #how to handle missing data
        h5("Now that we've visualized and assessed missingness, let's impute data."),
        selectInput01(ID=ns("sel_impute"), label="", choices=ch_impute_missOther)
        
      ),
      mainPanel(width=10,
        splitLayout(cellWidths=c("65%", "35%"),
          h4(textOutput(ns("text_sel_exp"))),
          h4(textOutput(ns("text_mcar")))
        ),
        br(),
        splitLayout(cellWidths=c("65%", "35%"),
          plotOutput(ns("plot_sel_exp")),
          DTOutput(ns("tab_mcar"))
        ),
        br(),
      )
    )
  )
}


# Server============================================================================================
missOtherServer <- function(id, df_train_nd_nI) {
  moduleServer(id, function(input, output, session) {
    
    ## Exploration--------------------
    ### Text output
    output$text_sel_exp <- renderText({
      switch(input$sel_exp,
             miss_occur=paste("Missing Values Occurrences Plot"),
             miss_var=paste("Missing Values per Variable Plot"),
             miss_obs=paste("Missing Values per Observation Plot")
      )
    })
    
    ### Plot output
    output$plot_sel_exp <- renderPlot({
      switch(input$sel_exp,
       miss_occur=df_train_nd_nvI() %>% 
         missing_plot(depVar, nchrPreds) + labs(title="") + theme_bw(base_size=16),
       miss_var=df_train_nd_nvI() %>% select(all_of(nchrVars)) %>% 
         gg_miss_var() + theme_bw(base_size=16),
       miss_obs=df_train_nd_nvI() %>% select(all_of(nchrVars)) %>% 
         gg_miss_case() + theme_bw(base_size=16)
      )
    })
  
    
    ## Statistical test--------------------
    ### Text output
    output$text_mcar <- renderText({
      req(input$rad_mcar=="Yes")
      
      paste("Results of Little's MCAR test")
    })
    
    
    ### Tabular output
    output$tab_mcar <- renderDT({
      req(input$rad_mcar=="Yes")
      
      df_train_nd_nvI() %>%
        select(all_of(nchrPreds)) %>%
        naniar::mcar_test() %>%
        mutate(across(c(statistic, p.value), ~signif(.x, 4))) %>%
        DT::datatable(
          rownames=FALSE,
          options=list(dom="t")
        )
    })
    
    
    ## Imputation--------------------
    ### Create reactive
#     df_train_nd_nvI <- reactive({
#       req(input$sel_impute)
#       
#       switch(input$sel_impute,
#         lwise_del=na.omit(df_train),
#           
#         mean_imp=df_train_nI() %>% 
#           mutate(across(all_of(nchrPreds), ~Hmisc::impute(x=.x, fun=mean))),
#           
#         med_imp=df_train_nI() %>% 
#           mutate(across(all_of(nchrPreds), ~Hmisc::impute(x=.x, fun=median))),
#           
#         mult_imp=
#       
#       
#       
#       
#     })
#   
#   
#     #### Output reactive
#     output$tab_sel_compare <- renderDT(
#       dat(), options=list(scrollX="400px")
#     )
#     
  })
}




