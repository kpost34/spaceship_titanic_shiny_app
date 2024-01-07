# Missingness-Other Module


# UI================================================================================================
missOtherUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Explore Missingness",
    titlePanel("Exploring Other Missing Data"),
    sidebarLayout(
      sidebarPanel(width=3,
        #missingness exploration (visualizations)
        h5("Let's visualize missingness in all non-character variables."),
        selectInput01(ID=ns("sel_exp"), label="", choices=ch_exp_nnm_missOther),
        br(),
        
        #test MCAR
        h5("Let's test for missingness completely at random (MCAR) of non-character, predictor variables"),
        radioButtons(ns("rad_mcar"), label="Run test", inline=TRUE, choices=c("Yes", "No"), selected="No"),
        br(),

        #how to handle missing data
        h5("Now that we've visualized and assessed missingness, let's impute data. Please select a method."),
        selectInput01(ID=ns("sel_impute"), label="", choices=ch_impute_missOther),
        actionButton(ns("btn_impute"), label="Submit", class="btn-primary")
      ),
      mainPanel(width=9,
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
        tableOutput(ns("tab_temp"))
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
       miss_occur=df_train_nd_nI() %>% 
         missing_plot(depVar, nchrPredswFn) + labs(title="") + theme_bw(base_size=16),
       miss_var=df_train_nd_nI() %>% select(all_of(nchrVarswFn)) %>% 
         gg_miss_var() + theme_bw(base_size=16),
       miss_obs=df_train_nd_nI() %>% select(all_of(nchrVarswFn)) %>% 
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
      
      df_train_nd_nI() %>%
        select(all_of(nchrPredswFn)) %>%
        naniar::mcar_test() %>%
        mutate(across(c(statistic, p.value), ~signif(.x, 4))) %>%
        DT::datatable(
          rownames=FALSE,
          options=list(dom="t")
        )
    })
    
    
    ## Imputation--------------------
    ### Trigger toast notifications
    observeEvent(input$btn_impute, {
      if(input$sel_impute %in% ch_impute_missOther) {
        show_toast(
          title="Predictor imputation",
          type="success",
          text=impute_predictor_msg(input$sel_impute),
          position="center",
          timer=2000
      )} 
    })
    
    
    ### Create reactive
    df_train_nd_nvI <- eventReactive(input$btn_impute, {
      req(input$sel_impute)

      switch(input$sel_impute,
        lwise_del=na.omit(df_train_nd_nI()),

        mean_imp=df_train_nd_nI() %>%
          mutate(across(all_of(nchrVarswFn), ~Hmisc::impute(x=.x, fun=mean))),

        med_imp=df_train_nd_nI() %>%
          mutate(across(all_of(nchrVarswFn), ~Hmisc::impute(x=.x, fun=median))),

        mult_imp=df_train_nd_nI() %>%
          mice(method="pmm", m=2, maxit=2) %>%
          complete()
      )
    })
    
    ## Submit selection-------------------
    # df_train_nd_nvI <- eventReactive(input$btn_impute, {
    #   dat()
    # })


    #### Output reactive
    output$tab_temp <- renderTable(
      df_train_nd_nvI() %>%
        head()
    )

    
    return(df_train_nd_nvI)
  })
}




