#Feature Engineering-Feature Selection Module

# UI-===============================================================================================
featSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Selection",
    titlePanel(title="Feature Selection"),
    h4("After transforming your data, extracting potential variables, and creating potential variables, you have
      the opportunity to select a final set of variables for modeling. Look at the variables once more before
      making your final decision"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("ui_sel_var_viz"))
       #CONSIDER A FUNCTION HERE??
        #a simple selector of all variables & a visualization that is appropriate for that selector
          #it's going to be a simple bivariate with the var selected and transported
        #another selector, which is used via a renderUI/uiOutput where the user can then select
          #the final set of variables (passenger_id and transported will not be options as they
          #are going to be included regardless)
          #when a user selects one var that's heavily related to others, those vars will drop
            #out of the pool
      ),
      mainPanel(
        plotOutput(ns("plot_sel_var_viz"))
      )
    )
  )
}


# Server============================================================================================
featSelServer <- function(id, df_train_nd_nvI, df_train_nd_nvI_tF, df_train_nd_nvI_cF) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Inputs--------------------
    ### Create reactive DF of full df
    df_train_full <- reactive({
      df_train_nd_nvI() %>%
        left_join(df_train_nd_nvI_tF()) %>%
        left_join(df_train_nd_nvI_cF())
    })
    
    
    ### Create reactive vector of names of non-chr predictors remaining
    ch_preds_full <- reactive({
      extract_pred_class(df_train_full())
    })
  
    
    ### Create selector using reactive vector of headers
    output$ui_sel_var_viz <- renderUI({
      selectizeInput(ns("sel_var_viz"), label="Select a variable to visualize", 
                     multiple=FALSE, 
                     choices=c("Please choose one"="", ch_preds_full()))
    })
    
    
    ### Create reactive 
    
    
    #NOTE:will need to create a reactiveVal of above
    
    
    

    
    ## Outputs--------------------
    ### Plot visualizing selected predictor and transported
    output$plot_sel_var_viz <- renderPlot({
      req(input$sel_var_viz)
      
      #use chr vec for custom fns
      vars_plot <- c("transported", input$sel_var_viz)
      cat_vars <- c("logical", "factor", "ord factor")
      
      #if selected variable is categorical, then barplot
      if(comp_var_class(input$sel_var_viz, ch_preds_full(), cat_vars)) {
        df_train_full() %>%
          barplotter(vars_plot)
      
      #if selected variable is numeric, then boxplot
      } else if(comp_var_class(input$sel_var_viz, ch_preds_full(), "numeric")) {
          df_train_full() %>%
            boxplotter(vars_plot)
        } 
    })
      
  #     # df_train %>% 
  #       # select(-where(is.character), -transported) %>%
  #       apply(df_train, 2, class) %>% 
  #       #switch the colnames and classes (i.e., values and names)
  #       set_names(names(.), .) -> nms
  #     
  #     comp_var_class("vip", tmp, c("logical", "factor")
  #   
  #   
  #   
  #   ### Create reactive DF for visualizing predictors with transported
  #   dat_viz <- reactive({
  #   })
  #   
  #   
  #   ### Create selector
  #   #reactive of vars
  #   # vars_avail <- reactiveVal(NA)
  #   
  #   ch_vars <- reactive({
  #     df_train_nd_nvI() %>%
  #       left_join(df_train_nd_nvI_tF()) %>%
  #       left_join(df_train_nd_nvI_cF()) %>%
  #       select(-c(passenger_id, passenger_group, cabin, num, name, transported)) %>%
  #       names() 
  #   })
  # 
  # # vars_avail(ch_vars())
  #   
  #   #selector
  #   output$ui_sel_var_mod <- renderUI({
  #     selectizeInput(ns("sel_var_mod"), multiple=TRUE, 
  #                    choices=c("Choose as many variable as you prefer"="", ch_vars()))
  #   })
  #   
    
    
  
  
  })
}



