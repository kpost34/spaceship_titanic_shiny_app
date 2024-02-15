#Feature Engineering-Feature Selection Module

# UI-===============================================================================================
featSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Selection",
    titlePanel(title="Feature Selection"),
    h4("After transforming your data, extracting potential variables, and creating potential variables, you have
      the opportunity to select a final set of variables for modeling. Look at the variables once more before
      making your final decision."),
    sidebarLayout(
      sidebarPanel(
        #select variable for visualization
        selectizeInput(ns("sel_var_viz"), label="Select a variable to visualize", 
                     multiple=FALSE, 
                     choices=c("Please choose one"="")),
        br(),
        #select variable for training DF
        selectizeInput(ns("sel_var_model"), label="Choose variables for model",
                       multiple=TRUE,
                       choices=c("Please select at least one"="")),
        #confirmation button dynamically appears
        uiOutput(ns("ui_btn_model"))
      ),
      #plot is the only output
      mainPanel(
        plotOutput(ns("plot_sel_var_viz")),
        br(),
        textOutput(ns("vars_info"))
      )
    )
  )
}


# Server============================================================================================
featSelServer <- function(id, df_train_nd_nvI, df_train_nd_nvI_tF, df_train_nd_nvI_cF) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Create reactive DF of full df--------------------
    df_train_full <- reactive({
      df_train_nd_nvI() %>%
        left_join(df_train_nd_nvI_tF()) %>%
        left_join(df_train_nd_nvI_cF())
    })
    
    
    
    ## Inputs--------------------
    ### Develop pool of choices for variable selector for visualizations
    #### Create reactive vector of names of non-chr predictors remaining
    ch_preds_full <- reactive({
      extract_pred_class(df_train_full())
    })
  
    
    #### Update selector choices
    observeEvent(ch_preds_full(), {
      updateSelectizeInput(inputId="sel_var_viz", 
                           choices=c("Please choose one"="", ch_preds_full()))
    })
    
    
    ### Develop pool of choices for variable selector for model
    #### Create reactiveVal to retain selected variables
    # vars_picked <- reactiveVal(NULL)
    
    
    #### Update vars_picked as selections made
    # observeEvent(input$sel_var_model, {
    #   vars_picked(input$sel_var_model)
    # })
    
    
    #### Initialize reactiveVal of choice pool for model selection
    # var_pool <- reactiveVal(ch_preds_full())
    
    
    ### Update var_pool as vars are selected
    # var_pool(
    #   deplete_var_pool(var_pool(), input$sel_var_model)
    # )
    
    #### Update choice pool depending on selection
    # vars_remain <- eventReactive(c(ch_preds_full(), vars_picked()), {
    #   deplete_var_pool(var_sel=vars_picked(),
    #                    var_pool=ch_preds_full())
    # })
    
    
    # observeEvent(input$sel_var_model, {
    #   var_pool(
    #     deplete_var_pool(var_pool(), input$sel_var_model)
    #   )
    
    
    
    #### Update selector using pool
    #initial update to get the choices
    observe({
      updateSelectizeInput(inputId="sel_var_model", label="Choose variables for model",
                           choices=ch_preds_full())
    })
    
    #update after each selection
    observeEvent(input$sel_var_model, {
      updateSelectizeInput(inputId="sel_var_model", label="Choose variables for model",
                           #dynamically updated variable pool
                           choices=deplete_var_pool(var_sel=input$sel_var_model,
                                                    var_pool=ch_preds_full()),
                           # choices=deplete_var_pool(var_sel=vars_picked(),
                           #                          var_pool=ch_preds_full()),
                           # choices=vars_remain(),
                           #update selected values
                           selected=input$sel_var_model)
    })
    
    
    #text output (to check code)
    output$vars_info <- renderText({
      paste("Selected variables:", input$sel_var_model)
      paste("Variables selected:", input$sel_var_model)
      paste("Variable pool:", deplete_var_pool(var_sel=input$sel_var_model,
                                               var_pool=ch_preds_full()))
    })
    
  
    ### Button appears once at least one variable selected for model
    # output$ui_btn_model <- renderUI({
    #   req(length(input$sel_var_model) > 0) #at least 1 variable selected
    #   
    #   actionButton(ns("btn_model"), 
    #                label="Confirm variable selections",
    #                class="btn-success")
    # })

  
    
    ## Outputs--------------------
    ### Plot visualizing selected predictor and transported
    output$plot_sel_var_viz <- renderPlot({
      req(input$sel_var_viz)
      
      #use chr vec for custom fns
      vars_plot <- c(input$sel_var_viz, "transported")
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
    
    
    
    ## Export--------------------
    ### Apply selected variables to full training df
    # df_train_select <- eventReactive(input$btn_model, {
    #   df_train_full() %>%
    #     #id field, selected variables, dependent variable
    #     select(passenger_id, all_of(input$sel_var_model), transported)
    # })
    
    
    
    ## Return DF--------------------
    # return(df_train_select)
    
    
    
    
      
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



