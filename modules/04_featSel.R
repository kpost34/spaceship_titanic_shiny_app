#Feature Engineering-Feature Selection Module

# UI-===============================================================================================
featSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Selection",
    titlePanel(title="Feature Selection"),
    h4(paste("After transforming your data, extracting potential variables, and creating potential",
             "variables, you have the opportunity to select a final set of variables for modeling.",
             "Look at the variables once more before making your final decision.")),
    sidebarLayout(
      sidebarPanel(
        #select variable for visualization
        selectizeInput(ns("sel_var_viz"), label="Select a variable to visualize", 
                     multiple=FALSE, 
                     choices=c("Please choose one"="")),
        br(),
        #select variables for training DF
        uiOutput(ns("ui_rad_vars_model"))
        #select variable for training DF
        # selectizeInput(ns("sel_var_model"), label="Choose variables for model",
        #                multiple=TRUE,
        #                choices=c("Please select at least one"="")),
        #confirmation button dynamically appears
        # uiOutput(ns("ui_btn_model"))
      ),
      #plot is the only output
      mainPanel(
        plotOutput(ns("plot_sel_var_viz"))
        # br(),
        # textOutput(ns("vars_info"))
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
    
    
    
    ## Visualization--------------------
    ### Input
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
    
    
    ### Output
    #### Plot visualizing selected predictor and transported
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

    
    
    ## Inputs for Feature Selection--------------------
    ### Create set of UIs based on current variables
    output$ui_rad_vars_model <- renderUI({
      names(df_train_full())[!names(df_train_full()) %in% c(chrVars, "transported")] %>%
        purrr::map(function(x) {
          radioButtons(inputId=paste("rad", x, "model", sep="_"),
                       label=x,
                       choices=c("no", "yes"),
                       selected="no",
                       inline=TRUE)
      })
    })
    
    
    ## Initialize reactiveVal sel_vars (for selected variables) & vars_dropped (vars corresponding to sel_vars)
    sel_vars <- reactiveVal(NULL)
    
    vars_dropped <- reactiveVal(NULL)
    
    
    ## Isolate all inputs related to variables
    var_inputs <- reactive({
      input_ids <- names(input)
      
      input_ids[str_detect(input_ids, "^rad_")]
    })
    
    
    ## Determine selected vars (when relevant inputs are equal to "yes")
    observe(
      sel_vars(
        var_inputs() %>%
          purrr::map_chr(function(x) {
            if(input[[x]]=="yes") {
              str_remove_all(x, "^rad_|_model$")
            } else{NA} #without else, yields NULLs and thus an error
          }) %>%
          na.omit() #remove NAs
      )
    )
    
    
    ## Update vars_dropped using custom fn
    observeEvent(sel_vars(), {
      vars_dropped(
        id_dropped_vars(dat=df_train_full(),
                        sel=sel_vars())
      )
    })
    
    
    ## Print evidence that radioButtons are updating 
    observeEvent(sel_vars(), {
      print(paste0("sel_vars:", paste(sel_vars(), collapse=", ")))
      print(paste0("vars_dropped:", paste(vars_dropped(), collapse=", ")))
    })
    
    
    ## Update radioButton choices to "no" only for variables that correspond to the selected ones
    observeEvent(vars_dropped(), {
      vars_dropped() %>%
        purrr::map(function(x) {
          updateRadioButtons(inputId=paste("rad", x, sep="_"),
                             label=x,
                             choices=c("no"),
                             selected="no",
                             inline=TRUE)
        })
    })
    
    
    
    ## Export--------------------
    ### Apply selected variables to full training df
    df_train_select <- eventReactive(input$btn_model, {
      df_train_full() %>%
        #id field, selected variables, dependent variable
        select(passenger_id, all_of(sel_vars()), transported)
    })
    
    
    
    ## Return DF--------------------
    return(df_train_select)
    
    
    #check code
    observeEvent(sel_vars(), {
      print(df_train_select())
    })
    
    
    
    #NOTES TO SELF:
    #1) remove extraneous code
    #2) organize code--this shouldn't all go under UI
    #3) need to create analogous functions to app4_radbtn.R
    
    
    #Updates to functionality
    #1) remove predictor/variable num (no string variables or transported)
    #2) selecting code is not working--not updating choices of analogous radioButton
    #3) similar to 2 but with the composite variable
    
    
    
    

  #   
  #   
  #   ### Develop pool of choices for variable selector for model
  #   #### Create reactiveVal to retain selected variables
  #   # vars_picked <- reactiveVal(NULL)
  #   
  #   
  #   #### Update vars_picked as selections made
  #   # observeEvent(input$sel_var_model, {
  #   #   vars_picked(input$sel_var_model)
  #   # })
  #   
  #   
  #   #### Initialize reactiveVal of choice pool for model selection
  #   # var_pool <- reactiveVal(ch_preds_full())
  #   
  #   
  #   ### Update var_pool as vars are selected
  #   # var_pool(
  #   #   deplete_var_pool(var_pool(), input$sel_var_model)
  #   # )
  #   
  #   #### Update choice pool depending on selection
  #   # vars_remain <- eventReactive(c(ch_preds_full(), vars_picked()), {
  #   #   deplete_var_pool(var_sel=vars_picked(),
  #   #                    var_pool=ch_preds_full())
  #   # })
  #   
  #   
  #   # observeEvent(input$sel_var_model, {
  #   #   var_pool(
  #   #     deplete_var_pool(var_pool(), input$sel_var_model)
  #   #   )
  #   
  #   
  #   
  #   #### Update selector using pool
  #   #initial update to get the choices
  #   observe({
  #     updateSelectizeInput(inputId="sel_var_model", label="Choose variables for model",
  #                          choices=ch_preds_full())
  #   })
  #   
  #   #update after each selection
  #   observeEvent(input$sel_var_model, {
  #     updateSelectizeInput(inputId="sel_var_model", label="Choose variables for model",
  #                          #dynamically updated variable pool
  #                          choices=deplete_var_pool(var_sel=input$sel_var_model,
  #                                                   var_pool=ch_preds_full()),
  #                          # choices=deplete_var_pool(var_sel=vars_picked(),
  #                          #                          var_pool=ch_preds_full()),
  #                          # choices=vars_remain(),
  #                          #update selected values
  #                          selected=input$sel_var_model)
  #   })
  #   
  #   
  #   #text output (to check code)
  #   output$vars_info <- renderText({
  #     paste("Selected variables:", input$sel_var_model)
  #     paste("Variables selected:", input$sel_var_model)
  #     paste("Variable pool:", deplete_var_pool(var_sel=input$sel_var_model,
  #                                              var_pool=ch_preds_full()))
  #   })
  #   
  # 
  #   ### Button appears once at least one variable selected for model
  #   # output$ui_btn_model <- renderUI({
  #   #   req(length(input$sel_var_model) > 0) #at least 1 variable selected
  #   #   
  #   #   actionButton(ns("btn_model"), 
  #   #                label="Confirm variable selections",
  #   #                class="btn-success")
  #   # })
  # 
  # 
  #   
  # 
  #   
  #   
  #   ## Return DF--------------------
  #   
  #   
  #   
  #   ## Export--------------------
  #   ### Apply selected variables to full training df
  #   # df_train_select <- eventReactive(input$btn_model, {
  #   #   df_train_full() %>%
  #   #     #id field, selected variables, dependent variable
  #   #     select(passenger_id, all_of(input$sel_var_model), transported)
  #   # })
  #   
  #   
  #   
  #   ## Return DF--------------------
  #   # return(df_train_select)
  #   
  #   
  #   
  #   
  #     
  # #     # df_train %>% 
  # #       # select(-where(is.character), -transported) %>%
  # #       apply(df_train, 2, class) %>% 
  # #       #switch the colnames and classes (i.e., values and names)
  # #       set_names(names(.), .) -> nms
  # #     
  # #     comp_var_class("vip", tmp, c("logical", "factor")
  # #   
  # #   
  # #   
  # #   ### Create reactive DF for visualizing predictors with transported
  # #   dat_viz <- reactive({
  # #   })
  # #   
  # #   
  # #   ### Create selector
  # #   #reactive of vars
  # #   # vars_avail <- reactiveVal(NA)
  # #   
  # #   ch_vars <- reactive({
  # #     df_train_nd_nvI() %>%
  # #       left_join(df_train_nd_nvI_tF()) %>%
  # #       left_join(df_train_nd_nvI_cF()) %>%
  # #       select(-c(passenger_id, passenger_group, cabin, num, name, transported)) %>%
  # #       names() 
  # #   })
  # # 
  # # # vars_avail(ch_vars())
  # #   
  # #   #selector
  # #   output$ui_sel_var_mod <- renderUI({
  # #     selectizeInput(ns("sel_var_mod"), multiple=TRUE, 
  # #                    choices=c("Choose as many variable as you prefer"="", ch_vars()))
  # #   })
  # #   
    
    
  
  
  })
}



