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
    #TEMPORARILY COMMENT OUT and change all df_train_full() to df_train_full
    # df_train_full <- reactive({
    #   df_train_nd_nvI() %>%
    #     left_join(df_train_nd_nvI_tF()) %>%
    #     left_join(df_train_nd_nvI_cF())
    # })
    
    
    
    ## Visualization--------------------
    ### Input
    ### Develop pool of choices for variable selector for visualizations
    #### Create reactive vector of names of non-chr predictors remaining
    ch_preds_full <- reactive({
      extract_pred_class(df_train_full)
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
        df_train_full %>%
          barplotter(vars_plot)
      
      #if selected variable is numeric, then boxplot
      } else if(comp_var_class(input$sel_var_viz, ch_preds_full(), "numeric")) {
          df_train_full %>%
            boxplotter(vars_plot)
        } 
    })

    
    
    ## Inputs for Feature Selection--------------------
    ### Create set of UIs based on current variables
    output$ui_rad_vars_model <- renderUI({
      names(df_train_full)[!names(df_train_full) %in% c(chrVars, "transported")] %>%
        purrr::map(function(x) {
          radioButtons(inputId=ns(paste("rad", x, "model", sep="_")),
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
      req(sel_vars()) #without it then error is produced b/c of str_detect on empty string in id_dropped_vars()
      
      vars_dropped(
        id_dropped_vars(sel_vars=sel_vars(),
                        var_pool=ch_preds_full())
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
          updateRadioButtons(inputId=paste("rad", x, "model", sep="_"),
                             label=x,
                             choices=c("no"),
                             selected="no",
                             inline=TRUE)
        })
    })
    
    
    
    ## Export--------------------
    ### Apply selected variables to full training df
    df_train_select <- eventReactive(input$btn_model, {
      df_train_full %>%
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
  
  
  })
}



