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
        linebreaks(2),
        #select variable for visualization
        selectizeInput(ns("sel_var_viz"), label="Select a variable to visualize", 
                     multiple=FALSE, 
                     choices=c("Please choose one"="")),
        linebreaks(2)
      ),
      #plot is the only output
      mainPanel(
        plotOutput(ns("plot_sel_var_viz"))
      )
    ),
    #confirmation button dynamically appears (after one var is selected)
    uiOutput(ns("ui_btn_model")),
    linebreaks(2),
    h4(strong("Please choose at least three features to train model.")),
      #select variables for training DF
      uiOutput(ns("ui_rad_vars_model"))
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
    ### Create set of radio buttons based on current variables
    output$ui_rad_vars_model <- renderUI({
      #creates the set of radioButtons
      names(df_train_full)[!names(df_train_full) %in% c(chrVars, "transported")] %>%
        purrr::map(function(x) {
          radioButtons(inputId=ns(paste("rad", x, "model", sep="_")),
                       label=x,
                       choices=c("no", "yes"),
                       selected="no",
                       inline=TRUE)
      }) -> rad_inputs
      
      #creates the logic behind splitting the inputs into columns
      n <- length(rad_inputs)
      set <- round(n/6)
      col_first <- 1:set
      col_second <- (set+1):(2*set)
      col_third <- (2*set+1):(3*set)
      col_fourth <- (3*set+1):(4*set)
      col_fifth <- (4*set+1):(5*set)
      col_sixth <- (5*set+1):n

      #returns the inputs into three columns
      tagList(
        column(width=2, rad_inputs[col_first]),
        column(width=2, rad_inputs[col_second]),
        column(width=2, rad_inputs[col_third]),
        column(width=2, rad_inputs[col_fourth]),
        column(width=2, rad_inputs[col_fifth]),
        column(width=2, rad_inputs[col_sixth])
      )
    })
    
    
    ### Initialize reactiveVal sel_vars (for selected variables) & vars_dropped (vars corresponding to sel_vars)
    sel_vars <- reactiveVal(NULL)
    
    vars_dropped <- reactiveVal(NULL)
    
    
    ## Isolate all inputs related to variables
    var_inputs <- reactive({
      input_ids <- names(input)
      
      input_ids[str_detect(input_ids, "^rad_")]
    })
    
    
    ### Determine selected vars (when relevant inputs are equal to "yes")
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
    
    
    ### Update vars_dropped using custom fn
    observeEvent(sel_vars(), {
      
      vars_dropped(
        id_dropped_vars(sel_vars=sel_vars(),
                        var_pool=ch_preds_full())
      )
    })
    
    
    #### Print evidence that radioButtons are updating [CAN DELETE LATER]
    # observeEvent(sel_vars(), {
    #   print(paste0("sel_vars:", paste(sel_vars(), collapse=", ")))
    #   print(paste0("vars_dropped:", paste(vars_dropped(), collapse=", ")))
    # })
    
    
    ### Update radioButton choices to "no" only for variables that correspond to the selected ones
    observeEvent(list(sel_vars(), vars_dropped()), {
      #force corresponding variable to be "no"
      vars_dropped() %>%
        purrr::map(function(x) {
          updateRadioButtons(inputId=paste("rad", x, "model", sep="_"),
                             label=x,
                             choices=c("no"),
                             selected="no",
                             inline=TRUE)
        })
      
      #bring back "yes" option of corresponding variable if user selects "yes" then "no"
      ch_preds_full()[!ch_preds_full() %in% c(vars_dropped(), sel_vars())] %>%
        purrr::map(function(x) {
          updateRadioButtons(inputId=paste("rad", x, "model", sep="_"),
                             label=x,
                             choices=c("no", "yes"),
                             selected="no",
                             inline=TRUE)
        })
    })
    
    
    
    ## Feature selection confirmation button--------------------
    ### Dynamically display button 
    output$ui_btn_model <- renderUI({
      
      #requires that sel_vars is not null (to avoid initial error)
      req(sel_vars())
      
      #logic to display button--at least three "yes" responses
      req(
        ch_preds_full() %>%
          purrr::map_int(function(x) {
            input[[paste("rad", x, "model", sep="_")]]=="yes"
          }) %>% {sum(.) >= 3}
      )
      
      #generate button
      actionButton(inputId=ns("btn_model"),
                   label="Confirm feature selections",
                   class="btn-success")
    })
    
    
    
    ## Export--------------------
    ### Apply selected variables to full training df
    df_train_select <- eventReactive(input$btn_model, {
      df_train_full %>%
        #id field, selected variables, dependent variable
        select(passenger_id, all_of(sel_vars()), transported) %>%
        #convert transported to fct for modelling
        mutate(transported = as.factor(transported))
    })
    
    
    #check code
    observeEvent(input$btn_model, {
      print(paste("selected features:", paste(names(df_train_select()), collapse=", ")))
    })
    
  
    
    ## Return DF--------------------
    return(df_train_select)
  })
}



