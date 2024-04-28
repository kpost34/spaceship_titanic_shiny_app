#Modelling-Model Selection Module

# UI-===============================================================================================
modSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Model Selection",
    titlePanel("Model Selection"),
    sidebarLayout(
      sidebarPanel(width=2,
        #assess model performance
        checkboxGroupInput(ns("chk_mod"), 
                           label="Please select one or more model types for assessment",
                           choices=ch_mod_type),
        linebreaks(2),
        #choose which model to tune
        radioButtons(ns("rad_mod"), 
                     "Select a model type for tuning",
                     choices=ch_mod_type,
                     selected=character(0)),
        linebreaks(2),
        #hyperparams: select which ones
        checkboxGroupInput(ns("chk_hyper"), label="", choices=NULL),
        #hyperparams: choose number of levels
        uiOutput(ns("ui_slid_hyper")),
        #confirmation button
        uiOutput(ns("ui_btn_confirm")),
      ),
      
      ## Tabular outputs
      mainPanel(width=10,
        #tables of summary stats of model types
        fluidRow(
          column(3,
            DTOutput(ns("tab_mod1"))
          ),
          column(1),
          column(3,
            DTOutput(ns("tab_mod2"))
          ),
          column(1),
          column(3,
            DTOutput(ns("tab_mod3"))
          )
        ),
        br(),
        # fluidRow(
        #   column(3,
        #     DTOutput(ns("tab_hyper_levels")),
        #   ),
        #   column(1),
        #   column(8,
            DTOutput(ns("tab_mod_tune"))
        #   )
        # )
      )
    )
  )
}


# Server============================================================================================
modSelServer <- function(id, df_train_select, df_vfold) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Create reactives of resample fits--------------------
    ### Extract formula
    form <- reactive({
      grab_formula(df_train_select())
    })
    
    
    ### Model fitting and 
    #### By model type
    #first model selected
    fit_rs_log <- reactive({
      
      create_fit_model(type="log_reg",
                       formula=form(),
                       folds=df_vfold())
    })
    
    #second model selected
    fit_rs_tree <- reactive({
      
      create_fit_model(type="dec_tree",
                       formula=form(),
                       folds=df_vfold())
    })
    
    
    #third model selected
    fit_rs_forest <- reactive({
      
      create_fit_model(type="forest",
                       formula=form(),
                       folds=df_vfold())
    })
    
    
    ### By model position
    fit_rs1 <- reactive({
      req(input$chk_mod[1])
      
      store_model(sel=input$chk_mod[1],
                  mod_log=fit_rs_log(),
                  mod_tree=fit_rs_tree(),
                  mod_forest=fit_rs_forest())
    })
    
    fit_rs2 <- reactive({
      req(input$chk_mod[2])
      
      store_model(sel=input$chk_mod[2],
                  mod_log=fit_rs_log(),
                  mod_tree=fit_rs_tree(),
                  mod_forest=fit_rs_forest())
    })
    
    fit_rs3 <- reactive({
      req(input$chk_mod[3])
      
      store_model(sel=input$chk_mod[3],
                  mod_log=fit_rs_log(),
                  mod_tree=fit_rs_tree(),
                  mod_forest=fit_rs_forest())
    })
    
    
    ## Generate tables--------------------
    ### Model selection
    #first table
    output$tab_mod1 <- renderDT(
      assess_model(fit_rs1()),
      rownames=FALSE, 
      options=list(dom="t",
                   autoWidth=TRUE,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        ch_mod_type[ch_mod_type==input$chk_mod[1]] %>% names())
    )
    
    
    #second table
    output$tab_mod2 <- renderDT(
      assess_model(fit_rs2()),
      rownames=FALSE, 
      options=list(dom="t",
                   autoWidth=TRUE,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        ch_mod_type[ch_mod_type==input$chk_mod[2]] %>% names())
    )
    
    
    #third table
    output$tab_mod3 <- renderDT(
      assess_model(fit_rs3()),
      rownames=FALSE, 
      options=list(dom="t",
                   autoWidth=TRUE,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        ch_mod_type[ch_mod_type==input$chk_mod[3]] %>% names())
    )
    
    
    
    ## Hyperparameter UI--------------------
    ### Conditionally display model-specific checkboxes
    observeEvent(input$rad_mod, {
      updateCheckboxGroupInput(inputId="chk_hyper",
                               label="Choose which hyperparameters to tune",
                               #conditionally populate choices arg
                               choices=if(input$rad_mod=="log_reg") {
                                 c("penalty", "mixture")
                               } else if(input$rad_mod=="dec_tree") {
                                 c("tree_depth", "min_n", "cost_complexity")
                               } else if(input$rad_mod=="forest") {
                                 c("mtry", "trees", "min_n")
                               })
    })
                    
    
    ### Conditionally display slider
    output$ui_slid_hyper <- renderUI({
      req(input$chk_hyper) 
      # req(length(input$chk_hyper) > 0)
      
      sliderInput(ns("slid_hyper"), 
                  label="Choose number of levels for each hyperparameter",
                  value=3, min=2, max=4)
    })
    
    
    ### Confirmation button for tuning
    output$ui_btn_confirm <- renderUI({
      req(input$chk_hyper)
      # req(length(input$chk_hyper) > 0)
      
      actionButton(ns("btn_confirm"), 
                   label="Confirm tuning selection",
                   class="btn-primary")
    })
    
    
    ## Reactives associated with selected model type--------------------
    ### Create model specification
    tune_spec_mod <- reactive({
      #at least one hyperparameter box checked
      req(input$chk_hyper)
      
      log_param <- c("penalty", "mixture")
      tree_param <- c("tree_depth", "min_n", "cost_complexity")
      forest_param <- c("mtry", "trees", "min_n")
    
      
      if(input$rad_mod=="log_reg"){
        log_reg_mod <- if(all(str_detect(input$chk_hyper, log_param))) {
          logistic_reg(
            penalty=tune(), 
            mixture=tune()
          )
        } else if(input$chk_hyper=="penalty") {
          logistic_reg(
            penalty=tune()
          )
        } else if(input$chk_hyper=="mixture") {
          logistic_reg(
            penalty=tune(),
            mixture=tune()
          )
        }
        
        log_reg_mod %>%
          set_engine("glmnet") %>%
          set_mode("classification") %>%
          translate()
      }
      
      else if(input$rad_mod=="dec_tree"){
        dec_tree_mod <- if(all(str_detect(input$chk_hyper, tree_param))) {
          decision_tree(
            tree_depth=tune(), 
            min_n=tune(),
            cost_complexity=tune()
          )
        } else if(all(str_detect(input$chk_hyper, tree_param[-3]))) {
          decision_tree(
            tree_depth=tune(), 
            min_n=tune()
          )
        } else if(all(str_detect(input$chk_hyper, tree_param[-2]))) {
          decision_tree(
            tree_depth=tune(),
            cost_complexity=tune()
          )
        } else if(all(str_detect(input$chk_hyper, tree_param[-1]))) {
          decision_tree(
            min_n=tune(),
            cost_complexity=tune()
          )
        } else if(input$chk_hyper=="tree_depth") {
          tree_depth=tune()
        } else if(input$chk_hyper=="min_n") {
          min_n=tune()
        } else if(input$chk_hyper=="cost_complexity") {
          cost_complexity=tune()
        }
        
        dec_tree_mod %>%
          set_engine("rpart") %>%
          set_mode("classification") %>%
          translate()
      }
        
      else if(input$rad_mod=="forest") {
        rand_forest_mod <- if(all(str_detect(input$chk_hyper, forest_param))) {
          rand_forest(
            mtry=tune(), 
            trees=tune(),
            min_n=tune()
          )
        } else if(all(str_detect(input$chk_hyper, forest_param[-3]))) {
          rand_forest(
            mtry=tune(), 
            trees=tune()
          )
        } else if(all(str_detect(input$chk_hyper, forest_param[-2]))) {
          rand_forest(
            mtry=tune(),
            min_n=tune()
          )
        } else if(all(str_detect(input$chk_hyper, forest_param[-1]))) {
          rand_forest(
            trees=tune(),
            min_n=tune()
          )
        } else if(input$chk_hyper=="mtry") {
          mtry=tune()
        } else if(input$chk_hyper=="trees") {
          trees=tune()
        } else if(input$chk_hyper=="min_n") {
          min_n=tune()
        }
        
        rand_forest_mod %>%
          set_engine("ranger") %>%
          set_mode("classification") %>%
          translate()
      }
    })
        
    
    
    ### Create grid of hyperparameters
    hyper_levels <- reactive({
      req(input$chk_hyper)
      req(input$slid_hyper)
      
      log_param <- c("penalty", "mixture")
      tree_param <- c("tree_depth", "min_n", "cost_complexity")
      forest_param <- c("mtry", "trees", "min_n")
      
      
      param <- if(input$rad_mod=="log_reg") {
        #if both selected or just mixture, then use both parameters
        if(all(str_detect(input$chk_hyper, log_param))|input$chk_hyper=="mixture") {
          parameters(penalty(), mixture())
          #otherwise if just penalty, then use only penalty
        } else if(input$chk_hyper=="penalty") {
          parameters(penalty())
        }
        
      } else if(input$rad_mod=="dec_tree") {
          if(all(str_detect(input$chk_hyper, tree_param))) {
          parameters(tree_depth(), min_n(), cost_complexity())
        } else if(all(str_detect(input$chk_hyper, tree_param[-3]))) {
          parameters(tree_depth(), min_n())
        } else if(all(str_detect(input$chk_hyper, tree_param[-2]))) {
          parameters(tree_depth(), cost_complexity())
        } else if(all(str_detect(input$chk_hyper, tree_param[-1]))) {
          parameters(min_n(), cost_complexity())
        } else if(input$chk_hyper=="tree_depth") {
          parameters(tree_depth())
        } else if(input$chk_hyper=="min_n") {
          parameters(min_n())
        } else if(input$chk_hyper=="cost_complexity") {
          parameters(cost_complexity())
        }
          
        } else if(input$rad_mod=="forest") {
          if(all(str_detect(input$chk_hyper, forest_param))) {
          parameters(mtry(), trees(), min_n())
        } else if(all(str_detect(input$chk_hyper, forest_param[-3]))) {
          parameters(mtry(), trees())
        } else if(all(str_detect(input$chk_hyper, forest_param[-2]))) {
          parameters(mtry(), min_n())
        } else if(all(str_detect(input$chk_hyper, forest_param[-1]))) {
          parameters(trees(), min_n())
        } else if(input$chk_hyper=="mtry") {
          parameters(mtry())
        } else if(input$chk_hyper=="trees") {
          parameters(trees())
        } else if(input$chk_hyper=="min_n") {
          parameters(min_n())
        }
      }
      
      #conditionally create grid
      if(input$chk_hyper=="mixture") {
        grid_regular(param,
                     filter = penalty < 0.01,
                     levels = c(penalty=1, mixture=input$slid_hyper))
      } else{grid_regular(param, levels=input$slid_hyper)}
    })
    
    
    ### Construct workflow
    tune_wf <- reactive({
      req(hyper_levels())
      
      workflow() %>%
        add_model(tune_spec_mod()) %>% #new model specification
        add_formula(form()) 
    })
    
      
    ### Model tuning with a grid
    tune_grid_mod <- reactive({
      req(tune_wf()) 
      
      tune_wf() %>%
        tune_grid(
          resamples=df_vfold(),
          grid=hyper_levels()
        ) 
    })
    
    
    #NOTE: LOGISTIC REGRESSION ONLY#
    ## Reactives associated with selected model type
    ### Create model specification
    # tune_spec_log <- reactive({
    #   #at least one hyperparameter box checked
    #   req(input$chk_hyper)
    # 
    #   log_reg_mod <- if(all(str_detect(input$chk_hyper, c("penalty", "mixture")))) {
    #     logistic_reg(
    #       penalty=tune(), 
    #       mixture=tune()
    #     )
    #   } else if(str_detect(input$chk_hyper, "penalty")) {
    #     logistic_reg(
    #       penalty=tune()
    #     )
    #   } else if(str_detect(input$chk_hyper, "mixture")) {
    #     logistic_reg(
    #       mixture=tune()
    #     )
    #   }
    #   
    #   log_reg_mod %>%
    #     set_engine("glmnet") %>%
    #     set_mode("classification") %>%
    #     translate()
    # })
    # 
    # 
    # ### Create grid of hyperparameters
    # log_hyper_levels <- reactive({
    #   req(input$chk_hyper)
    #   req(input$slid_hyper)
    #   
    #   param <- if(all(str_detect(input$chk_hyper, c("penalty", "mixture")))) {
    #     parameters(penalty(), mixture())
    #   } else if(str_detect(input$chk_hyper, "penalty")) {
    #     parameters(penalty())
    #   } else if(str_detect(input$chk_hyper, "mixture")) {
    #     parameters(mixture())
    #   }
    #   
    #   grid_regular(param, levels=input$slid_hyper)
    #   
    # })
    # 
    # 
    # ### Construct workflow
    # log_wf <- reactive({
    #   req(log_hyper_levels())
    #   
    #   workflow() %>%
    #     add_model(tune_spec_log()) %>% #new model specification
    #     add_formula(form()) 
    # })
    # 
    # 
    # ### Model tuning with a grid
    # log_tune <- reactive({
    #   req(log_wf()) 
    #   
    #   log_wf() %>%
    #     tune_grid(
    #       resamples=df_vfold(),
    #       grid=log_hyper_levels()
    #     ) 
    # })
    
    
    
    ## Hyperparameter Output--------------------
    ### Display visualizations
    #### Tuned model metrics
    output$tab_mod_tune <- renderDT(
      assess_model(tune_grid_mod(), simple=FALSE),
      rownames=FALSE, 
      options=list(pageLength=9, dom="tip",
                   autoWidth=TRUE,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        "Model Tuning Metrics")
        # ch_mod_type[ch_mod_type==input$chk_mod[1]] %>% names())
    )
    
    
    
    ### Display figures
    
    
    # ## Export--------------------
    # df_vfold <- eventReactive(input$btn_confirm, {
    #   df_vfold_tmp
    # })
    # 
    # 
    # #check code
    # observeEvent(input$btn_confirm, {
    # 
    #   print(str(df_vfold()))
    # })
    # 
    # 
    # 
    # ## Return DF--------------------
    # return(df_vfold)

  })
}



