#Modelling-Model Selection Module

# UI-===============================================================================================
modSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Model Selection",
    titlePanel("Model Selection"),
    sidebarLayout(
      sidebarPanel(width=2,
        checkboxGroupInput(ns("chk_mod"), label="Please select one or more model types for assessment",
                           choices=ch_mod_type),
        linebreaks(2),
        h4("Select a model type for tuning"),
        # splitLayout(
          actionButton(ns("btn_log"), "Logistic Regression"),
          actionButton(ns("btn_tree"), "Decision Trees"),
          actionButton(ns("btn_forest"), "Random Forest"),
        # ),
        linebreaks(2),
        h4("Choose which hyperparameters to tune"), #included in label
        uiOutput(ns("ui_chk_hyper")),
        h4("Select hyperparameter values"), #included in label
        
        uiOutput(ns("ui_btn_confirm")),
      ),
      
      ## Tabular outputs
      mainPanel(width=10,
        fluidRow(
          column(3,
        # splitLayout(
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
        DTOutput(ns("tab_tune"))
      )
    )
  )
}


# Server============================================================================================
modSelServer <- function(id, df_train_select, df_vfold) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Create reactives--------------------
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



