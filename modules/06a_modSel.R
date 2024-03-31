#Modelling-Model Selection Module

# UI-===============================================================================================
modSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Model Selection",
    titlePanel("Model Selection"),
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxGroupInput(ns("chk_mod"), label="Please select one or more model types for assessment",
                           choices=ch_mod_type),
        linebreaks(2),
        h4("Select a model type for tuning"),
        splitLayout(
          actionButton(ns("btn_log")),
          actionButton(ns("btn_tree")),
          actionButton(ns("btn_knn"))
        ),
        linebreaks(2),
        h4("Choose which hyperparameters to tune"), #included in label
        uiOutput(ns("ui_chk_hyper")),
        h4("Select hyperparameter values"), #included in label
        
        uiOutput(ns("ui_btn_confirm")),
      ),
      
      ## Tabular outputs
      mainPanel(width=9,
        splitLayout(
          DTOutput(ns("tab_mod1")),
          DTOutput(ns("tab_mod2")),
          DTOutput(ns("tab_mod3"))
        ),
        br(),
        DTOutput(ns("tab_tune"))
      )
    )
  )
}


# Server============================================================================================
modSelServer <- function(id, df_vfold) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Create reactives--------------------
    ### Extract formula
    form <- reactive({
      grab_formula(df_train_select)
    })
    
    
    ### Model fitting and assessment
    #first model selected
    fit_rs1 <- reactive({
      
      req(input$chk_mod[1])
      
      create_fit_model(type=input$chk_mod[1],
                       formula=form(),
                       folds=df_vfold())
    })
    
    #second model selected
    fit_rs2 <- reactive({
      
      req(input$chk_mod[2])
      
      create_fit_model(type=input$chk_mod[2],
                       formula=form(),
                       folds=df_vfold())
    })
    
    
    #third model selected
    fit_rs2 <- reactive({
      
      req(input$chk_mod[3])
      
      create_fit_model(type=input$chk_mod[3],
                       formula=form(),
                       folds=df_vfold())
    })
    
    
    ## Generate tables--------------------
    ### Model selection
    #first table
    output$tab_mod1 <- renderDT(
      collect_metrics(fit_rs1()),
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        input$chk_mod[1])
    )
    
    
    #second table
    output$tab_mod2 <- renderDT(
      collect_metrics(fit_rs2()),
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        input$chk_mod[2])
    )
    
    
    #third table
    output$tab_mod3 <- renderDT(
      collect_metrics(fit_rs3()),
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        input$chk_mod[3])
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



