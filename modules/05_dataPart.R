#Data Partitioning Module

# UI-===============================================================================================
dataPartUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Data Partitioning",
    titlePanel("Data Partitioning"),
    sidebarLayout(
      ## Input sliders for v-fold cross-validation with confirm button
      sidebarPanel(width=2,
        sliderInput(ns("slid_v"), label="Number of partitions", min=5, max=10, value=5),
        linebreaks(2),
        sliderInput(ns("slid_repeats"), label="Number of repeats", min=1, max=5, value=3),
        uiOutput(ns("ui_btn_confirm")),
        linebreaks(3),
        sliderInput(ns("slid_index"), label="Select a split index to preview", 
                    min=1, max=5, value=1, step=1)
      ),
      
      ## Tabular outputs
      mainPanel(width=10,
        DTOutput(ns("tab_analysis")),
        br(),
        DTOutput(ns("tab_assessment"))
      )
    )
  )
}


# Server============================================================================================
dataPartServer <- function(id, df_train_select) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Create reactives--------------------
    ### Apply data partitioning to DF
    df_vfold_tmp <- reactive({df_train_select() %>%
      vfold_cv(v=input$slid_v, repeats=input$slid_repeats, strat=transported)
    })
    
    
    ### Max index (number of splits * number of repeats)
    max_index <- reactive({
      input$slid_v * input$slid_repeats
    })
    
    
    ### Sample split
    selected_index <- reactive({
      get_rsplit(df_vfold_tmp(), index=input$slid_index)
    })
    
    
    
    ## Generate UI--------------------
    ### Confirm button
    output$ui_btn_confirm <- renderUI({
      actionButton(ns("btn_confirm"), label="Confirm above \nselections")
    })
    
    
    
    ## Generate tables--------------------
    ### Analysis
    output$tab_analysis <- renderDT(
      analysis(selected_index()) %>%
        mutate(across(where(is.numeric), ~signif(.x, 3))) %>%
        filter(row_number() %in% 1:50),
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        "Analysis")
    )
    
    
    ### Assessment
    output$tab_assessment <- renderDT(
      assessment(selected_index()) %>%
        mutate(across(where(is.numeric), ~signif(.x, 3))) %>%
        filter(row_number() %in% 1:50),
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        "Assessment")
    )

    
    
    ## Export--------------------
    df_vfold <- eventReactive(input$btn_confirm, {
      df_vfold_tmp()
    })
    
    
    #check code
    observeEvent(input$btn_confirm, {

      print(df_vfold())
    })
    
   
   
    ## Return DF--------------------
    return(df_vfold)

  })
}
