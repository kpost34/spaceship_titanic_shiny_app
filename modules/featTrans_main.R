#Feature Engineering-Transformations Module

# UI================================================================================================
featTrans_mainUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Transformations",
    titlePanel("Feature Scaling and Extraction"),
    h4("In this section, you will have the opportuntiy to normalize/standardize numerical data, bin numerical (or 
      character or factor variables) into (smaller) groups,perform ordinal encoding, and group rare categories
      together. What would you like to begin with?"),
    #fluidRow with column helps to align radio buttons
    fluidRow(
      column(6,align="center",offset=3,
        radioButtons(inputId=ns("rad_trans"),label="",choices=ch_trans_featTrans,selected=character(0),
                     inline=TRUE,width="100%"),
        uiOutput("ui_chk_trans") #is this necessary?
      )
    ),
    tabsetPanel(id=ns("featTrans_tab"), type="hidden",
      tabPanelBody("Feature Scaling",
        featTrans_scaleUI(ns("df1"))
      ),
      tabPanelBody("Discretization",
        featTrans_disUI(ns("df2"))
      ),
      tabPanelBody("Ordinal Encoding",
        featTrans_ordEncUI(ns("df3"))
      ),
      tabPanelBody("Rare Label Encoding",
        featTrans_rareEncUI(ns("df4"))
      )
    )
  )
}


# Server============================================================================================
featTrans_mainServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    # ns <- session$ns
    
    
  #   output$ui_chk_trans <- renderUI({   
  #   #update req() statement--should reflect that all four confirmations selected
  #   req(input$rad_trans)
  #   checkboxInput(inputId=ns("chk_trns"),label="CONFIRM ALL DATA TRANSFORMATIONS SELECTED",value=FALSE)
  # })
  
    ## Conditional tabsets----------------------------------------
    ### Conditional UI for displaying tab of tabset panel
    observeEvent(input$rad_trans, {
      updateTabsetPanel(inputId="featTrans_tab",selected=input$rad_trans)
    })
    
    
    ### Conditional UI for displaying main tabset panel for larger categories
    # observeEvent(input$rad_trans, {
    #   updateTabsetPanel(inputId="main_tab",selected=input$rad_trans)
    # })
    
    #source/run server submodules
    featTrans_scaleServer("df1", df_train_nvI)
    featTrans_disServer("df2", df_train_nvI)
    featTrans_ordEncServer("df3", df_train_nvI)
    featTrans_rareEncServer("df4", df_train_nvI)
    
    
    
  })

}

