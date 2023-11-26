#Feature Engineering-Transformations Module

# UI================================================================================================
featTrans_mainUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Transformations",
    titlePanel("Feature Scaling and Extraction"),
    
    h4("In this section, you will have the opportuntiy to normalize/standardize numerical data, bin 
       numerical (or character or factor variables) into (smaller) groups,perform ordinal encoding, 
       and group rare categories together. What would you like to begin with?"),
    fluidRow(
      column(6,align="center",
        radioButtons(inputId=ns("rad_trans"),label="",choices=ch_trans_featTrans,selected=character(0),
                     inline=TRUE,width="100%")
      ),
      column(6, align="center",
        #placeholder
        uiOutput(ns("ui_btn_trans_complete"))
      )
    ),
    
    #UIs of each submodule
    tabsetPanel(id=ns("featTrans_tab"), type="hidden",
      tabPanelBody("blank"),
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
featTrans_mainServer <- function(id, df_train_nvI, df_train_nvI_s, df_train_nvI_d, df_train_nvI_o, df_train_nvI_r) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
  

    # Conditionally display tabs
    observeEvent(input$rad_trans, {
      updateTabsetPanel(inputId="featTrans_tab",selected=input$rad_trans)
    })
    
    output$ui_btn_trans_complete <- renderUI({
      req(df_train_nvI_s(), df_train_nvI_d(), df_train_nvI_o(), df_train_nvI_r())
      
      actionButton(ns("btn_trans_complete"), "All transformations complete")
    })
    
    # Source/run server submodules
    df_train_nvI_s <- featTrans_scaleServer("df1", df_train_nvI)
    df_train_nvI_d <- featTrans_disServer("df2", df_train_nvI)
    df_train_nvI_o <- featTrans_ordEncServer("df3", df_train_nvI)
    df_train_nvI_r <- featTrans_rareEncServer("df4", df_train_nvI)
    
    # Join DFs
    df_train_nvI_t <- eventReactive(input$btn_trans_complete, {
      
      df_train_nvI_s() %>%
        left_join(df_train_nvI_d()) %>%
        left_join(df_train_nvI_o()) %>%
        left_join(df_train_nvI_r())
      
    })
    
    # Return DF
    return(df_train_nvI_t)
    
  })

}

