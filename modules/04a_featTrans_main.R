#Feature Engineering-Transformations Module

# UI================================================================================================
featTrans_mainUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Transformations",
    titlePanel("Feature Scaling and Extraction"),
    
    HTML("<h4>In this section, you will have the opportuntiy to normalize/standardize numerical data,  
       (<em style='color:blue;'>feature scaling</em>), bin numerical variables into groups 
       (<em style='color:blue;'>discretization</em>), convert categorical variables into ordered 
       factors (<em style='color:blue;'>ordinal encoding</em>), and combine infrequent groups of 
       factor variables (<em style='color:blue;'>rare label encoding</em>). All four transformations 
       must be completed before proceeding to feature creation. Please select an option.</h4>"),
    
    fluidRow(
      column(6,align="center",
        radioButtons(inputId=ns("rad_trans"), label="", selected=character(0), 
                     inline=TRUE, width="100%",
                     choices=ch_trans_featTrans)
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
featTrans_mainServer <- function(id, df_train_nd_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
  
    ## Conditionally display tabs--------------------
    ### Select transformation type
    observeEvent(input$rad_trans, {
      updateTabsetPanel(inputId="featTrans_tab",selected=input$rad_trans)
    })
    
    
    ## Conditionally display button that all transformations complete
    output$ui_btn_trans_complete <- renderUI({
      req(df_train_nd_nvI_s(), df_train_nd_nvI_d(), df_train_nd_nvI_o(), df_train_nd_nvI_r())
      
      actionButton(ns("btn_trans_complete"), 
                   label=HTML("<b>Confirm all transformations</b>"), 
                   style="width: 300px; height=150px",
                   class="btn-info")
    })
    
    
    
    ## Source/run server submodules--------------------
    df_train_nd_nvI_s <- featTrans_scaleServer("df1", df_train_nd_nvI)
    df_train_nd_nvI_d <- featTrans_disServer("df2", df_train_nd_nvI)
    df_train_nd_nvI_o <- featTrans_ordEncServer("df3", df_train_nd_nvI)
    df_train_nd_nvI_r <- featTrans_rareEncServer("df4", df_train_nd_nvI)
    
    
    
    ## Confirmation button selected--------------------
    ### Trigger toast notification
    observeEvent(input$btn_trans_complete, {
      show_toast(
        title="Feature transformations",
        type="success",
        text="All transformations completed...please proceed to feature creation",
        position="center",
        timer=3000
      )
    })
    
    
    ### Join DFs
    df_train_nd_nvI_tF <- eventReactive(input$btn_trans_complete, {
      
      df_train_nd_nvI_s() %>%
        left_join(df_train_nd_nvI_d()) %>%
        left_join(df_train_nd_nvI_o()) %>%
        left_join(df_train_nd_nvI_r())
      
    })
    
    
    
    ## Return DF--------------------
    return(df_train_nd_nvI_tF)
    
  })

}

