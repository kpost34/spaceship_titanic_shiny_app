#Feature Engineering-Transformation Module: Standardization Submodule

# UI================================================================================================
featTrans_scaleUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("ui_sel_scale1")),
      br(),
      uiOutput(ns("ui_sel_scale2")),
      uiOutput(ns("ui_btn_scale"))
    ),
    mainPanel(
      plotOutput(ns("plot_sel_scale1"),height="1000px"),
      tableOutput(ns("DT1"))
    )
  )
}



# Server============================================================================================
featTrans_scaleServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
   
    
    ## Inputs
    ### Input to select var to visualize, either unscaled or scaled
    output$ui_sel_scale1<-renderUI({
      selectInput01(ID=ns("sel_scale1"),label=varViz_feat,
                    #dynamically select numeric vars (NOTE: will need to update data object later)
                    choices=df_train_nvI() %>% select(where(is.numeric)) %>% names())
    })
    
    
    ### Input to select how to transform/scale selected variables
    output$ui_sel_scale2<-renderUI({
      req(input$sel_scale1)
      selectInput01(ID=ns("sel_scale2"),label=scaleOpt_feat,
                    choices=ch_trans_opt_featTrans)
    })
    
    ### Button to confirm selection
    output$ui_btn_scale<-renderUI({
      req(input$sel_scale1,input$sel_scale2)
      actionButton(inputId=ns("btn_scale"),label="Confirm your selection") 
    })
    
    ### Button to confirm scaling selections and create new columns/variables
    df_train_nvI_s<-eventReactive(input$btn_scale, {
      switch(input$sel_scale2,
             #raw = unchanged
             raw=df_train_nvI(),
             #log = log-transform + identifier
             log=df_train_nvI() %>% 
               mutate(across(where(is.numeric),~log(.x + 1),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")),
             #mm_scale = min-max scale + identifier
             mm_scale=df_train_nvI() %>%
               mutate(across(where(is.numeric),~min_max_scaler(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")), 
             #standize = standardized + identifier
             standize=df_train_nvI() %>%
               mutate(across(where(is.numeric),~standardizer(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")) 
      )
    })
    
    #Temporary table--proof that above code is working
    output$DT1<-renderTable({
      head(df_train_nvI_s())
    })
    
    
    ## Output
    ### Display set of plots
    output$plot_sel_scale1<-renderPlot({
      req(input$sel_scale1)
      cowplotter(df_train_nvI(),input$sel_scale1)
    })
    
  })
}





