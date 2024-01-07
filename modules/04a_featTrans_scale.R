#Feature Engineering-Transformation Module: Standardization Submodule

# UI================================================================================================
featTrans_scaleUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput01(ID=ns("sel_var_viz"), label=varViz_feat,
                    choices=numVars),
      br(),
      uiOutput(ns("ui_type_scale")),
      uiOutput(ns("ui_btn_scale_complete")),
      strong(textOutput(ns("text_btn_scale_complete")))
    ),
    mainPanel(width=9,
      plotOutput(ns("plot_sel_var_viz"), height="1000px") %>%
        withSpinner(),
      tableOutput(ns("temp_table"))
    )
  )
}



# Server============================================================================================
featTrans_scaleServer <- function(id, df_train_nd_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
   
    ## Inputs--------------------
    ### Input to select how to transform/scale selected variables
    output$ui_type_scale<-renderUI({
      req(input$sel_var_viz)
      
      selectInput01(ID=ns("type_scale"),
                    label=scaleOpt_feat,
                    choices=ch_trans_opt_featTrans)
    })
    
    ### Button to confirm selection
    output$ui_btn_scale_complete <- renderUI({
      req(input$sel_var_viz, input$type_scale)
      actionButton(inputId=ns("btn_scale_complete"), label="Confirm your selection", 
                   class="btn-success") 
    })
  
    
    
    ## Output--------------------
    ### Display set of plots
    output$plot_sel_var_viz<-renderPlot({
      req(input$sel_var_viz)
      
      cowplotter(df_train_nd_nvI(), input$sel_var_viz)
    })
    
    
    
    ## Export--------------------
    ### Create new DF with scaled columns/variables
    df_train_nd_nvI_s<-eventReactive(input$btn_scale_complete, {
      switch(input$type_scale,
             #raw = unchanged
             raw=df_train_nd_nvI(),
             #log = log-transform + identifier
             log=df_train_nd_nvI() %>% 
               mutate(across(where(is.numeric),~log(.x + 1),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")),
             #mm_scale = min-max scale + identifier
             mm_scale=df_train_nd_nvI() %>%
               mutate(across(where(is.numeric),~min_max_scaler(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")), 
             #standize = standardized + identifier
             standize=df_train_nd_nvI() %>%
               mutate(across(where(is.numeric),~standardizer(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")) 
      )
    })
    
    ### User feedback: display text of transformation selected
    output$text_btn_scale_complete <- renderText({
      req(input$type_scale)
      req(input$btn_scale_complete)
      confirm_scaling_msg(input$type_scale)
    })
      

                                      
    
    ### Temporary table--proof that above code is working
    output$temp_table <- renderTable({
      head(df_train_nd_nvI_s())
    })
    
    return(df_train_nd_nvI_s)
    
  })
}





