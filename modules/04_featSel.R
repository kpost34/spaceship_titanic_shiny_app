#Feature Engineering-Feature Selection Module

# UI-===============================================================================================
featSelUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Selection",
    titlePanel(title="Feature Selection"),
    h4("After transforming your data, extracting potential variables, and creating potential variables, you have
      the opportunity to select a final set of variables for modeling. Look at the variables once more before
      making your final decision"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("ui_sel_var_mod")
       #CONSIDER A FUNCTION HERE??
        #a simple selector of all variables & a visualization that is appropriate for that selector
          #it's going to be a simple bivariate with the var selected and transported
        #another selector, which is used via a renderUI/uiOutput where the user can then select
          #the final set of variables (passenger_id and transported will not be options as they
          #are going to be included regardless)
          #when a user selects one var that's heavily related to others, those vars will drop
            #out of the pool
      ),
      mainPanel(
      )
    )
  )
}


# Server============================================================================================
featSelServer <- function(id, df_train_nvI, df_train_nvI_tF, df_train_nvI_cF) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Inputs--------------------
    ## Create selector
    #reactive of vars
    # vars_avail <- reactiveVal(NA)
    
    ch_vars <- reactive({
      df_train_nvI() %>%
        left_join(df_train_nvI_tF()) %>%
        left_join(df_train_nvI_cF()) %>%
        select(-c(passenger_id, passenger_group, cabin, num, name, transported)) %>%
        names() 
    })
  
  # vars_avail(ch_vars())
    
    #selector
    output$ui_sel_var_mod <- renderUI({
      selectizeInput(ns("sel_var_mod"), multiple=TRUE, 
                     choices=c("Choose as many variable as you prefer"="", ch_vars()))
    })
    
    
    
  
  
  })
}



