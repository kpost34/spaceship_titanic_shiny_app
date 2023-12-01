#Feature Engineering-Transformation Module: Rare Label Encoding Submodule


# UI================================================================================================
featTrans_rareEncUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width=2,
      selectInput01(ID=ns("sel_var_viz1"), label=varViz_feat,
                    choices=c("deck", "ticket")),
      uiOutput(ns("ui_sel_var_viz1")),
      uiOutput(ns("ui_sel_var_cat1")),
      linebreaks(5),
      uiOutput(ns("ui_sel_var_viz2")),
      uiOutput(ns("ui_sel_var_cat2")),
      uiOutput(ns("ui_btn_rareEnc_complete"))
    ),
    mainPanel(width=10,
      fluidRow(
        column(6,
          plotOutput(ns("plot_sel_var_viz1"),height="350px")
        ),
        column(6,
          plotOutput(ns("plot_sel_var_cat1"),height="350px")
        )
      ),
      linebreaks(2),
      fluidRow(
        column(6,
          plotOutput(ns("plot_sel_var_viz2"),height="350px")
        ),
        column(6,
          plotOutput(ns("plot_sel_var_cat2"),height="350px"),
          tableOutput(ns("temp_table_rareEnc")))
      )
    )
  )
}



# Server============================================================================================
featTrans_rareEncServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Inputs--------------------
    ### Input to select levels to combine as a category and visualize in a new barplot (NAs are off limits)
    output$ui_sel_var_cat1<-renderUI({
      req(input$sel_var_viz1)
      
      selectizeInput(inputId=ns("sel_var_cat1"),label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               df_train_nvI() %>% 
                                 pull(input$sel_var_viz1) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Input to select other var to visualize as a barplot
    output$ui_sel_var_viz2 <- renderUI({
      req(input$sel_var_viz1)
      # req(length(input$sel_var_cat1) > 1)
      
      selectInput01(ID=ns("sel_var_viz2"), label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=c("deck", "ticket") %>% .[.!=input$sel_var_viz1])
    })
    
    #### Input to select levels to combine as a category and visualize in a new barplot
    output$ui_sel_var_cat2 <- renderUI({
      req(input$sel_var_viz2)
      
      selectizeInput(inputId=ns("sel_var_cat2"),label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               df_train_nvI() %>% 
                                 pull(input$sel_var_viz2) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Button to confirm selections
    output$ui_btn_rareEnc_complete <- renderUI({
      req(input$sel_var_viz1)
      
      actionButton(inputId=ns("btn_rareEnc_complete"), label="Confirm selections",
                   class="btn-success")
    })
  
  
    ## Outputs--------------------
    #var1-raw
    output$plot_sel_var_viz1 <- renderPlot({
      req(input$sel_var_viz1)
      
      barplotter2(df_train_nvI(), input$sel_var_viz1)
    })
    
    #var1-combined categories
    output$plot_sel_var_cat1 <- renderPlot({
      req(length(input$sel_var_cat1) > 1)
      
      barplotter2(df_train_nvI(), var=input$sel_var_viz1, cats=input$sel_var_cat1)
    })
    
    #var2-raw
    output$plot_sel_var_viz2 <- renderPlot({
      req(input$sel_var_viz2)
      
      barplotter2(df_train_nvI(), var=input$sel_var_viz2, col="mako")
    })
    
    #var2-combined categories
    output$plot_sel_var_cat2 <- renderPlot({
      req(length(input$sel_var_cat2) > 1)
      
      barplotter2(df_train_nvI(), var=input$sel_var_viz2, cats=input$sel_var_cat2, col="mako")
    })
    
    
    ## Export--------------------
    ### Extract features via rare label encoding
    df_train_nvI_r <- eventReactive(input$btn_rareEnc_complete, {
      df_train_nvI() %>%
        {if(length(input$sel_var_cat1) >= 2) 
          #paste variable name using !! and :=
          mutate(.,!!paste0(input$sel_var_viz1,"_rare") := fct_collapse(!!sym(input$sel_var_viz1),
                                                                                other=input$sel_var_cat1))
          else .} %>%
        {if(length(input$sel_var_cat2) >= 2)
          mutate(.,!!paste0(input$sel_var_viz2,"_rare") := fct_collapse(!!sym(input$sel_var_viz2),
                                                                                other=input$sel_var_cat2))
          else .} %>%
        #retain cols of interest
        select(passenger_id, ends_with("_rare")) 
    })
    
    
    ### Output temp table
    output$temp_table_rareEnc<-renderTable({
      df_train_nvI_r() %>% head()
    })
    
    return(df_train_nvI_r)
    
  })
}