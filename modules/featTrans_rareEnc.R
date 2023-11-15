#Feature Engineering-Transformation Module: Rare Label Encoding Submodule


# UI================================================================================================
featTrans_rareEncUI <- function(id) {
  ns <- NS(id)
  
  # tabPanelBody("Rare Label Encoding",
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("ui_sel_rareEnc1a")),
        uiOutput(ns("ui_sel_rareEnc1b")),
        linebreaks(5),
        uiOutput(ns("ui_sel_rareEnc2a")),
        uiOutput(ns("ui_sel_rareEnc2b")),
        uiOutput(ns("ui_btn_rareEnc"))
      ),
      mainPanel(
        fluidRow(
          column(6,
            plotOutput(ns("plot_sel_rareEnc1a"),height="250px")
          ),
          column(6,
            plotOutput(ns("plot_sel_rareEnc1b"),height="250px")
          )
        ),
        linebreaks(2),
        fluidRow(
          column (6,
            plotOutput(ns("plot_sel_rareEnc2a"),height="250px")
          ),
          column(6,
            plotOutput(ns("plot_sel_rareEnc2b"),height="250px"),
            tableOutput(ns("temp_table_rareEnc")))
        )
      )
    )
  # )
}



# Server============================================================================================
featTrans_rareEncServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Inputs
    ### Input to select var to visualize as a barplot
    output$ui_sel_rareEnc1a<-renderUI({
      selectInput01(ID=ns("sel_rareEnc1a"),label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=df_train_nvI() %>% select(deck,ticket) %>% names())
    })
    
    ### Input to select levels to combine as a category and visualize in a new barplot (NAs are off limits)
    output$ui_sel_rareEnc1b<-renderUI({
      req(input$sel_rareEnc1a)
      selectizeInput(inputId=ns("sel_rareEnc1b"),label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               df_train_nvI() %>% 
                                 pull(input$sel_rareEnc1a) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Input to select other var to visualize as a barplot
    output$ui_sel_rareEnc2a<-renderUI({
      req(length(input$sel_rareEnc1b)>1)
      selectInput01(ID=ns("sel_rareEnc2a"),label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=df_train_nvI() %>% select(deck,ticket,-input$sel_rareEnc1a) %>% names())
    })
    
    #### Input to select levels to combine as a category and visualize in a new barplot
    output$ui_sel_rareEnc2b<-renderUI({
      req(input$sel_rareEnc2a)
      selectizeInput(inputId=ns("sel_rareEnc2b"),label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               df_train_nvI() %>% 
                                 pull(input$sel_rareEnc2a) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Button to confirm selections
    output$ui_btn_rareEnc<-renderUI({
      req(input$sel_rareEnc1a)
      actionButton(inputId=ns("btn_rareEnc"),label="Confirm selections")
    })
  
  
    ## Display plots
    #var1-raw
    output$plot_sel_rareEnc1a<-renderPlot({
      req(input$sel_rareEnc1a)
      rare_enc_barplotter(df_train_nvI(),input$sel_rareEnc1a)
    })
    
    #var1-combined categories
    output$plot_sel_rareEnc1b<-renderPlot({
      req(length(input$sel_rareEnc1b)>1)
      rare_enc_barplotter(df_train_nvI(),var=input$sel_rareEnc1a,cats=input$sel_rareEnc1b)
    })
    
    #var2-raw
    output$plot_sel_rareEnc2a<-renderPlot({
      req(input$sel_rareEnc2a)
      rare_enc_barplotter(df_train_nvI(),input$sel_rareEnc2a)
    })
    
    #var2-combined categories
    output$plot_sel_rareEnc2b<-renderPlot({
      req(length(input$sel_rareEnc2b)>1)
      rare_enc_barplotter(df_train_nvI(),var=input$sel_rareEnc2a,cats=input$sel_rareEnc2b)
    })
    
    
    ## Extract features via rare label encoding
    df_train_nvI_r<-eventReactive(input$btn_rareEnc, {
      df_train_nvI() %>%
        {if(length(input$sel_rareEnc1b)>=2) 
          #paste variable name using !! and :=
          mutate(.,!!paste0(input$sel_rareEnc1a,"_rare") := fct_collapse(!!sym(input$sel_rareEnc1a),
                                                                                other=input$sel_rareEnc1b))
          else .} %>%
        {if(length(input$sel_rareEnc2b)>=2)
          mutate(.,!!paste0(input$sel_rareEnc2a,"_rare") := fct_collapse(!!sym(input$sel_rareEnc2a),
                                                                                other=input$sel_rareEnc2b))
          else .} %>%
        #retain cols of interest
        select(passenger_id,ends_with("_rare")) 
    })
    
    
    ## Output temp table
    output$temp_table_rareEnc<-renderTable({
      df_train_nvI_r() %>% head()
    })
  
    # ### Update data frame
    # df_train_nvI_eF<-reactive({
    #   req(input$chk_tranFea04)
    #   #insert joins here
    # })
    
  })
}