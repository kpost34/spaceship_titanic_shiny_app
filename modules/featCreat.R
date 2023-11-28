#Feature Engineering-Feature Creation Module

# UI================================================================================================
featCreatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Creation",id="creFea04",
    titlePanel(title="Feature Creation"),
    h4("Now you have the opportunity to create new features for your model using the existing variables. Let's look
       at some possible options"),
    #inputs
    wellPanel(
      fluidRow(
        column(5,
          #select input for group size
          selectInput01(ID=ns("sel_var_group_size"), 
                        label="Create a group size variable that uses...",
                        choices=ch_grp_size_featCreat)
        ),
        column(2,
          linebreaks(5),    
          uiOutput(ns("ui_btn_creFea_complete"))
        ),
        column(5,
          #select input for luxury expenses
          selectizeInput(inputId=ns("sel_var_lux_expense"),
                         label="Create a luxury expense variable that uses the sum of",
                         multiple=TRUE,
                         choices=c(
                           "Choose at least two variables"="",
                           ch_lux_featCreat)),
        )
        # column(2,
        #   actionButton(inputId=ns("btn_exp2_creFea04"),label="Visualize results") #remove?
        # )
      )
      #vertically aligns button with selectizeInput
      # tags$style(type="text/css", "#btn_exp2_creFea04 {width: 100%; margin-top: 25px;}")
    ),
    #outputs
    fluidRow(
      column(6,
        # plotOutput(ns("plot_sel_exp1a_creFea04")),
        plotOutput(ns("plot_var_group_size"))
      ),
      column(6,
        plotOutput(ns("plot_lux_expense_heatmap")),
        br(),
        plotOutput(ns("plot_lux_expense_boxplot")),
        #temporary table--previews data after confirming selections
        tableOutput(ns("plot_temp_table_creFea04"))
      )
    )
  )
}

# Server============================================================================================
featCreatServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Outputs--------------------
    ### Group size variable
    #### Create a reactive data frame based on input
    df_group_size <- reactive({
      req(input$sel_var_group_size)
      
      switch(input$sel_var_group_size,
             ticket_group_size=df_train_nvI() %>%
              group_by(passenger_group) %>%
              mutate(ticket_group_size=n(),
                    ticket_group_size=as.factor(ticket_group_size)) %>%
               ungroup(),
             family_size=df_train_nvI() %>%
               group_by(passenger_group,l_name) %>%
               mutate(family_size=n(),
                      family_size=as.factor(family_size)) %>%
               ungroup(),
             travel_party_size=df_train_nvI() %>%
               group_by(cabin) %>% 
               mutate(travel_party_size=n(),
                      travel_party_size=as.factor(travel_party_size)) %>%
               ungroup()
        )
    })
    
    #### Make bar plot using new df
    # output$plot_sel_exp1a_creFea04 <- renderPlot({
    #   #require that user does not select "none" to get plots
    #   req(input$sel_var_group_size %in% ch_grp_size_featCreat[ch_grp_size_featCreat!="none"])
    #   
    #   df_group_size() %>%
    #     barplotter(input$sel_var_group_size)
    # })
  
    output$plot_var_group_size <- renderPlot({
      #require that user does not select "none" to get plots
      req(input$sel_var_group_size %in% ch_grp_size_featCreat[ch_grp_size_featCreat!="none"])
      
      df_group_size() %>%
        barplotter2(var=input$sel_var_group_size)
    })
    
    
    ### Luxury expense variable
    #### Create reactive df for plotting and feature creation
    df_lux_expense <- reactive({
      req(input$sel_var_lux_expense %in% ch_lux_featCreat[ch_lux_featCreat!="none"])
      
      lux_builder(df_train_nvI(), input$sel_var_lux_expense)
    })
    
    
    #### Display plots after action button depressed
    observeEvent(input$btn_exp2_creFea04, {
      output$plot_lux_expense_heatmap <- renderPlot({
        heatmapper(df_lux_expense(), input$sel_var_lux_expense) 
      })
      
      output$plot_lux_expense_boxplot <- renderPlot({
        boxplotter2(df_lux_expense())
      })
    })
    
    
    ## Feature creation--------------------
    ### Display action button to confirm selections dynamically
    output$ui_btn_creFea_complete <- renderUI({
      req(input$sel_var_group_size)
      req(input$sel_var_lux_expense=="none"|length(input$sel_var_lux_expense)>=2)
      req(input$btn_exp2_creFea04)
      
      actionButton(inputId=ns("btn_creFea_complete"), "Confirm feature creation selections")
    })
    
    ### Create features
    df_train_nvI_eF <- eventReactive(input$btn_creFea_complete,{
      df_group_size() %>%
        select(passenger_id,ends_with("size")) %>%
        left_join(df_lux_expense() %>%
                    select(passenger_id,luxury),by="passenger_id")
    }
    )
    
    ### Checking
    output$plot_temp_table_creFea04<-renderTable({
      head(df_train_nvI_eF())
    })
    
    
    ## Export--------------------
    return(df_train_nvI_eF)
    
    
  })
}



















