# Missingness-Names Module


# UI================================================================================================
missNameUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Names",
    titlePanel("Names and Missingness"),
    sidebarLayout(
      sidebarPanel(
        #exploring missing names
        h4("Did you notice that some passengers did not have names? If not, take a closer look"),
          selectInput01(ID=ns("sel_exp"), label="",choices=namMis03_expVec),
          hr(style = "border-top: 1px solid #000000;"),
        
        #go deeper with some possibilities
        h4(str_missName1),
          HTML(str_missName2),
          radioButtons(inputId=ns("rad_grpVar"),
                       label=h4(str_missName2d),
                       choices=c("passenger_group"="passenger_group",
                                 "cabin occupancy"="cabin"),
                       selected=character(0)),
          hr(style = "border-top: 1px solid #000000;"),
        
        #options on missing names
        h4("Given all this information, how would you like to handle passengers with missing names?"),
          selectInput01(ID=ns("sel_impOpt"),
                        label="",
                        choices=namMis03_impOptVec),
          br(),
          uiOutput(ns("ui_slid_impOpt")),
        actionButton(ns("btn_impOpt"), "Submit")
      ),
      
      mainPanel(
        htmlOutput(ns("text_sel_exp")),
        DTOutput(ns("tab_sel_exp")),
        br(),
        htmlOutput(ns("text_rad_grpVar")),
        plotOutput(ns("plot_rad_grpVar")),
        # tableOutput(ns("test_table")),
      )
    )
  )
}





# Server============================================================================================
missNameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Exploring missing names--------------------
    ### Text outputs
    output$text_sel_exp <- renderUI({
      #here switch() can be used with all four choices to display the appropriate name
      switch(input$sel_exp,
        miss_samp=h3(paste("Sample of Passengers with Missing Names")),
        nmiss_samp=h3(paste("Sample of Passengers with Names")),
        sum_tab=h3(paste("Summary of Missing Names"))
      )
    })
    
    ### Create reactive object (for tabular output)
    dat1 <- reactive({
      #reactive is used to build reactive table objects
      switch(input$sel_exp,
        miss_samp=df_train %>% 
          select(all_of(vars_miss_exp)) %>% 
          filter(is.na(name)) %>% 
          slice_sample(n=5),
        nmiss_samp=df_train %>% 
          select(all_of(vars_miss_exp)) %>%
          filter(!is.na(name)) %>% 
          slice_sample(n=5),
        sum_tab=chr_miss_tabler(df_train)
      )
    })
    
  
    ### Output table/plot
    #### Table output
    output$tab_sel_exp <- renderDT(
      dat1(), 
      rownames=FALSE,
      options=list(dom="t",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all")))
    )
    
    
    ## Understanding name missingness conditioned on other variables--------------------
    ### Text outputs
    output$text_rad_grpVar <- renderUI({
      req(input$rad_grpVar)
      switch(input$rad_grpVar,
        passenger_group=h3(paste("Summary of Missing Names by Size of Passenger Groups")),
        cabin=h3(paste("Summary of Missing Names by Cabin Occupancy"))
      )
    })
    
    ### Create reactive object (for tabular and plot outputs)
    dat2 <- reactive({
      switch(input$rad_grpVar,
        passenger_group=mis_name_tabler(df_train,l_name,passenger_group),
        cabin=mis_name_tabler(df_train,l_name,cabin)
      )
    })
    
    
    ### Output plots
    output$plot_rad_grpVar <- renderPlot({
      req(input$rad_grpVar)
      col_plotter(dat2(), num_name, n, input$rad_grpVar)
    })
    
    
    ### Dynamic UI 
    #### Display sliders
    output$ui_slid_impOpt <- renderUI({
      
      req(input$sel_impOpt %in% c("imp_pass_group","imp_cabin"))
      switch(input$sel_impOpt,
        imp_pass_group=sliderInput(ns("slid1_impOpt"),
                        "Select a range of named passengers per passenger_group to use for name imputation",
                        value=c(3,3),min=1,max=7),
        imp_cabin=sliderInput(ns("slid2_impOpt"),
                              "Select a range of named passengers per cabin to use for name 
                              imputation",value=c(3,3),min=1,max=6)
      )
    })
    
    
    #### Create reactive object (for creating a new DF)
    dat3 <- reactive({
      req(input$sel_impOpt %in% c("imp_pass_group","imp_cabin"))
      switch(input$sel_impOpt,
        imp_pass_group=mis_name_tabler(df_train,l_name,passenger_group),
        imp_cabin=mis_name_tabler(df_train,l_name,cabin)
      )
    })
    
    #### Create new data frame object after name imputation or col/row removal
    df_train_nI_tmp <- reactive({
      #requires selection from drop-down menu
      req(input$sel_impOpt)
      #dplyr code if drop_cols selected
      if(input$sel_impOpt=="drop_cols"){
        df_train %>% select(-contains("name"))
      }
      
      #same for remove_rows
      else if(input$sel_impOpt=="remove_rows"){
        df_train %>% filter(!is.na("name"))
      }
      #if imp_pass_groups chosen and slider input values chosen then name_imputer() runs
      else if(input$sel_impOpt=="imp_pass_group" & length(input$slid1_impOpt) > 0){
        name_imputer(dat3(), num_name, input$slid1_impOpt,df_train,passenger_group)
      }
      else if(input$sel_impOpt=="imp_cabin" & length(input$slid2_impOpt) > 0){
        name_imputer(dat3(), num_name, input$slid2_impOpt,df_train,cabin)
      }
    })
    
    #### Test whether code above is working
    output$test_table<-renderTable({
      head(df_train_nI()) 
    })
    
    #### Select how to handle missing names
    eventReactive(input$btn_impOpt, {
      req(input$sel_impOpt)
      df_train_nI_tmp()
    })
    
    observeEvent(input$btn_impOpt, {
      if(input$sel_impOpt %in% namMis03_impOptVec) {
        show_toast(
          title="Name imputation",
          type="success",
          text=impute_name_msg(input$sel_impOpt),
          position="center",
          timer=3000
        )} else{
          show_toast(
            title="Name imputation",
            type="error",
            text="Please select an action",
            position="center",
            timer=2000
          )
        }
    })
    
    #### Temporary code--to update name of DF
    # reactive({
    #   df_train_nI()
    # })
    
    #### Test whether code directly above is working
    # output$test_table2<-renderTable({
    #   head(df_train_nvI()) 
    # })
    
  })
}





