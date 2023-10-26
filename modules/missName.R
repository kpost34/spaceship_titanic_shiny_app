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
        selectInput01(ID=ns("sel_exp_namMis03"),label="",choices=namMis03_expVec),
        br(),
        #go deeper with some possibilities
        h4("Two hundred out of 8693 passengers (in the training data) lack names. That's 2.3%. Although first names, and thus
           full names will be impossible to impute from the other variables. Last names may be populated with confidence if we
           assume passengers traveled together as families. Two ways to conclude that the traveling party is a family is
           1) purchasing tickets together (same passenger group) or 2) saying in the same room (cabin). Here's how the patterns break down."),
        radioButtons(inputId=ns("rad_grpVar_namMis03"),label="",choices=c("passenger_group"="passenger_group",
                                                                      "cabin occupancy"="cabin"),
                     selected=character(0)),
        h4("Note that each group, regardless of group size or grouping variable, has one unnamed passenger."),
        br(),
        h4("Given all this information, how would you like to handle passengers with missing names?"),
        selectInput01(ID=ns("sel_impOpt_namMis03"),label="",choices=namMis03_impOptVec),
        br(),
        uiOutput(ns("ui_slid_impOpt_namMis03"))
      ),
      mainPanel(
        htmlOutput(ns("text_sel_exp_namMis03")),
        DTOutput(ns("tab_sel_exp_namMis03")),
        plotOutput(ns("plot_sel_exp_namMis03")),
        br(),
        htmlOutput(ns("text_rad_grpVar_namMis03")),
        plotOutput(ns("plot_rad_grpVar_namMis03")),
        tableOutput(ns("test_table")),
        # tableOutput(ns("test_table2"))
      )
    )
  )
}





# Server============================================================================================
missNameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Exploring missing names--------------------
    ### Text outputs
    output$text_sel_exp_namMis03<-renderUI({
      #here switch() can be used with all four choices to display the appropriate name
      switch(input$sel_exp_namMis03,
        miss_samp=h3(paste("Sample of Passengers with Missing Names")),
        nmiss_samp=h3(paste("Sample of Passengers with Names")),
        sum_tab=h3(paste("Summary of Missing Names")),
        plot=h3(paste("Plot of Missing Names"))
      )
    })
    
    ### Create reactive object (for tabular output)
    dat1_namMis03<-reactive({
      #reactive is used to build reactive table objects
      switch(input$sel_exp_namMis03,
        miss_samp=trainDF %>% filter(is.na(name)) %>% slice_sample(n=5),
        nmiss_samp=trainDF %>% filter(!is.na(name)) %>% slice_sample(n=5),
        sum_tab=chr_miss_tabler(trainDF)
      )
    })
    
  
    ### Output table/plot
    #### Table output
    output$tab_sel_exp_namMis03<-renderDT(
      dat1_namMis03(),options=list(scrollX="400px")
    )
    
    #### Plot output
    output$plot_sel_exp_namMis03<-renderPlot({
      #plot outputs only when selected (note that this is always the same plot)
      req(input$sel_exp_namMis03=="plot")
      chr_miss_boxplotter(trainDF)
    })
    
    
    ## Understanding name missingness conditioned on other variables--------------------
    ### Text outputs
    output$text_rad_grpVar_namMis03<-renderUI({
      req(input$rad_grpVar_namMis03)
      switch(input$rad_grpVar_namMis03,
        passenger_group=h3(paste("Summary of Missing Names by Size of Passenger Groups")),
        cabin=h3(paste("Summary of Missing Names by Cabin Occupancy"))
      )
    })
    
    ### Create reactive object (for tabular and plot outputs)
    dat2_namMis03<-reactive({
      switch(input$rad_grpVar_namMis03,
        passenger_group=mis_name_tabler(trainDF,l_name,passenger_group),
        cabin=mis_name_tabler(trainDF,l_name,cabin)
      )
    })
    
    
    ### Output plots
    output$plot_rad_grpVar_namMis03<-renderPlot({
      req(input$rad_grpVar_namMis03)
      col_plotter(dat2_namMis03(),num_name,n)
    })
    
    
    ### Dynamic UI 
    #### Display sliders
    output$ui_slid_impOpt_namMis03<-renderUI({
      req(input$sel_impOpt_namMis03 %in% c("imp_pass_group","imp_cabin"))
      switch(input$sel_impOpt_namMis03,
        imp_pass_group=sliderInput("slid1_impOpt_namMis03",
                        "Select a range of named passengers per passenger_group to use for name imputation",
                        value=c(3,3),min=1,max=7),
        imp_cabin=sliderInput("slid2_impOpt_namMis03","Select a range of named passengers per cabin to use for name 
                              imputation",value=c(3,3),min=1,max=6)
      )
    })
    
    
    #### Create reactive object (for creating a new DF)
    dat3_namMis03<-reactive({
      req(input$sel_impOpt_namMis03 %in% c("imp_pass_group","imp_cabin"))
      switch(input$sel_impOpt_namMis03,
        imp_pass_group=mis_name_tabler(trainDF,l_name,passenger_group),
        imp_cabin=mis_name_tabler(trainDF,l_name,cabin)
      )
    })
    
    #### Create new data frame object after name imputation or col/row removal
    trainDF_nI<-reactive({
      #requires selection from drop-down menu
      req(input$sel_impOpt_namMis03)
      #dplyr code if drop_cols selected
      if(input$sel_impOpt_namMis03=="drop_cols"){
        trainDF %>% select(-contains("name"))
      }
      
      #same for remove_rows
      else if(input$sel_impOpt_namMis03=="remove_rows"){
        trainDF %>% filter(!is.na("name"))
      }
      #if imp_pass_groups chosen and slider input values chosen then name_imputer() runs
      else if(input$sel_impOpt_namMis03=="imp_pass_group" & length(input$slid1_impOpt_namMis03)>0){
        name_imputer(dat3_namMis03(),num_name,input$slid1_impOpt_namMis03,trainDF,passenger_group)
      }
      else if(input$sel_impOpt_namMis03=="imp_cabin" & length(input$slid2_impOpt_namMis03)>0){
        name_imputer(dat3_namMis03(),num_name,input$slid2_impOpt_namMis03,trainDF,cabin)
      }
    })
    
    #### Test whether code above is working
    output$test_table<-renderTable({
      head(trainDF_nI()) 
    })
    
    
    #### Temporary code--to update name of DF
    reactive({
      trainDF_nI()
    })
    
    #### Test whether code directly above is working
    # output$test_table2<-renderTable({
    #   head(trainDF_nvI()) 
    # })
    
  })
}





