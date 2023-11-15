#Feature Engineering-Transformations Module

# UI================================================================================================
featTransUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Transformations",id="trnsFea04",
    titlePanel("Feature Scaling and Extraction"),
    h4("In this section, you will have the opportuntiy to normalize/standardize numerical data, bin numerical (or 
      character or factor variables) into (smaller) groups,perform ordinal encoding, and group rare categories
      together. What would you like to begin with?"),
    #fluidRow with column helps to align radio buttons
    fluidRow(
      column(6,align="center",offset=3,
        radioButtons(inputId=ns("rad_trans"),label="",choices=ch_trans_featTrans,selected=character(0),
                     inline=TRUE,width="100%"),
        uiOutput("ui_chk_trans")
      )
    ),
    #selector for the discretization panels
    # fluidRow(
    #   uiOutput("ui_sel_dis1")
    # ),
    sidebarLayout(
      sidebarPanel(
        #create invisible panel that can be updated
        tabsetPanel(id=ns("sidebar_tab"),type="hidden",
          #feature scaling
          tabPanelBody("Feature Scaling",
            uiOutput(ns("ui_sel_scale1")),
            br(),
            uiOutput(ns("ui_sel_scale2")),
            uiOutput(ns("ui_btn_scale"))
          ),
          #discretization
          tabPanelBody("Discretization",
            uiOutput(ns("ui_sel_dis1")),
            uiOutput(ns("ui_rad_dis1")),
            uiOutput(ns("ui_num_dis1")),
            br(),
            fluidRow(
              column(9,
                htmlOutput(ns("text_dis3a"))
              ),
              column(3,
                uiOutput(ns("ui_btn_dis3a"))
              )
            ),
            tags$style(type="text/css", "#ui_btn_dis3a {width: 100%; margin-top: 25px;}"),
            br(),
            htmlOutput(ns("text_dis2")),
            uiOutput(ns("ui_rad_dis2a")),
            uiOutput(ns("ui_num_dis2a")),
            uiOutput(ns("ui_rad_dis2b")),
            uiOutput(ns("ui_num_dis2b")),
            fluidRow(
              column(9,
                htmlOutput(ns("text_dis3b"))
              ),
              column(3,
                uiOutput(ns("ui_btn_dis3b"))
              )
            ),
            tags$style(type="text/css", "#ui_btn_dis3b {width: 100%; margin-top: 25px;}")
          ),           
          #ordinal encoding
          tabPanelBody("Ordinal Encoding",
            uiOutput(ns("ui_sel_ordEnc1")),
            linebreaks(2),
            radioButtons(inputId=ns("rad_ordEnc"),label="Would you like to perform ordinal encoding on any of
                         the variables?",choices=c("Yes","No"),selected=character(0)),
            linebreaks(2),
            htmlOutput(ns("text_ordEnc")),
            #produces a list of checkboxes and selectors
            map(labs, split_chk_sel_builder, fn=ns),
            # ui_splits,
            uiOutput(ns("ui_btn_ordEnc2"))
          ),
          #rare label encoding
          tabPanelBody("Rare Label Encoding",
            uiOutput(ns("ui_sel_rareEnc1a")),
            uiOutput(ns("ui_sel_rareEnc1b")),
            linebreaks(5),
            uiOutput(ns("ui_sel_rareEnc2a")),
            uiOutput(ns("ui_sel_rareEnc2b")),
            uiOutput(ns("ui_btn_rareEnc"))
          )
        )
      ),
      mainPanel(
        #set this to dynamically produce tabs
        tabsetPanel(id=ns("main_tab"),type="hidden",
          #feature scaling
          tabPanelBody("Feature Scaling",
            plotOutput(ns("plot_sel_scale1"),height="1000px"),
            tableOutput(ns("DT1"))
          ),
          #discretization
          tabPanelBody("Discretization",
            plotOutput(ns("plot_sel_dis1")),
            plotOutput(ns("plot_sel_dis2")),
            tableOutput(ns("temp_table_dis1")),
            tableOutput(ns("temp_table_dis2")),
            tableOutput(ns("temp_table_dis3")),
            tableOutput(ns("temp_table_dis4")),
            tableOutput(ns("temp_table_dis5")),
            tableOutput(ns("temp_table_dis6")),
            tableOutput(ns("temp_table_dis7")),
            tableOutput(ns("temp_table_dis8"))
          ),
          #ordinal encoding
          tabPanelBody("Ordinal Encoding",
            plotOutput(ns("plot_sel_ordEnc1")),
            linebreaks(2),
            htmlOutput(ns("text_sel_ordEnc1")),
            tableOutput(ns("temp_tab"))
          ),
          #rare label encoding
          tabPanelBody("Rare Label Encoding",
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
                tableOutput(ns("temp_table_rareEnc"))
              )
            )
          )
        )
      )
    )
  )
}


# Server============================================================================================
featTransServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    output$ui_chk_trans <- renderUI({   
    #update req() statement--should reflect that all four confirmations selected
    req(input$rad_trans)
    checkboxInput(inputId=ns("chk_trns"),label="CONFIRM ALL DATA TRANSFORMATIONS SELECTED",value=FALSE)
  })
  
    ## Conditional tabsets----------------------------------------
    ### Conditional UI for displaying 'main' sidebar tabset panel
    observeEvent(input$rad_trans, {
      updateTabsetPanel(inputId="sidebar_tab",selected=input$rad_trans)
    })
    
    
    ### Conditional UI for displaying main tabset panel for larger categories
    observeEvent(input$rad_trans, {
      updateTabsetPanel(inputId="main_tab",selected=input$rad_trans)
    })
    
    
    
    
    ## Dynamic UI to display inputs----------------------------------------
    ### Character string objects for label argument
    varViz_feat<-"Please select a variable to visualize"
    varSel_feat<-"Please select which variables for ordinal encoding"
    varSelOrd_feat<-c("Rank all from least to most important"="")
    scaleOpt_feat<-"Please select the type of scaling for the list of numerical variables"
    
    
    ### Normalization/standardization--------------------
    #### Input to select var to visualize, either unscaled or scaled
    output$ui_sel_scale1<-renderUI({
      req(input$rad_trans)
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
    
    #### Button to confirm selection
    output$ui_btn_scale<-renderUI({
      req(input$rad_trans,input$sel_scale1,input$sel_scale2)
      actionButton(inputId=ns("btn_scale"),label="Confirm your selection") 
    })
    
    #### Button to confirm scaling selections and create new columns/variables
    df_train_nvI_s<-eventReactive(input$btn_scale, {
      switch(input$sel_scale2,
             #raw = unchanged
             raw=df_train_nvI(),
             #log = log-transform + identifier
             log=df_train_nvI() %>% 
               mutate(across(where(is.numeric),~log(.x),.names="{.col}_scale")) %>%
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
    
    
  
    ### Discretization--------------------
    #### Input to select var to visualize as histogram (for discretization)
    output$ui_sel_dis1<-renderUI({
      req(input$rad_trans=="Discretization")
      selectInput01(ID=ns("sel_dis1"),label=varViz_feat,
                    #dynamically select numerical variables and num 
                    choices=disVars)
    })
    
    #### Input to choose whether to log10-transform x-axis
    output$ui_rad_dis1<-renderUI({
      req(input$sel_dis1)
      radioButtons(inputId=ns("rad_dis1"),
                   label="Choose whether to log10-scale the x-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    #### Input to choose number of bins for histogram
    output$ui_num_dis1<-renderUI({
      req(input$sel_dis1)
      numericInput(inputId=ns("num_dis1"),
                   label="Select the number of bins for the histogram (2-100)",
                   value=30,min=2,max=100)
    })
    
    #### Output to display text for next set of inputs
    output$text_dis2<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      h4("Visualization of binned data")
    })
    
    
    #### Input to select a log10-transformed y-axis
    output$ui_rad_dis2a<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      radioButtons(inputId=ns("rad_dis2a"),
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    #### Input to choose number of breaks
    output$ui_num_dis2a<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      numericInput(inputId=ns("num_dis2a"),
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2,min=1,max=5)
    })
    
    #### Input to choose whether to have R or user-selected bin boundaries
    output$ui_rad_dis2b<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      radioButtons(inputId=ns("rad_dis2b"),
                   label="Choose who selects the bin boundaries",
                   choices=c("R","me"),selected=character(0),inline=TRUE)
    })
    
    
  
    
    
    #### Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
    output$ui_num_dis2b<-renderUI({
      req(input$rad_dis2a,input$rad_dis2b=="me")
      tags_num<-tagList()
      for(i in seq_len(input$num_dis2a)){
        tags_num[[i]]<-numericInput(ns(paste0("n",i)),paste0("Break",i),min=0,value=NULL)
      }
      tags_num
    })
    
  
  
    #### Dynamically display action button (and associated text) to discretize variable 
    #display text for action buttons
    output$text_dis3a<-renderUI({
      req(!is.na(input$rad_dis1))
      h4(paste0("I am not interested in discretizing ",input$sel_dis1,"."))
    })
    
    output$text_dis3b<-renderUI({
      #either "R" is selected or "me" is selected and the number and every break point input is populated
      req((!is.na(input$rad_dis2a) & input$rad_dis2b=="R")|
         (input$rad_dis2b=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a)
      )
      h4(paste("Click confirm to discretize",input$sel_dis1, "using these settings."))
    })
    
    #display buttons
    output$ui_btn_dis3a<-renderUI({
      req(!is.na(input$rad_dis1))
      switch(input$sel_dis1,
        age=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        room_service=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        food_court=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        shopping_mall=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        spa=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        vr_deck=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        num=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm")
      )
    })
    
    output$ui_btn_dis3b<-renderUI({
      req((!is.na(input$rad_dis2a) & input$rad_dis2b=="R")|
            (input$rad_dis2b=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a)
      )
      switch(input$sel_dis1,
        age=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        room_service=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        food_court=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        shopping_mall=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        spa=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        vr_deck=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        num=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm")
      )
    })
    
  
    
    
    ### Ordinal Encoding--------------------
    #### Select variable to visualize
    output$ui_sel_ordEnc1<-renderUI({
      selectInput01(ID=ns("sel_ordEnc1"),label=varViz_feat,
                    #dynamically select factors 
                    choices=df_train_nvI() %>% select(where(is.factor),-num) %>% names())
    })
    
    # Dynamically display text above checkboxes below
    output$text_ordEnc<-renderUI({
      req(input$rad_ordEnc=="Yes") 
      h4("Check each variable for ordinal encoding and rank the categories from least to most important")
    })
    
    
    #### Dynamically create checkboxes to choose variables for ordinal encoding
      output$ui_chk_ordEnc2a<-renderUI({
        req(input$rad_ordEnc=="Yes") 
        checkboxInput(inputId=ns("chk_ordEnc2a"),label="ticket",value=FALSE)
      })
      output$ui_chk_ordEnc2b<-renderUI({
        req(input$rad_ordEnc=="Yes") 
        checkboxInput(inputId=ns("chk_ordEnc2b"),label="home_planet",value=FALSE)
      })
      output$ui_chk_ordEnc2c<-renderUI({
        req(input$rad_ordEnc=="Yes") 
        checkboxInput(inputId=ns("chk_ordEnc2c"),label="deck",value=FALSE)
      })
      output$ui_chk_ordEnc2d<-renderUI({
        req(input$rad_ordEnc=="Yes") 
        checkboxInput(inputId=ns("chk_ordEnc2d"),label="side",value=FALSE)
      })
      output$ui_chk_ordEnc2e<-renderUI({
        req(input$rad_ordEnc=="Yes") 
        checkboxInput(inputId=ns("chk_ordEnc2e"),label="destination",value=FALSE)
      })
  
      
    #### Dynamically create selectors for ordinal encoding
    output$ui_sel_ordEnc2a<-renderUI({
      req(input$chk_ordEnc2a)
      selectizeInput(inputId=ns("sel_ordEnc2a"),label="", multiple=TRUE,
                    choices=c(varSelOrd_feat,
                            df_train_nvI()[["ticket"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2b<-renderUI({
      req(input$chk_ordEnc2b)
      selectizeInput(inputId=ns("sel_ordEnc2b"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["home_planet"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2c<-renderUI({
      req(input$chk_ordEnc2c)
      selectizeInput(inputId=ns("sel_ordEnc2c"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["deck"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2d<-renderUI({
      req(input$chk_ordEnc2d)
      selectizeInput(inputId=ns("sel_ordEnc2d"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["side"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2e<-renderUI({
      req(input$chk_ordEnc2e)
      selectizeInput(inputId=ns("sel_ordEnc2e"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["destination"]] %>% levels()))
    })
    
    
    #### Dynamically display button
    output$ui_btn_ordEnc2<-renderUI({
      n_ticket<-nlevels(df_train_nvI()[["ticket"]])
      n_home_planet<-nlevels(df_train_nvI()[["home_planet"]])
      n_deck<-nlevels(df_train_nvI()[["deck"]])
      n_side<-nlevels(df_train_nvI()[["side"]])
      n_destination<-nlevels(df_train_nvI()[["destination"]])
      
      #button displays if 1) "No" selected in radio button; 2) at least one box is checked AND for each var either
        #1) box unchecked or all categories selected 
      req(input$rad_ordEnc=="No"|(
        sum(length(input$sel_ordEnc2a)==n_ticket,
            length(input$sel_ordEnc2b)==n_home_planet, 
            length(input$sel_ordEnc2c)==n_deck,
            length(input$sel_ordEnc2d)==n_side,
            length(input$sel_ordEnc2e)==n_destination) > 0 & (
        (length(input$sel_ordEnc2a)==n_ticket|input$chk_ordEnc2a==FALSE) &
        (length(input$sel_ordEnc2b)==n_home_planet|input$chk_ordEnc2b==FALSE) &
        (length(input$sel_ordEnc2c)==n_deck|input$chk_ordEnc2c==FALSE) &
        (length(input$sel_ordEnc2d)==n_side|input$chk_ordEnc2d==FALSE) &
        (length(input$sel_ordEnc2e)==n_destination|input$chk_ordEnc2e==FALSE)
        )
        )
      )
      actionButton(inputId=ns("btn_ordEnc2"),label="Confirm all ordinal encoding selections")
    })
    
  
    ### Rare Label Encoding--------------------
    #### Input to select var to visualize as a barplot
    output$ui_sel_rareEnc1a<-renderUI({
      selectInput01(ID=ns("sel_rareEnc1a"),label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=df_train_nvI() %>% select(deck,ticket) %>% names())
    })
    
    # Input to select levels to combine as a category and visualize in a new barplot (NAs are off limits)
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
  
    
    
    
    
    ## Conditional output for feature extraction----------------------------------------
    ### Normalization/standardization--------------------
    #### Display set of plots
    output$plot_sel_scale1<-renderPlot({
      req(input$sel_scale1)
      cowplotter(df_train_nvI(),input$sel_scale1)
    })
  
    
    ### Discretization--------------------
    #### Plot raw data with fill=transported as histogram
    output$plot_sel_dis1<-renderPlot({
      req(input$rad_dis1)
      switch(input$sel_dis1,
        age=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        room_service=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        food_court=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        shopping_mall=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        spa=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        vr_deck=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        num=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1)
      )
    })
    
    
    #### Plot numerical var in bins filled by transported either using R or user specified break values
    #create reactive vectors of cut locations
    user_cuts<-reactive({
      c(input$n1,input$n2,input$n3,input$n4,input$n5)
    })
    
    R_cuts<-reactive({
      layer_scales(plot_dis())$x$breaks
    })
    
  
    
    #use switch to create plot object
    plot_dis<-reactive({
      req(input$rad_dis2a, input$rad_dis2b)
      switch(input$rad_dis2b,
             R=bin_plotter(dat=df_train_nvI(),col=input$sel_dis1,num.breaks=input$num_dis2a,
                           y.log.scale=input$rad_dis2a),
             me=user_bin_plotter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts(),
                                 y.log.scale=input$rad_dis2a)
      )
    })
    
    #plot the plot object
    output$plot_sel_dis2<-renderPlot({
      plot_dis()
    })
    
    
    
    #### Create reactiveValues object and initialize with NULL values
    #df_train_dis_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,vr_deck=NULL,num=NULL)
    # df_train_dis_list<-reactiveValues()
    df_train_dis_list<-vector(mode="list")
  
   
    
    # Update reactiveValues object with passenger_id values as a tibble when selector input is used
    # observeEvent(input$rad_trans, {
    #   
    # }
    # })
    
   # df_train_dis_list<-reactiveValues(age=NULL)
   #  
   #  df_train_dis_list$age<-reactive({df_train_nvI() %>% select(passenger_id)})
  
    # 
    # df_train_dis_list$room_service<-reactive({
    #   df_train_nvI() %>% select(passenger_id) 
    # })
    # 
    # df_train_dis_list$food_court<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$shopping_mall<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$spa<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$vr_deck<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$num<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
  
    
  
    
    # df_train_dis_list<-reactiveValues(age=NULL)
    
    #df_train_dis_list$room_service<-tibble(x="test")
    
   # df_train_dis_list<-reactiveValues(age=df_train["passenger_id"],
   #                         room_service=df_train["passenger_id"])
   
                           # food_court=df_train["passenger_id"],
                           # shopping_mall=df_train["passenger_id"],
                           # spa=df_train["passenger_id"],
                           # vr_deck=df_train["passenger_id"],
                           # num=df_train["passenger_id"])
    
    # df_train_dis_list<-reactiveValues(age=NA,room_service=NA,food_court=NA,shopping_mall=NA,spa=NA,
    #                                  vr_deck=NA,num=NA)
    
    # Confirm discretization (for each variable)
    #age
    # df_train_dis_list$age<-eventReactive(input$btn_dis3a_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
  
    observeEvent(input$sel_dis1e, {
      df_train_dis_list$age<-df_train %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_age, {
      req(input$rad_dis2b)
      if(input$rad_dis2b=="R"){
        df_train_dis_list$age<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
      }
      else if(input$rad_dis2b=="me"){
        df_train_dis_list$age<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      }
    })
    
    
    observeEvent(input$sel_dis1, {
      df_train_dis_list$room_service<-df_train %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_room_service, {
      req(input$rad_dis2b)
      if(input$rad_dis2b=="R"){
        df_train_dis_list$room_service<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
      }
      else if(input$rad_dis2b=="me"){
        df_train_dis_list$room_service<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      }
    })
    
    # df_train_dis_list$age_pi<-eventReactive(input$btn_dis3a_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$age<-reactive({
    #   if(input$rad_trans=="Discretization" & !exists("rad_dis2b_trnsFea")){
    #     df_train_nvI() %>% select(passenger_id)}
    #   if(input$sel_dis1=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b=="R"){
    #     cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
    #     }
    #   if(input$sel_dis1=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b=="me"){
    #     cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #     }
    # })
  
    
  
    #room_service
    # df_train_dis_list$room_service<-eventReactive(list(input$rad_trans,input$btn_room_service), {
    #   rs1<-df_train["passenger_id"]
    #   rs2<- switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$room_service<-eventReactive(input$btn_room_service, {
      # switch(input$rad_dis2b,
      #        R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
      #        me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      # )
    # })
    
    # reax_room_service<-eventReactive(input$btn_room_service, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    
    # #food_court
    # df_train_dis_list$food_court<-eventReactive(input$btn_food_court, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #shopping_mall
    # df_train_dis_list$shopping_mall<-eventReactive(input$btn_shopping_mall, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #spa
    # df_train_dis_list$spa<-eventReactive(input$btn_spa, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #vr_deck
    # df_train_dis_list$vr_deck<-eventReactive(input$btn_vr_deck, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #num
    # df_train_dis_list$num<-eventReactive(input$btn_num, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    #train_list<-reactiveValuesToList(df_train_dis_list)
    
    
    
    
  # Combine all list elements into a new reactive DF
  
    
    # df_train_nvI_d<-eventReactive(input$btn_age,{
    #   df_train_nvI() %>%
    #     select(passenger_id) %>%
    #     {if(ncol(df_train_dis_list$age())==2)(left_join(.,df_train_dis_list$age(),by="passenger_id")) else .} #%>%
    #     {if(ncol(df_train_dis_list$room_service())==2)(left_join(.,df_train_dis_list$room_service(),by="passenger_id")) else .} %>%
    #     {if(ncol(df_train_dis_list$food_court())==2)(left_join(.,df_train_dis_list$food_court(),by="passenger_id")) else .} #%>%
    # #     # {if(input$btn_shopping_mall==1)(left_join(.,df_train_dis_list$shopping_mall(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_spa==1)(left_join(.,df_train_dis_list$spa(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_vr_deck==1)(left_join(.,df_train_dis_list$vr_deck(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_num==1)(left_join(.,df_train_dis_list$num(),by="passenger_id")) else .}
    # })
    
  
    
  
    
    
    # df_train_nvI_d<-reactive({
    #   reduce(list(df_train_dis_list$age,
    #               df_train_dis_list$room_service),
    # # # #               # df_train_dis_list$food_court(),
    # # # #               # df_train_dis_list$shopping_mall(),
    # # # #               # df_train_dis_list$spa(),
    # # # #               # df_train_dis_list$vr_deck(),
    # # #               # df_train_dis_list$num()),
    #          left_join,by="passenger_id")
    # })
    
    
  
    
    # df_train_nvI_d<-reactive({
    #   reduce(df_train_dis_list,left_join,by="passenger_id")
    # })
      
    # df_train_nvI_d<-reactive({
    #     df_train_dis_list$age %>%
    #     left_join(df_train_dis_list$room_service,by="passenger_id") #%>%
    #     # left_join(df_train_dis_list$food_court,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$shopping_mall,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$spa,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$vr_deck,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$num,by="passenger_id")
    # })
      
        #purrr::when(sum(!is.na(df_train_dis_list$age()))>0 ~left_join(.,df_train_dis_list$age(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(df_train_dis_list$room_service()))>0 ~left_join(.,df_train_dis_list$room_service(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(df_train_dis_list$food_court()))>0 ~left_join(.,df_train_dis_list$food_court(),by="passenger_id"), ~.) #%>%
        #{if(!is.na(df_train_list$shopping_mall))(left_join(.,df_train_dis_list$shopping_mall(),by="passenger_id")) else .} #%>%
      # {if(!is.null(df_train_dis_list$spa()))(left_join(.,df_train_dis_list$spa(),by="passenger_id")) else .} %>%
      # {if(!is.null(df_train_dis_list$vr_deck()))(left_join(.,df_train_dis_list$vr_deck(),by="passenger_id")) else .} %>%
      # {if(!is.null(df_train_dis_list$num()))(left_join(.,df_train_dis_list$num(),by="passenger_id")) else .}
    #})
    
    
    # toy_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,
    #                   c               vr_deck=NULL,num=NULL)
    
    # 
    # df_train %>%
    #   select(passenger_id) %>%
    #   #left_join(df_train[c("passenger_id","age")],by="passenger_id")
    #   {if(is.null(samp))(left_join(.,df_train[c("passenger_id","age")],by="passenger_id")) else .} %>%
    #   {if(is.null(dog))(left_join(.,df_train[c("passenger_id","spa")],by="passenger_id")) else .}
    # 
    # df_train %>%
    #   select(passenger_id) %>%
    #   #left_join(df_train[c("passenger_id","age")],by="passenger_id")
    #   purrr::when(is.null(samp) ~left_join(.,df_train[c("passenger_id","age")],by="passenger_id"), ~.)
    
    #Convert reactiveValues to list
    #df_train_dis_list<-reactiveValuesToList(df_train_dis_list)
    
    
    
    # Check whether results working (by variable)
    #age
    output$temp_table_dis1<-renderTable({
      df_train_dis_list$age %>% head()
    })
  
    # # #room_service
    output$temp_table_dis2<-renderTable({
      df_train_dis_list$room_service %>% head()
    })
    # 
    # 
    # # #food_court
    # output$temp_table_dis3<-renderTable({
    #   df_train_dis_list$food_court() %>% head()
    # })
    # 
    # # #shopping_mall
    # output$temp_table_dis4<-renderTable({
    #   df_train_dis_list$shopping_mall() %>% head()
    # })
    # 
    # #spa
    # output$temp_table_dis5<-renderTable({
    #   df_train_dis_list$spa() %>% head()
    # })
    # 
    # #vr_deck
    # output$temp_table_dis6<-renderTable({
    #   df_train_dis_list$vr_deck() %>% head()
    # })
    # 
    # #num
    # output$temp_table_dis7<-renderTable({
    #   df_train_dis_list$num() %>% head()
    # })
    
    # output$temp_table_dis8<-renderTable({
    #   df_train_nvI_d() %>% head()
    # })
  
    
    # Connect reactive DFs together
    #list_dis<-vector(mode="list",length=7)
    # list_dis<-reactiveValuesToList(c(df_train_nvI_da(),df_train_nvI_dr(),df_train_nvI_df(),df_train_nvI_dm(),
    #                                          df_train_nvI_ds(),df_train_nvI_dv(),df_train_nvI_dn()))
    # 
    
   # df_train_nvI_d<-reactive({
   #   df_train_nvI_da() %>%
   #   left_join(df_train_nvI_dr(),by=passenger_id) %>%
   #     left_join(df_train_nvI_df(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dm(),by=passenger_id) %>%
   #     left_join(df_train_nvI_ds(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dv(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dv(),by=passenger_id) %>%
   #     select(passenger_id,ends_with("dis"))
   # })
    
  
    
    #age - a
    #room_service - r
    #food_court - f
    #shopping_mall - m
    #spa - s
    #vr_deck - v
    #num - n
    
  
    
    
    ### Ordinal Encoding--------------------
    #### Display plot
    output$plot_sel_ordEnc1<-renderPlot({
      req(input$sel_ordEnc1)
      barplotter(df_train_nvI(),input$sel_ordEnc1)
    })
    
    
    #### Display text associated with each variable 
    output$text_sel_ordEnc1<-renderUI({
      req(input$sel_ordEnc1)
      switch(input$sel_ordEnc1,
        ticket=HTML("<i>passenger_id</i> is broken into two parts: <i>passenger_group</i> (the first four digits) and 
          <i>ticket</i> (the last two digits). The 'ticket' component indicates the number/position within a passenger group."),
        home_planet=HTML("<i>home_planet</i> represents the planet that the passenger left, which is often where they live. 
          There are three home planets in this data set: <br>
          <b>Earth</b>: third planet from the Sun <br>
          <b>Mars</b>: fourth planet fromt the Sun <br>
          <b>Europa</b>: smallest of the four Galilean moons orbiting Jupiter"),
        deck=HTML("<i>deck</i> is one of three components of the variable <i>cabin</i> along with <i>num</i> and <i>side</i>.
          There are eight different decks: <b>A</b>-<b>G</b> and <b>T</b>"),
        side=HTML("<i>side</i> is one of three components of the varible <i>cabin</i> along with <i>deck</i> and <i>num</i>.
          <i>side</i> can take on one of two values: <b>P</b> for port and <b>S</b> for starboard."),
        destination=HTML("<i>destination</i> represents the planet to which the passenger is traveling. There are three
          possible destinations: <br>
          <b>TRAPPIST-1e</b>: a rocky, near-Earth-sized exoplanet that researchers consider as potentially habitable by
            humans <br>
          <b>55 Cancri e</b>: an exoplanet nearly 9x the mass of the Earth with an atmosphere composed of at least hydrogen
            and helium <br>
          <b>PSO J318.5-22</b>: a rogue planet with estimated temperatures of its clouds exceed 800 <sup>o</sup> C")
      )
    })
    
    
    #### Create reactive data frame
    df_train_nvI_o<-eventReactive(input$btn_ordEnc2, {
        df_train_nvI() %>%
          #choose all factors except num
          mutate(across(.cols=df_train_fct_nonumVars,~as.ordered(.x))) %>%
            #if...else statements for whether to change factor level order based on if checkbox checked
            {if(input$chk_ordEnc2a==TRUE)
              mutate(.,ticket_ord=fct_relevel(ticket,input$sel_ordEnc2a))
              else .} %>%
            {if(input$chk_ordEnc2b==TRUE)
              mutate(.,home_planet_ord=fct_relevel(home_planet,input$sel_ordEnc2b))
              else .} %>%
            {if(input$chk_ordEnc2c==TRUE)
              mutate(.,deck_ord=fct_relevel(deck,input$sel_ordEnc2c))
              else .} %>%
            {if(input$chk_ordEnc2d==TRUE)
              mutate(.,side_ord=fct_relevel(side,input$sel_ordEnc2d))
              else .} %>%
             {if(input$chk_ordEnc2e==TRUE)
              mutate(.,destination_ord=fct_relevel(destination,input$sel_ordEnc2e))
              else .} %>%
          #retain passenger_id and mutated cols
          select(passenger_id,ends_with("_ord"))
      })
    
    #### Print temp table as a check
    output$temp_table<-renderTable({
      df_train_nvI_o() %>% head()
    })
  
    
    
    ### Rare Label Encoding--------------------
    #### Display plots
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
    
    # Extract features via rare label encoding
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
    
    #### Output temp table
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

