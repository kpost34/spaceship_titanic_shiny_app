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
        radioButtons(inputId="rad_trnsFea04",label="",choices=ch_trans_featTrans,selected=character(0),
                     inline=TRUE,width="100%"),
        # uiOutput("ui_chk_trnsFea04")
      )
    ),
    #selector for the discretization panels
    # fluidRow(
    #   uiOutput("ui_sel_dis1_trnsFea04")
    # ),
    sidebarLayout(
      sidebarPanel(
        #create invisible panel that can be updated
        tabsetPanel(id="sidebar_tab_trnsFea04",type="hidden",
          #feature scaling
          tabPanelBody("Feature Scaling",
            uiOutput("ui_sel_scale1_trnsFea04"),
            br(),
            uiOutput("ui_sel_scale2_trnsFea04"),
            uiOutput("ui_btn_scale_trnsFea04")
          ),
          #discretization
          tabPanelBody("Discretization",
            uiOutput("ui_sel_dis1_trnsFea04"),
            uiOutput("ui_rad_dis1_trnsFea04"),
            uiOutput("ui_num_dis1_trnsFea04"),
            br(),
            fluidRow(
              column(9,
                htmlOutput("text_dis3a_trnsFea04")
              ),
              column(3,
                uiOutput("ui_btn_dis3a_trnsFea04")
              )
            ),
            tags$style(type="text/css", "#ui_btn_dis3a_trnsFea04 {width: 100%; margin-top: 25px;}"),
            br(),
            htmlOutput("text_dis2_trnsFea04"),
            uiOutput("ui_rad_dis2a_trnsFea04"),
            uiOutput("ui_num_dis2a_trnsFea04"),
            uiOutput("ui_rad_dis2b_trnsFea04"),
            uiOutput("ui_num_dis2b_trnsFea04"),
            fluidRow(
              column(9,
                htmlOutput("text_dis3b_trnsFea04")
              ),
              column(3,
                uiOutput("ui_btn_dis3b_trnsFea04")
              )
            ),
            tags$style(type="text/css", "#ui_btn_dis3b_trnsFea04 {width: 100%; margin-top: 25px;}")
          ),           
          #ordinal encoding
          tabPanelBody("Ordinal Encoding",
            uiOutput("ui_sel_ordEnc1_trnsFea04"),
            linebreaks(2),
            radioButtons(inputId="rad_ordEnc_trnsFea04",label="Would you like to perform ordinal encoding on any of
                         the variables?",choices=c("Yes","No"),selected=character(0)),
            linebreaks(2),
            htmlOutput("text_ordEnc_trnsFea04"),
            #produces a list of checkboxes and selectors
            ui_splits,
            uiOutput("ui_btn_ordEnc2_trnsFea04")
          ),
          #rare label encoding
          tabPanelBody("Rare Label Encoding",
            uiOutput("ui_sel_rareEnc1a_trnsFea04"),
            uiOutput("ui_sel_rareEnc1b_trnsFea04"),
            linebreaks(5),
            uiOutput("ui_sel_rareEnc2a_trnsFea04"),
            uiOutput("ui_sel_rareEnc2b_trnsFea04"),
            uiOutput("ui_btn_rareEnc_trnsFea04")
          )
        )
      ),
      mainPanel(
        #set this to dynamically produce tabs
        tabsetPanel(id="main_tab_trnsFea04",type="hidden",
          #feature scaling
          tabPanelBody("Feature Scaling",
            plotOutput("plot_sel_scale1_trnsFea04",height="1000px"),
            tableOutput("DT1")
          ),
          #discretization
          tabPanelBody("Discretization",
            plotOutput("plot_sel_dis1_trnsFea04"),
            plotOutput("plot_sel_dis2_trnsFea04"),
            tableOutput("temp_table_dis1_trnsFea04"),
            tableOutput("temp_table_dis2_trnsFea04"),
            tableOutput("temp_table_dis3_trnsFea04"),
            tableOutput("temp_table_dis4_trnsFea04"),
            tableOutput("temp_table_dis5_trnsFea04"),
            tableOutput("temp_table_dis6_trnsFea04"),
            tableOutput("temp_table_dis7_trnsFea04"),
            tableOutput("temp_table_dis8_trnsFea04")
          ),
          #ordinal encoding
          tabPanelBody("Ordinal Encoding",
            plotOutput("plot_sel_ordEnc1_trnsFea04"),
            linebreaks(2),
            htmlOutput("text_sel_ordEnc1_trnsFea04"),
            tableOutput("temp_tab_trnsFea04")
          ),
          #rare label encoding
          tabPanelBody("Rare Label Encoding",
            fluidRow(
              column(6,
                plotOutput("plot_sel_rareEnc1a_trnsFea04",height="250px")
              ),
              column(6,
                plotOutput("plot_sel_rareEnc1b_trnsFea04",height="250px")
              )
            ),
            linebreaks(2),
            fluidRow(
              column (6,
                plotOutput("plot_sel_rareEnc2a_trnsFea04",height="250px")
              ),
              column(6,
                plotOutput("plot_sel_rareEnc2b_trnsFea04",height="250px"),
                tableOutput("temp_table_rareEnc_trnsFea04")
              )
            )
          )
        )
      )
    )
  )
}


# Server============================================================================================
featTransServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    output$ui_chk_trnsFea04<-renderUI({   
    #update req() statement--should reflect that all four confirmations selected
    req(input$rad_trnsFea04)
    checkboxInput(inputId="chk_trnsFea04",label="CONFIRM ALL DATA TRANSFORMATIONS SELECTED",value=FALSE)
  })
  
    ## Conditional tabsets----------------------------------------
    ### Conditional UI for displaying 'main' sidebar tabset panel
    observeEvent(input$rad_trnsFea04, {
      updateTabsetPanel(inputId="sidebar_tab_trnsFea04",selected=input$rad_trnsFea04)
    })
    
    
    ### Conditional UI for displaying main tabset panel for larger categories
    observeEvent(input$rad_trnsFea04, {
      updateTabsetPanel(inputId="main_tab_trnsFea04",selected=input$rad_trnsFea04)
    })
    
    
    
    
    ## Dynamic UI to display inputs----------------------------------------
    ### Character string objects for label argument
    varViz_feat<-"Please select a variable to visualize"
    varSel_feat<-"Please select which variables for ordinal encoding"
    varSelOrd_feat<-c("Rank all from least to most important"="")
    scaleOpt_feat<-"Please select the type of scaling for the list of numerical variables"
    
    
    ### Normalization/standardization--------------------
    #### Input to select var to visualize, either unscaled or scaled
    output$ui_sel_scale1_trnsFea04<-renderUI({
      req(input$rad_trnsFea04)
      selectInput01(id="sel_scale1_trnsFea04",label=varViz_feat,
                    #dynamically select numeric vars (NOTE: will need to update data object later)
                    choices=trainDF_nvI() %>% select(where(is.numeric)) %>% names())
    })
    
    
    ### Input to select how to transform/scale selected variables
    output$ui_sel_scale2_trnsFea04<-renderUI({
      req(input$sel_scale1_trnsFea04)
      selectInput01(id="sel_scale2_trnsFea04",label=scaleOpt_feat,
                    choices=ch_trans_opt_featTrans)
    })
    
    #### Button to confirm selection
    output$ui_btn_scale_trnsFea04<-renderUI({
      req(input$rad_trnsFea04,input$sel_scale1_trnsFea04,input$sel_scale2_trnsFea04)
      actionButton(inputId="btn_scale_trnsFea04",label="Confirm your selection") 
    })
    
    #### Button to confirm scaling selections and create new columns/variables
    trainDF_nvI_s<-eventReactive(input$btn_scale_trnsFea04, {
      switch(input$sel_scale2_trnsFea04,
             #raw = unchanged
             raw=trainDF_nvI(),
             #log = log-transform + identifier
             log=trainDF_nvI() %>% 
               mutate(across(where(is.numeric),~log(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")),
             #mm_scale = min-max scale + identifier
             mm_scale=trainDF_nvI() %>%
               mutate(across(where(is.numeric),~min_max_scaler(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")), 
             #standize = standardized + identifier
             standize=trainDF_nvI() %>%
               mutate(across(where(is.numeric),~standardizer(.x),.names="{.col}_scale")) %>%
               select(passenger_id,ends_with("scale")) 
      )
    })
    
    #Temporary table--proof that above code is working
    output$DT1<-renderTable({
      head(trainDF_nvI_s())
    })
    
    
  
    ### Discretization--------------------
    #### Input to select var to visualize as histogram (for discretization)
    output$ui_sel_dis1_trnsFea04<-renderUI({
      req(input$rad_trnsFea04=="Discretization")
      selectInput01(id="sel_dis1_trnsFea04",label=varViz_feat,
                    #dynamically select numerical variables and num 
                    choices=trainDF_disVars)
    })
    
    #### Input to choose whether to log10-transform x-axis
    output$ui_rad_dis1_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04)
      radioButtons(inputId="rad_dis1_trnsFea04",
                   label="Choose whether to log10-scale the x-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    #### Input to choose number of bins for histogram
    output$ui_num_dis1_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04)
      numericInput(inputId="num_dis1_trnsFea04",
                   label="Select the number of bins for the histogram (2-100)",
                   value=30,min=2,max=100)
    })
    
    #### Output to display text for next set of inputs
    output$text_dis2_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04,input$rad_dis1_trnsFea04)
      h4("Visualization of binned data")
    })
    
    
    #### Input to select a log10-transformed y-axis
    output$ui_rad_dis2a_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04,input$rad_dis1_trnsFea04)
      radioButtons(inputId="rad_dis2a_trnsFea04",
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    #### Input to choose number of breaks
    output$ui_num_dis2a_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04,input$rad_dis1_trnsFea04)
      numericInput(inputId="num_dis2a_trnsFea04",
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2,min=1,max=5)
    })
    
    #### Input to choose whether to have R or user-selected bin boundaries
    output$ui_rad_dis2b_trnsFea04<-renderUI({
      req(input$sel_dis1_trnsFea04,input$rad_dis1_trnsFea04)
      radioButtons(inputId="rad_dis2b_trnsFea04",
                   label="Choose who selects the bin boundaries",
                   choices=c("R","me"),selected=character(0),inline=TRUE)
    })
    
    
  
    
    
    #### Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
    output$ui_num_dis2b_trnsFea04<-renderUI({
      req(input$rad_dis2a_trnsFea04,input$rad_dis2b_trnsFea04=="me")
      tags_num<-tagList()
      for(i in seq_len(input$num_dis2a_trnsFea04)){
        tags_num[[i]]<-numericInput(paste0("n",i),paste0("Break",i),min=0,value=NULL)
      }
      tags_num
    })
    
  
  
    #### Dynamically display action button (and associated text) to discretize variable 
    #display text for action buttons
    output$text_dis3a_trnsFea04<-renderUI({
      req(!is.na(input$rad_dis1_trnsFea04))
      h4(paste0("I am not interested in discretizing ",input$sel_dis1_trnsFea04,"."))
    })
    
    output$text_dis3b_trnsFea04<-renderUI({
      #either "R" is selected or "me" is selected and the number and every break point input is populated
      req((!is.na(input$rad_dis2a_trnsFea04) & input$rad_dis2b_trnsFea04=="R")|
         (input$rad_dis2b_trnsFea04=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a_trnsFea04)
      )
      h4(paste("Click confirm to discretize",input$sel_dis1_trnsFea04, "using these settings."))
    })
    
    #display buttons
    output$ui_btn_dis3a_trnsFea04<-renderUI({
      req(!is.na(input$rad_dis1_trnsFea04))
      switch(input$sel_dis1_trnsFea04,
        age=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        room_service=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        food_court=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        shopping_mall=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        spa=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        vr_deck=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        num=actionButton(paste("btn_dis3a",input$sel_dis1_trnsFea04,sep="_"),label="Confirm")
      )
    })
    
    output$ui_btn_dis3b_trnsFea04<-renderUI({
      req((!is.na(input$rad_dis2a_trnsFea04) & input$rad_dis2b_trnsFea04=="R")|
            (input$rad_dis2b_trnsFea04=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a_trnsFea04)
      )
      switch(input$sel_dis1_trnsFea04,
        age=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        room_service=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        food_court=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        shopping_mall=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        spa=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        vr_deck=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm"),
        num=actionButton(paste("btn_dis3b",input$sel_dis1_trnsFea04,sep="_"),label="Confirm")
      )
    })
    
  
    
    
    ### Ordinal Encoding--------------------
    #### Select variable to visualize
    output$ui_sel_ordEnc1_trnsFea04<-renderUI({
      selectInput01(id="sel_ordEnc1_trnsFea04",label=varViz_feat,
                    #dynamically select factors 
                    choices=trainDF_nvI() %>% select(where(is.factor),-num) %>% names())
    })
    
    # Dynamically display text above checkboxes below
    output$text_ordEnc_trnsFea04<-renderUI({
      req(input$rad_ordEnc_trnsFea04=="Yes") 
      h4("Check each variable for ordinal encoding and rank the categories from least to most important")
    })
    
    
    #### Dynamically create checkboxes to choose variables for ordinal encoding
      output$ui_chk_ordEnc2a_trnsFea04<-renderUI({
        req(input$rad_ordEnc_trnsFea04=="Yes") 
        checkboxInput(inputId="chk_ordEnc2a_trnsFea04",label="ticket",value=FALSE)
      })
      output$ui_chk_ordEnc2b_trnsFea04<-renderUI({
        req(input$rad_ordEnc_trnsFea04=="Yes") 
        checkboxInput(inputId="chk_ordEnc2b_trnsFea04",label="home_planet",value=FALSE)
      })
      output$ui_chk_ordEnc2c_trnsFea04<-renderUI({
        req(input$rad_ordEnc_trnsFea04=="Yes") 
        checkboxInput(inputId="chk_ordEnc2c_trnsFea04",label="deck",value=FALSE)
      })
      output$ui_chk_ordEnc2d_trnsFea04<-renderUI({
        req(input$rad_ordEnc_trnsFea04=="Yes") 
        checkboxInput(inputId="chk_ordEnc2d_trnsFea04",label="side",value=FALSE)
      })
      output$ui_chk_ordEnc2e_trnsFea04<-renderUI({
        req(input$rad_ordEnc_trnsFea04=="Yes") 
        checkboxInput(inputId="chk_ordEnc2e_trnsFea04",label="destination",value=FALSE)
      })
  
      
    #### Dynamically create selectors for ordinal encoding
    output$ui_sel_ordEnc2a_trnsFea04<-renderUI({
      req(input$chk_ordEnc2a_trnsFea04)
      selectizeInput(inputId="sel_ordEnc2a_trnsFea04",label="", multiple=TRUE,
                    choices=c(varSelOrd_feat,
                            trainDF_nvI()[["ticket"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2b_trnsFea04<-renderUI({
      req(input$chk_ordEnc2b_trnsFea04)
      selectizeInput(inputId="sel_ordEnc2b_trnsFea04",label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               trainDF_nvI()[["home_planet"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2c_trnsFea04<-renderUI({
      req(input$chk_ordEnc2c_trnsFea04)
      selectizeInput(inputId="sel_ordEnc2c_trnsFea04",label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               trainDF_nvI()[["deck"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2d_trnsFea04<-renderUI({
      req(input$chk_ordEnc2d_trnsFea04)
      selectizeInput(inputId="sel_ordEnc2d_trnsFea04",label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               trainDF_nvI()[["side"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2e_trnsFea04<-renderUI({
      req(input$chk_ordEnc2e_trnsFea04)
      selectizeInput(inputId="sel_ordEnc2e_trnsFea04",label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               trainDF_nvI()[["destination"]] %>% levels()))
    })
    
    
    #### Dynamically display button
    output$ui_btn_ordEnc2_trnsFea04<-renderUI({
      n_ticket<-nlevels(trainDF_nvI()[["ticket"]])
      n_home_planet<-nlevels(trainDF_nvI()[["home_planet"]])
      n_deck<-nlevels(trainDF_nvI()[["deck"]])
      n_side<-nlevels(trainDF_nvI()[["side"]])
      n_destination<-nlevels(trainDF_nvI()[["destination"]])
      
      #button displays if 1) "No" selected in radio button; 2) at least one box is checked AND for each var either
        #1) box unchecked or all categories selected 
      req(input$rad_ordEnc_trnsFea04=="No"|(
        sum(length(input$sel_ordEnc2a_trnsFea04)==n_ticket,
            length(input$sel_ordEnc2b_trnsFea04)==n_home_planet, 
            length(input$sel_ordEnc2c_trnsFea04)==n_deck,
            length(input$sel_ordEnc2d_trnsFea04)==n_side,
            length(input$sel_ordEnc2e_trnsFea04)==n_destination) > 0 & (
        (length(input$sel_ordEnc2a_trnsFea04)==n_ticket|input$chk_ordEnc2a_trnsFea04==FALSE) &
        (length(input$sel_ordEnc2b_trnsFea04)==n_home_planet|input$chk_ordEnc2b_trnsFea04==FALSE) &
        (length(input$sel_ordEnc2c_trnsFea04)==n_deck|input$chk_ordEnc2c_trnsFea04==FALSE) &
        (length(input$sel_ordEnc2d_trnsFea04)==n_side|input$chk_ordEnc2d_trnsFea04==FALSE) &
        (length(input$sel_ordEnc2e_trnsFea04)==n_destination|input$chk_ordEnc2e_trnsFea04==FALSE)
        )
        )
      )
      actionButton(inputId="btn_ordEnc2_trnsFea04",label="Confirm all ordinal encoding selections")
    })
    
  
    ### Rare Label Encoding--------------------
    #### Input to select var to visualize as a barplot
    output$ui_sel_rareEnc1a_trnsFea04<-renderUI({
      selectInput01(id="sel_rareEnc1a_trnsFea04",label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=trainDF_nvI() %>% select(deck,ticket) %>% names())
    })
    
    # Input to select levels to combine as a category and visualize in a new barplot (NAs are off limits)
    output$ui_sel_rareEnc1b_trnsFea04<-renderUI({
      req(input$sel_rareEnc1a_trnsFea04)
      selectizeInput(inputId="sel_rareEnc1b_trnsFea04",label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               trainDF_nvI() %>% 
                                 pull(input$sel_rareEnc1a_trnsFea04) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Input to select other var to visualize as a barplot
    output$ui_sel_rareEnc2a_trnsFea04<-renderUI({
      req(length(input$sel_rareEnc1b_trnsFea04)>1)
      selectInput01(id="sel_rareEnc2a_trnsFea04",label=varViz_feat,
                    #dynamically ticket and deck 
                    choices=trainDF_nvI() %>% select(deck,ticket,-input$sel_rareEnc1a_trnsFea04) %>% names())
    })
    
    #### Input to select levels to combine as a category and visualize in a new barplot
    output$ui_sel_rareEnc2b_trnsFea04<-renderUI({
      req(input$sel_rareEnc2a_trnsFea04)
      selectizeInput(inputId="sel_rareEnc2b_trnsFea04",label="",multiple=TRUE,
                     choices=c("Choose at least two"="",
                               trainDF_nvI() %>% 
                                 pull(input$sel_rareEnc2a_trnsFea04) %>% 
                                 unique() %>%
                                 sort() %>%
                                 as.character()))
    })
    
    #### Button to confirm selections
    output$ui_btn_rareEnc_trnsFea04<-renderUI({
      req(input$sel_rareEnc1a_trnsFea04)
      actionButton(inputId="btn_rareEnc_trnsFea04",label="Confirm selections")
    })
  
    
    
    
    
    ## Conditional output for feature extraction----------------------------------------
    ### Normalization/standardization--------------------
    #### Display set of plots
    output$plot_sel_scale1_trnsFea04<-renderPlot({
      req(input$sel_scale1_trnsFea04)
      cowplotter(trainDF_nvI(),input$sel_scale1_trnsFea04)
    })
  
    
    ### Discretization--------------------
    #### Plot raw data with fill=transported as histogram
    output$plot_sel_dis1_trnsFea04<-renderPlot({
      req(input$rad_dis1_trnsFea04)
      switch(input$sel_dis1_trnsFea04,
        age=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        room_service=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        food_court=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        shopping_mall=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        spa=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        vr_deck=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04),
        num=histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
          n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04)
      )
    })
    
    
    #### Plot numerical var in bins filled by transported either using R or user specified break values
    #create reactive vectors of cut locations
    user_cuts<-reactive({
      c(input$n1,input$n2,input$n3,input$n4,input$n5)
    })
    
    R_cuts<-reactive({
      layer_scales(plot_dis_trnsFea04())$x$breaks
    })
    
  
    
    #use switch to create plot object
    plot_dis_trnsFea04<-reactive({
      req(input$rad_dis2a_trnsFea04, input$rad_dis2b_trnsFea04)
      switch(input$rad_dis2b_trnsFea04,
             R=bin_plotter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,num.breaks=input$num_dis2a_trnsFea04,
                           y.log.scale=input$rad_dis2a_trnsFea04),
             me=user_bin_plotter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts(),
                                 y.log.scale=input$rad_dis2a_trnsFea04)
      )
    })
    
    #plot the plot object
    output$plot_sel_dis2_trnsFea04<-renderPlot({
      plot_dis_trnsFea04()
    })
    
    
    
    #### Create reactiveValues object and initialize with NULL values
    #trainDF_dis_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,vr_deck=NULL,num=NULL)
    # trainDF_dis_list<-reactiveValues()
    trainDF_dis_list<-vector(mode="list")
  
   
    
    # Update reactiveValues object with passenger_id values as a tibble when selector input is used
    # observeEvent(input$rad_trnsFea04, {
    #   
    # }
    # })
    
   # trainDF_dis_list<-reactiveValues(age=NULL)
   #  
   #  trainDF_dis_list$age<-reactive({trainDF_nvI() %>% select(passenger_id)})
  
    # 
    # trainDF_dis_list$room_service<-reactive({
    #   trainDF_nvI() %>% select(passenger_id) 
    # })
    # 
    # trainDF_dis_list$food_court<-eventReactive(input$rad_trnsFea04, {
    #   trainDF["passenger_id"]
    # })
    # 
    # trainDF_dis_list$shopping_mall<-eventReactive(input$rad_trnsFea04, {
    #   trainDF["passenger_id"]
    # })
    # 
    # trainDF_dis_list$spa<-eventReactive(input$rad_trnsFea04, {
    #   trainDF["passenger_id"]
    # })
    # 
    # trainDF_dis_list$vr_deck<-eventReactive(input$rad_trnsFea04, {
    #   trainDF["passenger_id"]
    # })
    # 
    # trainDF_dis_list$num<-eventReactive(input$rad_trnsFea04, {
    #   trainDF["passenger_id"]
    # })
  
    
  
    
    # trainDF_dis_list<-reactiveValues(age=NULL)
    
    #trainDF_dis_list$room_service<-tibble(x="test")
    
   # trainDF_dis_list<-reactiveValues(age=trainDF["passenger_id"],
   #                         room_service=trainDF["passenger_id"])
   
                           # food_court=trainDF["passenger_id"],
                           # shopping_mall=trainDF["passenger_id"],
                           # spa=trainDF["passenger_id"],
                           # vr_deck=trainDF["passenger_id"],
                           # num=trainDF["passenger_id"])
    
    # trainDF_dis_list<-reactiveValues(age=NA,room_service=NA,food_court=NA,shopping_mall=NA,spa=NA,
    #                                  vr_deck=NA,num=NA)
    
    # Confirm discretization (for each variable)
    #age
    # trainDF_dis_list$age<-eventReactive(input$btn_dis3a_age, {
    #   trainDF_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # trainDF_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    
  
    observeEvent(input$sel_dis1_trnsFea04e, {
      trainDF_dis_list$age<-trainDF %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_age, {
      req(input$rad_dis2b_trnsFea04)
      if(input$rad_dis2b_trnsFea04=="R"){
        trainDF_dis_list$age<-cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts())
      }
      else if(input$rad_dis2b_trnsFea04=="me"){
        trainDF_dis_list$age<-cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
      }
    })
    
    
    observeEvent(input$sel_dis1_trnsFea04, {
      trainDF_dis_list$room_service<-trainDF %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_room_service, {
      req(input$rad_dis2b_trnsFea04)
      if(input$rad_dis2b_trnsFea04=="R"){
        trainDF_dis_list$room_service<-cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts())
      }
      else if(input$rad_dis2b_trnsFea04=="me"){
        trainDF_dis_list$room_service<-cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
      }
    })
    
    # trainDF_dis_list$age_pi<-eventReactive(input$btn_dis3a_age, {
    #   trainDF_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # trainDF_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    
    # trainDF_dis_list$age<-reactive({
    #   if(input$rad_trnsFea04=="Discretization" & !exists("rad_dis2b_trnsFea")){
    #     trainDF_nvI() %>% select(passenger_id)}
    #   if(input$sel_dis1_trnsFea04=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b_trnsFea04=="R"){
    #     cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts())
    #     }
    #   if(input$sel_dis1_trnsFea04=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b_trnsFea04=="me"){
    #     cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #     }
    # })
  
    
  
    #room_service
    # trainDF_dis_list$room_service<-eventReactive(list(input$rad_trnsFea04,input$btn_room_service), {
    #   rs1<-trainDF["passenger_id"]
    #   rs2<- switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    
    # trainDF_dis_list$room_service<-eventReactive(input$btn_room_service, {
      # switch(input$rad_dis2b_trnsFea04,
      #        R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
      #        me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
      # )
    # })
    
    # reax_room_service<-eventReactive(input$btn_room_service, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    
    
    # #food_court
    # trainDF_dis_list$food_court<-eventReactive(input$btn_food_court, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    # 
    # #shopping_mall
    # trainDF_dis_list$shopping_mall<-eventReactive(input$btn_shopping_mall, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    # 
    # #spa
    # trainDF_dis_list$spa<-eventReactive(input$btn_spa, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    # 
    # #vr_deck
    # trainDF_dis_list$vr_deck<-eventReactive(input$btn_vr_deck, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    # 
    # #num
    # trainDF_dis_list$num<-eventReactive(input$btn_num, {
    #   switch(input$rad_dis2b_trnsFea04,
    #          R=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=R_cuts()),
    #          me=cutter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=user_cuts())
    #   )
    # })
    
    #train_list<-reactiveValuesToList(trainDF_dis_list)
    
    
    
    
  # Combine all list elements into a new reactive DF
  
    
    # trainDF_nvI_d<-eventReactive(input$btn_age,{
    #   trainDF_nvI() %>%
    #     select(passenger_id) %>%
    #     {if(ncol(trainDF_dis_list$age())==2)(left_join(.,trainDF_dis_list$age(),by="passenger_id")) else .} #%>%
    #     {if(ncol(trainDF_dis_list$room_service())==2)(left_join(.,trainDF_dis_list$room_service(),by="passenger_id")) else .} %>%
    #     {if(ncol(trainDF_dis_list$food_court())==2)(left_join(.,trainDF_dis_list$food_court(),by="passenger_id")) else .} #%>%
    # #     # {if(input$btn_shopping_mall==1)(left_join(.,trainDF_dis_list$shopping_mall(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_spa==1)(left_join(.,trainDF_dis_list$spa(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_vr_deck==1)(left_join(.,trainDF_dis_list$vr_deck(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_num==1)(left_join(.,trainDF_dis_list$num(),by="passenger_id")) else .}
    # })
    
  
    
  
    
    
    # trainDF_nvI_d<-reactive({
    #   reduce(list(trainDF_dis_list$age,
    #               trainDF_dis_list$room_service),
    # # # #               # trainDF_dis_list$food_court(),
    # # # #               # trainDF_dis_list$shopping_mall(),
    # # # #               # trainDF_dis_list$spa(),
    # # # #               # trainDF_dis_list$vr_deck(),
    # # #               # trainDF_dis_list$num()),
    #          left_join,by="passenger_id")
    # })
    
    
  
    
    # trainDF_nvI_d<-reactive({
    #   reduce(trainDF_dis_list,left_join,by="passenger_id")
    # })
      
    # trainDF_nvI_d<-reactive({
    #     trainDF_dis_list$age %>%
    #     left_join(trainDF_dis_list$room_service,by="passenger_id") #%>%
    #     # left_join(trainDF_dis_list$food_court,by="passenger_id") %>%
    #     # left_join(trainDF_dis_list$shopping_mall,by="passenger_id") %>%
    #     # left_join(trainDF_dis_list$spa,by="passenger_id") %>%
    #     # left_join(trainDF_dis_list$vr_deck,by="passenger_id") %>%
    #     # left_join(trainDF_dis_list$num,by="passenger_id")
    # })
      
        #purrr::when(sum(!is.na(trainDF_dis_list$age()))>0 ~left_join(.,trainDF_dis_list$age(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(trainDF_dis_list$room_service()))>0 ~left_join(.,trainDF_dis_list$room_service(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(trainDF_dis_list$food_court()))>0 ~left_join(.,trainDF_dis_list$food_court(),by="passenger_id"), ~.) #%>%
        #{if(!is.na(trainDF_list$shopping_mall))(left_join(.,trainDF_dis_list$shopping_mall(),by="passenger_id")) else .} #%>%
      # {if(!is.null(trainDF_dis_list$spa()))(left_join(.,trainDF_dis_list$spa(),by="passenger_id")) else .} %>%
      # {if(!is.null(trainDF_dis_list$vr_deck()))(left_join(.,trainDF_dis_list$vr_deck(),by="passenger_id")) else .} %>%
      # {if(!is.null(trainDF_dis_list$num()))(left_join(.,trainDF_dis_list$num(),by="passenger_id")) else .}
    #})
    
    
    # toy_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,
    #                   c               vr_deck=NULL,num=NULL)
    
    # 
    # trainDF %>%
    #   select(passenger_id) %>%
    #   #left_join(trainDF[c("passenger_id","age")],by="passenger_id")
    #   {if(is.null(samp))(left_join(.,trainDF[c("passenger_id","age")],by="passenger_id")) else .} %>%
    #   {if(is.null(dog))(left_join(.,trainDF[c("passenger_id","spa")],by="passenger_id")) else .}
    # 
    # trainDF %>%
    #   select(passenger_id) %>%
    #   #left_join(trainDF[c("passenger_id","age")],by="passenger_id")
    #   purrr::when(is.null(samp) ~left_join(.,trainDF[c("passenger_id","age")],by="passenger_id"), ~.)
    
    #Convert reactiveValues to list
    #trainDF_dis_list<-reactiveValuesToList(trainDF_dis_list)
    
    
    
    # Check whether results working (by variable)
    #age
    output$temp_table_dis1_trnsFea04<-renderTable({
      trainDF_dis_list$age %>% head()
    })
  
    # # #room_service
    output$temp_table_dis2_trnsFea04<-renderTable({
      trainDF_dis_list$room_service %>% head()
    })
    # 
    # 
    # # #food_court
    # output$temp_table_dis3_trnsFea04<-renderTable({
    #   trainDF_dis_list$food_court() %>% head()
    # })
    # 
    # # #shopping_mall
    # output$temp_table_dis4_trnsFea04<-renderTable({
    #   trainDF_dis_list$shopping_mall() %>% head()
    # })
    # 
    # #spa
    # output$temp_table_dis5_trnsFea04<-renderTable({
    #   trainDF_dis_list$spa() %>% head()
    # })
    # 
    # #vr_deck
    # output$temp_table_dis6_trnsFea04<-renderTable({
    #   trainDF_dis_list$vr_deck() %>% head()
    # })
    # 
    # #num
    # output$temp_table_dis7_trnsFea04<-renderTable({
    #   trainDF_dis_list$num() %>% head()
    # })
    
    # output$temp_table_dis8_trnsFea04<-renderTable({
    #   trainDF_nvI_d() %>% head()
    # })
  
    
    # Connect reactive DFs together
    #list_dis_trnsFea04<-vector(mode="list",length=7)
    # list_dis_trnsFea04<-reactiveValuesToList(c(trainDF_nvI_da(),trainDF_nvI_dr(),trainDF_nvI_df(),trainDF_nvI_dm(),
    #                                          trainDF_nvI_ds(),trainDF_nvI_dv(),trainDF_nvI_dn()))
    # 
    
   # trainDF_nvI_d<-reactive({
   #   trainDF_nvI_da() %>%
   #   left_join(trainDF_nvI_dr(),by=passenger_id) %>%
   #     left_join(trainDF_nvI_df(),by=passenger_id) %>%
   #     left_join(trainDF_nvI_dm(),by=passenger_id) %>%
   #     left_join(trainDF_nvI_ds(),by=passenger_id) %>%
   #     left_join(trainDF_nvI_dv(),by=passenger_id) %>%
   #     left_join(trainDF_nvI_dv(),by=passenger_id) %>%
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
    output$plot_sel_ordEnc1_trnsFea04<-renderPlot({
      req(input$sel_ordEnc1_trnsFea04)
      barplotter(trainDF_nvI(),input$sel_ordEnc1_trnsFea04)
    })
    
    
    #### Display text associated with each variable 
    output$text_sel_ordEnc1_trnsFea04<-renderUI({
      req(input$sel_ordEnc1_trnsFea04)
      switch(input$sel_ordEnc1_trnsFea04,
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
    trainDF_nvI_o<-eventReactive(input$btn_ordEnc2_trnsFea04, {
        trainDF_nvI() %>%
          #choose all factors except num
          mutate(across(.cols=trainDF_fct_nonumVars,~as.ordered(.x))) %>%
            #if...else statements for whether to change factor level order based on if checkbox checked
            {if(input$chk_ordEnc2a_trnsFea04==TRUE)
              mutate(.,ticket_ord=fct_relevel(ticket,input$sel_ordEnc2a_trnsFea04))
              else .} %>%
            {if(input$chk_ordEnc2b_trnsFea04==TRUE)
              mutate(.,home_planet_ord=fct_relevel(home_planet,input$sel_ordEnc2b_trnsFea04))
              else .} %>%
            {if(input$chk_ordEnc2c_trnsFea04==TRUE)
              mutate(.,deck_ord=fct_relevel(deck,input$sel_ordEnc2c_trnsFea04))
              else .} %>%
            {if(input$chk_ordEnc2d_trnsFea04==TRUE)
              mutate(.,side_ord=fct_relevel(side,input$sel_ordEnc2d_trnsFea04))
              else .} %>%
             {if(input$chk_ordEnc2e_trnsFea04==TRUE)
              mutate(.,destination_ord=fct_relevel(destination,input$sel_ordEnc2e_trnsFea04))
              else .} %>%
          #retain passenger_id and mutated cols
          select(passenger_id,ends_with("_ord"))
      })
    
    #### Print temp table as a check
    output$temp_table_trnsFea04<-renderTable({
      trainDF_nvI_o() %>% head()
    })
  
    
    
    ### Rare Label Encoding--------------------
    #### Display plots
    #var1-raw
    output$plot_sel_rareEnc1a_trnsFea04<-renderPlot({
      req(input$sel_rareEnc1a_trnsFea04)
      rare_enc_barplotter(trainDF_nvI(),input$sel_rareEnc1a_trnsFea04)
    })
    
    #var1-combined categories
    output$plot_sel_rareEnc1b_trnsFea04<-renderPlot({
      req(length(input$sel_rareEnc1b_trnsFea04)>1)
      rare_enc_barplotter(trainDF_nvI(),var=input$sel_rareEnc1a_trnsFea04,cats=input$sel_rareEnc1b_trnsFea04)
    })
    
    #var2-raw
    output$plot_sel_rareEnc2a_trnsFea04<-renderPlot({
      req(input$sel_rareEnc2a_trnsFea04)
      rare_enc_barplotter(trainDF_nvI(),input$sel_rareEnc2a_trnsFea04)
    })
    
    #var2-combined categories
    output$plot_sel_rareEnc2b_trnsFea04<-renderPlot({
      req(length(input$sel_rareEnc2b_trnsFea04)>1)
      rare_enc_barplotter(trainDF_nvI(),var=input$sel_rareEnc2a_trnsFea04,cats=input$sel_rareEnc2b_trnsFea04)
    })
    
    # Extract features via rare label encoding
    trainDF_nvI_r<-eventReactive(input$btn_rareEnc_trnsFea04, {
      trainDF_nvI() %>%
        {if(length(input$sel_rareEnc1b_trnsFea04)>=2) 
          #paste variable name using !! and :=
          mutate(.,!!paste0(input$sel_rareEnc1a_trnsFea04,"_rare") := fct_collapse(!!sym(input$sel_rareEnc1a_trnsFea04),
                                                                                other=input$sel_rareEnc1b_trnsFea04))
          else .} %>%
        {if(length(input$sel_rareEnc2b_trnsFea04)>=2)
          mutate(.,!!paste0(input$sel_rareEnc2a_trnsFea04,"_rare") := fct_collapse(!!sym(input$sel_rareEnc2a_trnsFea04),
                                                                                other=input$sel_rareEnc2b_trnsFea04))
          else .} %>%
        #retain cols of interest
        select(passenger_id,ends_with("_rare")) 
    })
    
    #### Output temp table
    output$temp_table_rareEnc_trnsFea04<-renderTable({
      trainDF_nvI_r() %>% head()
    })
  
    # ### Update data frame
    # trainDF_nvI_eF<-reactive({
    #   req(input$chk_tranFea04)
    #   #insert joins here
    # })
    
    
  })
}

