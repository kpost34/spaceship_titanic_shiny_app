#### Load packages
pacman::p_load(shiny,shinyjs,conflicted,here,tidyverse,janitor,DT,visdat,finalfit,skimr,GGally,rstatix,naniar,mice,cowplot)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


#### Load functions
source(here("functions and modals","spaceship_titanic_app_func_ui.R"))
source(here("functions and modals","spaceship_titanic_app_func_01.R"))
source(here("functions and modals","spaceship_titanic_app_func_02.R"))
source(here("functions and modals","spaceship_titanic_app_func_03.R"))


#### Read in and clean data
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ### reclassify vars
  mutate(across(c(ticket,home_planet,deck:destination),~as.factor(.x))) -> trainDF


#### Create vectors
### Col names
## All character cols
trainDF %>% select(where(is.character)) %>% names() -> trainDF_chrVars
## All cols but character
trainDF %>% select(!where(is.character)) %>% names() -> trainDF_nchrVars
#excluding dep var
trainDF_nchrVars[trainDF_nchrVars!="transported"] -> trainDF_nchrPreds

## All logical and factor cols
trainDF %>% select(where(is.logical)|where(is.factor)) %>% names() -> trainDF_catVars

## All numeric cols
trainDF %>% select(where(is.numeric)|where(is.integer)) %>% names() -> trainDF_numVars

## Cabin component cols
cabinVars<-c("deck","num","side")

## Dependent variable
depVar<-"transported"

## choices vectors
Chk01_quickVec<-c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")
Chk01_summVec<-c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")
namMis03_expVec<-c("missing example"="miss_samp","non-missing example"="nmiss_samp","summary table"="sum_tab","bar plot"="plot")
namMis03_impOptVec<-c("drop name columns"="drop_cols",
                      "remove with rows with missing names"="remove_rows",
                      "populate using passenger group"="imp_pass_group",
                      "populate using cabin info"="imp_cabin")
nchrMis03_expVec<-c("missing values occurrences"="miss_occur",
                   "missing values per variable" = "miss_var",
                   "missing values per observation" = "miss_obs",
                   "missing pattern"="miss_patt")
nchrMis03_impVec<-c("retain complete cases only"="lwise_del",
                    "remove variable(s) missing data"="var_del",
                    "mean imputation (numeric vars only)" = "mean_imp")
trnsFea04_transVec<-c("Feature Scaling",
                      "Discretization",
                      "Ordinal Encoding",
                      "Rare Label Encoding")
trnsFea04_transOptVec<-c("leave unchanged"="raw",
                          "log transform"="log",
                          "min-max scale"="mm_scale",
                          "standardize"="standize")
creFea04_grpSizeVec<-c("ticket group size (same passenger group)"="ticket_group",
                       "family size (passenger group & last name)"="family",
                       "travel party size (cabin)"="travel_party")
creFea04_luxVec<-c("room_service","food_court","shopping_mall","spa","vr_deck")
                       




#NAMING FORMULAS
#inputs: [abbrvInput]_[abbrvTask]_[Tabname]
#e.g., sel_quick_Chk01 (from Chk011 tab): sel = selectInput, quick = "Quick data check", chk01
#outputs: [abbrOutput]_[abbrvInput]_[# if 2+]_[tabName]
#e.g., tab_sel_Chk01 (from Chk01 tab): tab = table, sel = selectInput, chk01 (if 2 tables then append 1, 2, etc to before tabName)
#i.e., tab_[inputNameFormula]

ui<-navbarPage(title="Spaceship Titanic Shiny App", id="mainTab",position="static-top",
  #### 0: Title page and intro====================================================================================================
  # tabPanel(title="Intro",id="intro_00",
  #          numericInput(inputId = "numInput",label="pick a number",value=2),
  #          textOutput("textout1")
  #          )
    ##option to skip intro-> advances to tab/menu 1
    ## input name ... Hello, x. Your mission is....
    ##preview
  #### 1: Menu-Data Checking======================================================================================================
  tabPanel(title="Data Check",id="Chk01",
    sidebarLayout(
      sidebarPanel(width=3,
        selectInput01(id="sel_quick_Chk01",label="Quick data check",choices=Chk01_quickVec),
        linebreaks(2),
        selectInput01(id="sel_summ_Chk01",label="Data summaries",choices=Chk01_summVec),
      ),
      mainPanel(width=9,
        DTOutput("tab_sel_quick_Chk01"),
        br(),
        DTOutput("tab_sel_summ_Chk01")
      )
    )
  ),

  #### 2: Menu-EDA================================================================================================================
  navbarMenu(title="EDA",menuName="EDA02",
    #tab 1: univariate EDA--------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Univariate",tabID="uniEDA02",varID=c("var1","var2"),options=trainDF_nchrVars,fn=selectInput01),
    
    #tab 2: bivariate EDA---------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Bivariate",tabID="biEDA02",varID=c("var12","var34"),options=trainDF_nchrVars,fn=selectizeInput01),
    
    #tab 3: multivariate EDA-------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Multivariate",tabID="mulEDA02",varID=c("var123","var456"),options=trainDF_nchrVars,fn=selectizeInput02)
  ),


  #### 3: Menu-Missing Data=======================================================================================================
  navbarMenu(title="Missingness",menuName="Mis03",
    #tab 1: names-----------------------------------------------------------------------------------------------------------------
    tabPanel(title="Names", id="namMis03",
      titlePanel("Names and Missingness"),
      sidebarLayout(
        sidebarPanel(
          #exploring missing names
          h4("Did you notice that some passengers did not have names? If not, take a closer look"),
          selectInput01(id="sel_exp_namMis03",label="",choices=namMis03_expVec),
          br(),
          #go deeper with some possibilities
          h4("Two hundred out of 8693 passengers (in the training data) lack names. That's 2.3%. Although first names, and thus
             full names will be impossible to impute from the other variables. Last names may be populated with confidence if we
             assume passengers traveled together as families. Two ways to conclude that the traveling party is a family is
             1) purchasing tickets together (same passenger group) or 2) saying in the same room (cabin). Here's how the patterns break down."),
          radioButtons(inputId="rad_grpVar_namMis03",label="",choices=c("passenger_group"="passenger_group",
                                                                        "cabin occupancy"="cabin"),
                       selected=character(0)),
          h4("Note that each group, regardless of group size or grouping variable, has one unnamed passenger."),
          br(),
          h4("Given all this information, how would you like to handle passengers with missing names?"),
          selectInput01(id="sel_impOpt_namMis03",label="",choices=namMis03_impOptVec),
          br(),
          uiOutput("ui_slid_impOpt_namMis03")
        ),
        mainPanel(
          htmlOutput("text_sel_exp_namMis03"),
          DTOutput("tab_sel_exp_namMis03"),
          plotOutput("plot_sel_exp_namMis03"),
          br(),
          htmlOutput("text_rad_grpVar_namMis03"),
          plotOutput("plot_rad_grpVar_namMis03"),
          tableOutput("test_table"),
          tableOutput("test_table2")
        )
      )
    ),
    #tab 2: exploring non-character missingness-----------------------------------------------------------------------------------
    tabPanel(title="Explore Missingness",id="nchrMis03",
      titlePanel("Exploring Other Missing Data"),
      sidebarLayout(
        sidebarPanel(
          h4("Let's visualize missingness in all non-character variables."),
          selectInput01(id="sel_exp_nchrMis03",label="",choices=nchrMis03_expVec),
          br(),
          h4("Which variable pairs exhibit missingness at random (MAR)?. Compare each variable with missing data to the
          remaining set of variables."),
          selectInput01(id="sel_compare_nchrMis03",label="",choices=trainDF_nchrPreds),
          br(),
          #selectInput01(id="sel_imp_nchrMis03",label="",choices=)
        ),
        mainPanel(
          htmlOutput("text_sel_exp_nchrMis03"),
          plotOutput("plot_sel_exp_nchrMis03"),
          br(),
          htmlOutput("text_sel_compare_nchrMis03"),
          DTOutput("tab_sel_compare_nchrMis03")
        )
      )
    )
  ),
  
  #### 4: Menu-Feature Engineering======================================================================================
  navbarMenu(title="Feature Engineering",menuName="Fea04",
    #tab 1: data transformations----------------------------------------------------------------------------------------
    tabPanel(title="Transformations",id="trnsFea04",
      titlePanel("Feature Scaling and Extraction"),
      h4("In this section, you will have the opportuntiy to normalize/standardize numerical data, bin numerical (or 
        character or factor variables) into (smaller) groups,perform ordinal encoding, and group rare categories
        together. What would you like to begin with?"),
      #fluidRow with column helps to align radio buttons
      fluidRow(
        column(6,align="center",offset=3,
          radioButtons(inputId="rad_trnsFea04",label="",choices=trnsFea04_transVec,selected=character(0),
                       inline=TRUE,width="100%")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          #create invisible panel that can be updated
          tabsetPanel(id="sidebar_tab_trnsFea04",type="hidden",
            tabPanelBody("Feature Scaling",
              uiOutput("ui_sel_scale1_trnsFea04"),
              br(),
              uiOutput("ui_sel_scale2_trnsFea04"),
              uiOutput("ui_btn_scale_trnsFea04")
            ),
            tabPanelBody("Discretization",
              uiOutput("ui_sel_dis1_trnsFea04"),
              radioButtons(inputId="rad_dis1_trnsFea04",
                           label="Choose whether to log10-scale the x-axis",
                           choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE),
              numericInput(inputId="num_dis1_trnsFea04",
                           label="Select the number of bins for the histogram (2-100)",
                           value=30,min=2,max=100),
              br(),
              actionButton(inputId="btn_dis2_trnsFea04",label="Visualize binned data?"),
              uiOutput("ui_rad_dis2a_trnsFea04"),
              uiOutput("ui_num_dis2a_trnsFea04"),
              uiOutput("ui_rad_dis2b_trnsFea04"),
              uiOutput("ui_num_dis2b_trnsFea04")
            ),
            tabPanelBody("Ordinal Encoding",
              uiOutput("ui_sel_ordEnc_trnsFea04")
            ),
            tabPanelBody("Rare Label Encoding",
              uiOutput("ui_sel_rareEnc1_trnsFea04"),
              uiOutput("ui_sel_rareEnc2_trnsFea04")
            )
          )
        ),
        mainPanel(
          #set this to dynamically produce tabs
          tabsetPanel(id="main_tab_trnsFea04",type="hidden",
            tabPanelBody("Feature Scaling",
              plotOutput("plot_sel_scale1_trnsFea04",height="1000px"),
              tableOutput("DT1")
            ),
            tabPanelBody("Discretization",
              plotOutput("plot_sel_dis1_trnsFea04"),
              plotOutput("plot_sel_dis2_trnsFea04")
            ),
            tabPanelBody("Ordinal Encoding",
              uiOutput("text_sel_ordEnc_trnsFea04"),
              br(),
              plotOutput("plot_sel_ordEnc_trnsFea04")
            ),
            tabPanelBody("Rare Label Encoding",
              plotOutput("plot_sel_rareEnc1_trnsFea04"),
              br(),
              plotOutput("plot_sel_rareEnc2_trnsFea04")
            )
          )
        )
      )
    ),
        
          
          #set up as update panel framework: given this larger choice then successive uis
          #1) normalizing/standardizing numerical variables
          #2) binning numerical (or even character) variables into groups 
          #3) ordinal encoding: converting categorical variable to ordinal variables (by rank)
          #4) grouping sparse categories together into a separate group (e.g., other)
          
          #1) age, vip, room_service, food_court, shopping_mall, spa, vr_deck
          #2) age, vip, room_service, food_court, shopping_mall, spa, vr_deck, num
          #3) home_planet, deck, side, destination
          #4) to keep things simple: deck or ticket
    
    
    #when to choose which method
    #1) normalizing data will not handle outliers well, but standardization is robust to outliers
    #2) normalization good when data are non-normal, while standardization good when data follow normal dist
    
    
    #tab 2: feature creation--------------------------------------------------------------------------------------------
    tabPanel(title="Feature Creation",id="creFea04",
      titlePanel(title="Feature Creation"),
      h4("Now you have the opportunity to create new features for your model using the existing variables. Let's look
         at some possible options"),
      sidebarLayout(
        sidebarPanel(
          #select input for group size
          selectInput01(id="sel_exp1_creFea04",label="Create a group size variable that uses...",
                        choices=creFea04_grpSizeVec),
          #select input for luxury expenses
          selectizeInput(inputId="sel_exp2_creFea04",label="Create a luxury expense variable that uses the sum of",
                         multiple=TRUE,choices=c("Choose at least two"="",creFea04_luxVec))
        ),
        mainPanel()
      )
    ),
  
  
    #tab 3: feature selection---------------------------------------------------------------------------------------------
    tabPanel(title="Feature Selection",id="selFea04",
      titlePanel(title="Feature Selection"),
      h4("After transforming your data, extracting potential variables, and creating potential variables, you have
        the opportunity to select a final set of variables for modeling. Look at the variables once more before
        making your final"),
      sidebarLayout(
        sidebarPanel(
         #CONSIDER A FUNCTION HERE??
         # #select input for all variables--choose one predictor which will output plots
         # selectInput01(id="sel_exp1_selFea04",label="Create a group size variable that uses...",
         #               choices=""),
         # #select input for final set of variables
         # selectizeInput(inputId="sel_exp2_selFea04",label="Create a luxury expense variable that uses the sum of",
         #                multiple=TRUE,choices=c("Choose at least two"="",creFea04_luxVec))
        ),
        mainPanel()
      )
    )
  )
  
      
  


  
  # #### 5: Tab-Data Partitioning=================================================================================================
  # tabPanel(title="Data Partitioning",id="part_05",
  #   sidebarLayout(
  #     sidebarPanel(),
  #     mainPanel()
  #   )
  # ),
  # 
  # #### 6: Menu-Modeling=================================================================================================
  # navbarMenu(title="Modeling",menuName="mod_06",
  #   tabPanel(title="Model specification", id="mod_spec_06a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Fit models", id="mod_fit_06b",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model assessment", id="mod_assess_06c",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   )
  # ),
  # 
  # #### 7: Menu-Validation & Tuning===========================================================================================
  # navbarMenu(title="Validation & Tuning", menuName="valTune_07",
  #   tabPanel(title="Cross-validate",id="valTune_07a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model tuning", id="valTune_mod_07b",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),  
  #   tabPanel(title="Model selection", id="valTune_sel_07c",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model assessment", id="valTune_assess_07d",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   )
  # ),
  #   
  # #### 8: Tab-Model Testing============================================================================================
  # tabPanel(title="Testing",id="mod_test_08",
  #   sidebarLayout(
  #     sidebarPanel(),
  # downloadUI - give user opportunity to download data to submit to Kaggle
  #     mainPanel()
  #   )
  # )
)



server<-function(input,output,session){
  
  #### Server 0: Intro================================================================================================================
  
  #### Server 1: Data Checking========================================================================================================
  ### Display dims, data sample, or missingness
  dat_check<-reactive({
    switch(input$sel_quick_Chk01,
           dim=dim_tbl(trainDF),
           dat_samp=slice_sample(trainDF,n=5),
           miss=n_miss_tbl(trainDF)
    )
  })

  output$tab_sel_quick_Chk01<-renderDT(
    dat_check(),options=list(scrollX="400px",
                             pageLength=5)
  )

  ### Display data summary by col type
  dat_sum<-reactive({
    switch(input$sel_summ_Chk01,
      chr=skim_tbl(trainDF,type="character"),
      fct=skim_tbl(trainDF,type="factor"),
      lgl=skim_tbl(trainDF,type="logical"),
      num=skim_tbl(trainDF,type="numeric"))
  })

  output$tab_sel_summ_Chk01<-renderDT(
    dat_sum(),options=list(scrollX="400px")
  )

  
  #### Server 2: EDA============================================================================================
  ### Univariate-------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var1_uniEDA02<-renderUI({
    h3(paste(input$sel_var1_uniEDA02))
  })
  
  output$text_sel_var2_uniEDA02<-renderUI({
    h3(paste(input$sel_var2_uniEDA02))
  })
  
  ## Table outputs
  # reactives of output tables
  dat1_uniEDA02<-reactive({
    if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
      summaryize(trainDF,input$sel_var1_uniEDA02)
    }
    else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
      tabylize(trainDF,input$sel_var1_uniEDA02)
    }
  })

  dat2_uniEDA02<-reactive({
    if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
      summaryize(trainDF,input$sel_var2_uniEDA02)
    }
    else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
      tabylize(trainDF,input$sel_var2_uniEDA02)
    }
  })
  
  # Output tables
  output$tab_sel_var1_uniEDA02<-renderDT(
    dat1_uniEDA02(),options=list(scrollX="400px",
                                pageLength=5)
  )

  output$tab_sel_var2_uniEDA02<-renderDT(
    dat2_uniEDA02(),options=list(scrollX="400px",
                                pageLength=5)
  )
  
  
  ## Plot outputs
  output$plot_sel_var1_uniEDA02<-renderPlot({
    if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
      histogrammer(trainDF,input$sel_var1_uniEDA02)
    }
    else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var1_uniEDA02)
    }
  })

  output$plot_sel_var2_uniEDA02<-renderPlot({
    if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
      histogrammer(trainDF,input$sel_var2_uniEDA02)
    }
    else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var2_uniEDA02)
    }
  })
  
  
  ### Bivariate-------------------------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var12_biEDA02<-renderUI({
    h3(paste(input$sel_var12_biEDA02,collapse="-"))
  })
  
  output$text_sel_var34_biEDA02<-renderUI({
    h3(paste(input$sel_var34_biEDA02,collapse="-"))
  })
  
  ## Table outputs
  # Create reactives of output tables
  dat1_biEDA02<-reactive({
    req(length(input$sel_var12_biEDA02)==2)
    #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
    if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
      tabylize(trainDF,input$sel_var12_biEDA02)
    }
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
      summaryize(trainDF,input$sel_var12_biEDA02,input$sel_var12_biEDA02[input$sel_var12_biEDA02 %in% trainDF_catVars])
    }
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_numVars)==2) {
      corrtester(trainDF,input$sel_var12_biEDA02)
    }
  })
  
  
  dat2_biEDA02<-reactive({
    req(length(input$sel_var34_biEDA02)==2)
    #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
    if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
      tabylize(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
      summaryize(trainDF,input$sel_var34_biEDA02,input$sel_var34_biEDA02[input$sel_var34_biEDA02 %in% trainDF_catVars])
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_numVars)==2) {
      corrtester(trainDF,input$sel_var34_biEDA02)
    }
  })
  
  
  # Output tables
  output$tab_sel_var12_biEDA02<-renderDT(
    dat1_biEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
  )
  
  output$tab_sel_var34_biEDA02<-renderDT(
    dat2_biEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
  )

  ## Plot outputs
  output$plot_sel_var12_biEDA02<-renderPlot({
    #must select two inputs first
    req(length(input$sel_var12_biEDA02)==2)
    #if both categorical, then bar plot
    if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
      barplotter(trainDF,input$sel_var12_biEDA02)
    }
    #if 1 cat & 1 num then boxplot
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
      boxplotter(trainDF,input$sel_var12_biEDA02)
    }
    #if two num then scatterplot
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==0) {
      scatterplotter(trainDF,input$sel_var12_biEDA02)
    }
  })
  
  output$plot_sel_var34_biEDA02<-renderPlot({
    req(length(input$sel_var34_biEDA02)==2)
    if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
      barplotter(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
      boxplotter(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==0) {
      scatterplotter(trainDF,input$sel_var34_biEDA02)
    }
  })

  
  ### Multivariate-------------------------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var123_mulEDA02<-renderUI({
    h3(paste(input$sel_var123_mulEDA02,collapse="-"))
  })
  
  output$text_sel_var456_mulEDA02<-renderUI({
    h3(paste(input$sel_var456_mulEDA02,collapse="-"))
  })
  
  
  ## Plot outputs
  output$plot_sel_var123_mulEDA02<-renderPlot({
    #must select three inputs first
    req(length(input$sel_var123_mulEDA02)==3)
    #if all categorical, then bar plot
    if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars)==3) {
      barplotter(trainDF,input$sel_var123_mulEDA02)
    }
    #if 2 cat & 1 num then boxplot
    else if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars)==2) {
      boxplotter(trainDF,input$sel_var123_mulEDA02)
    }
    #if 2-3 num then scatterplot
    else if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars) < 2) {
      scatterplotter(trainDF,input$sel_var123_mulEDA02)
    }
  })
  
  output$plot_sel_var456_mulEDA02<-renderPlot({
    req(length(input$sel_var456_mulEDA02)==3)
    if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars)==3) {
      barplotter(trainDF,input$sel_var456_mulEDA02)
    }
    else if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars)==2) {
      boxplotter(trainDF,input$sel_var456_mulEDA02)
    }
    else if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars) < 2) {
      scatterplotter(trainDF,input$sel_var456_mulEDA02)
    }
  })
  
  

  ##### Server 3: Missing Data====================================================================================================
  #### Names----------------------------------------------------------------------------------------------------------------------
  ### Exploring missing names
  ## Text outputs
  output$text_sel_exp_namMis03<-renderUI({
    #here switch() can be used with all four choices to display the appropriate name
    switch(input$sel_exp_namMis03,
      miss_samp=h3(paste("Sample of Passengers with Missing Names")),
      nmiss_samp=h3(paste("Sample of Passengers with Names")),
      sum_tab=h3(paste("Summary of Missing Names")),
      plot=h3(paste("Plot of Missing Names"))
    )
  })
  
  ## Create reactive object (for tabular output)
  dat1_namMis03<-reactive({
    #reactive is used to build reactive table objects
    switch(input$sel_exp_namMis03,
      miss_samp=trainDF %>% filter(is.na(name)) %>% slice_sample(n=5),
      nmiss_samp=trainDF %>% filter(!is.na(name)) %>% slice_sample(n=5),
      sum_tab=chr_miss_tabler(trainDF)
    )
  })
  

  ## Output table/plot
  # Table output
  output$tab_sel_exp_namMis03<-renderDT(
    dat1_namMis03(),options=list(scrollX="400px")
  )
  
  # Plot output
  output$plot_sel_exp_namMis03<-renderPlot({
    #plot outputs only when selected (note that this is always the same plot)
    req(input$sel_exp_namMis03=="plot")
    chr_miss_boxplotter(trainDF)
  })
  
  
  ### Understanding name missingness conditioned on other variables
  ## Text outputs
  output$text_rad_grpVar_namMis03<-renderUI({
    req(input$rad_grpVar_namMis03)
    switch(input$rad_grpVar_namMis03,
      passenger_group=h3(paste("Summary of Missing Names by Size of Passenger Groups")),
      cabin=h3(paste("Summary of Missing Names by Cabin Occupancy"))
    )
  })
  
  ## Create reactive object (for tabular and plot outputs)
  dat2_namMis03<-reactive({
    switch(input$rad_grpVar_namMis03,
      passenger_group=mis_name_tabler(trainDF,l_name,passenger_group),
      cabin=mis_name_tabler(trainDF,l_name,cabin)
    )
  })
  
  
  ## Output plots
  output$plot_rad_grpVar_namMis03<-renderPlot({
    req(input$rad_grpVar_namMis03)
    col_plotter(dat2_namMis03(),num_name,n)
  })
  
  
  ## Dynamic UI 
  # Display sliders
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
  
  
  ## Create reactive object (for creating a new DF)
  dat3_namMis03<-reactive({
    req(input$sel_impOpt_namMis03 %in% c("imp_pass_group","imp_cabin"))
    switch(input$sel_impOpt_namMis03,
      imp_pass_group=mis_name_tabler(trainDF,l_name,passenger_group),
      imp_cabin=mis_name_tabler(trainDF,l_name,cabin)
    )
  })
  
  ## Create new data frame object after name imputation or col/row removal
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
  
  ## Test whether code above is working
  output$test_table<-renderTable({
    head(trainDF_nI()) 
  })
  
  
  ## Temporary code--to update name of DF
  trainDF_nvI<-reactive({
    trainDF_nI()
  })
  
  ## Test whether code directly above is working
  output$test_table2<-renderTable({
    head(trainDF_nvI()) 
  })
  
  

  
  #### Non-Character Missingness----------------------------------------------------------------------------------------
  ### Exploration
  ## Text output
  output$text_sel_exp_nchrMis03<-renderUI({
    switch(input$sel_exp_nchrMis03,
           miss_occur=h3(paste("Missing Values Occurrences Plot")),
           miss_var=h3(paste("Missing Values per Variable Plot")),
           miss_obs=h3(paste("Missing Values per Observation Plot")),
           miss_patt=h3(paste("Missing Pattern Plot"))
    )
  })
  
  ## Plot output
  output$plot_sel_exp_nchrMis03<-renderPlot({
    switch(input$sel_exp_nchrMis03,
           miss_occur=trainDF_nvI() %>% missing_plot(depVar,trainDF_nchrPreds),
           miss_var=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_var(),
           miss_obs=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_case(),
           miss_patt=trainDF_nvI() %>% select(all_of(trainDF_nchrVars)) %>% gg_miss_upset()
    )
  })

  
  ### Statistical comparisons
  ## Text output
  output$text_sel_compare_nchrMis03<-renderUI({
    req(input$sel_compare_nchrMis03)
    h3(paste("Missing Data Analysis of",input$sel_compare_nchrMis03))
  })
  
  
  ## Table output
  # Create reactive
  dat_nchrMis03<-reactive({
    req(input$sel_compare_nchrMis03)
    if(input$sel_compare_nchrMis03 %in% cabinVars){
      sel_vars<-setdiff(trainDF_nchrVars,cabinVars)
    }
    else{sel_vars<-setdiff(trainDF_nchrVars,input$sel_compare_nchrMis03)}
    missing_compare(trainDF_nvI(),dependent=input$sel_compare_nchrMis03,explanatory=sel_vars
    )
  })


  # Output reactive
  output$tab_sel_compare_nchrMis03<-renderDT(
    dat_nchrMis03(),options=list(scrollX="400px")
  )

  
  ##### Server 4: Features==============================================================================================
  #### Feature Scaling and Extraction-----------------------------------------------------------------------------------
  ### Conditional UI for displaying sidebar tabset panel
  observeEvent(input$rad_trnsFea04, {
    updateTabsetPanel(inputId="sidebar_tab_trnsFea04",selected=input$rad_trnsFea04)
  })
  
  
  ### Conditional UI for displaying main tabset panel
  observeEvent(input$rad_trnsFea04, {
    updateTabsetPanel(inputId="main_tab_trnsFea04",selected=input$rad_trnsFea04)
  })
  
  
  ### Dynamic UI to populate choices using names of reactive data frame
  ## Character string objects for label argument
  varViz_feat<-"Please select a variable to visualize"
  scaleOpt_feat<-"Please select the type of scaling for the list of numerical variables"
  
  ## Normalization/standardization
  # Input to select var to visualize, either unscaled or scaled
  output$ui_sel_scale1_trnsFea04<-renderUI({
    req(input$rad_trnsFea04)
    selectInput01(id="sel_scale1_trnsFea04",label=varViz_feat,
                  #dynamically select numeric vars (NOTE: will need to update data object later)
                  choices=trainDF_nvI() %>% select(where(is.numeric)) %>% names())
  })
  
  
  # Input to select how to transform/scale selected variables
  output$ui_sel_scale2_trnsFea04<-renderUI({
    req(input$sel_scale1_trnsFea04)
    selectInput01(id="sel_scale2_trnsFea04",label=scaleOpt_feat,
                  choices=trnsFea04_transOptVec)
  })
  
  # Button to confirm selection
  output$ui_btn_scale_trnsFea04<-renderUI({
    req(input$rad_trnsFea04,input$sel_scale1_trnsFea04,input$sel_scale2_trnsFea04)
    actionButton(inputId="btn_scale_trnsFea04",label="Confirm your selection") 
  })
  
  # Button to confirm scaling selections and create new columns/variables
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
  
  

  ## Discretization
  # Input to select var to visualize as histogram
  output$ui_sel_dis1_trnsFea04<-renderUI({
    #req(input$rad_trnsFea04)
    selectInput01(id="sel_dis1_trnsFea04",label=varViz_feat,
                  #dynamically select numerical variables and num (NOTE: will need to update data object later)
                  choices=trainDF_nvI() %>% select(where(is.numeric),num) %>% names())
  })

  # Create multiple UIs if action button is pressed
  observeEvent(input$btn_dis2_trnsFea04, {
    #input to select a log10-transformed y-axis
    output$ui_rad_dis2a_trnsFea04<-renderUI({
      radioButtons(inputId="rad_dis2a_trnsFea04",
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    #input to choose number of breaks
    output$ui_num_dis2a_trnsFea04<-renderUI({
      numericInput(inputId="num_dis2a_trnsFea04",
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2,min=1,max=5)
    })
    #input to choose whether to have R or the user selects the bin boundaries
    output$ui_rad_dis2b_trnsFea04<-renderUI({
      radioButtons(inputId="rad_dis2b_trnsFea04",
                   label="Choose who selects the bin boundaries",
                   choices=c("R","me"),selected=character(0),inline=TRUE)
    })
  })
  
  # Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
  output$ui_num_dis2b_trnsFea04<-renderUI({
    req(input$rad_dis2b_trnsFea04=="me")
    tags<-tagList()
    for(i in seq_len(input$num_dis2a_trnsFea04)){
      tags[[i]]<-numericInput(paste0("n",i),
                              paste("Break",i),
                              min=0,value=NULL)
    }
    tags
  })
    
  ## Ordinal Encoding
  output$ui_sel_ordEnc_trnsFea04<-renderUI({
    #req(input$rad_trnsFea04)
    selectInput01(id="sel_ordEnc_trnsFea04",label=varViz_feat,
                  #dynamically select factors (NOTE: will need to update data object later)
                  choices=trainDF_nvI() %>% select(where(is.factor),-num) %>% names())
  })
  
  
  ## Rare Label Encoding
  # Input to select var to visualize as a barplot
  output$ui_sel_rareEnc1_trnsFea04<-renderUI({
    #req(input$rad_trnsFea04)
    selectInput01(id="sel_rareEnc1_trnsFea04",label=varViz_feat,
                  #dynamically ticket and deck (NOTE: will need to update data object later)
                  choices=trainDF_nvI() %>% select(deck,ticket) %>% names())
  })
  
  # Input to select vars to combine as a category and visualize in a new barplot (NAs are off limits)
  output$ui_sel_rareEnc2_trnsFea04<-renderUI({
    req(input$sel_rareEnc1_trnsFea04)
    selectizeInput(inputId="sel_rareEnc2_trnsFea04",label="",multiple=TRUE,
                   choices=c("Choose at least two"="",
                             trainDF_nvI() %>% 
                               pull(input$sel_rareEnc1_trnsFea04) %>% 
                               unique() %>%
                               sort() %>%
                               as.character()))
  })

  
  ### Conditional output for feature extraction
  ## Normalization/standardization
  # Display set of plots
  output$plot_sel_scale1_trnsFea04<-renderPlot({
    req(input$sel_scale1_trnsFea04)
    cowplotter(trainDF_nvI(),input$sel_scale1_trnsFea04)
  })

  
  ## Discretization
  # Plot raw data with fill=transported as histogram
  output$plot_sel_dis1_trnsFea04<-renderPlot({
    req(input$sel_dis1_trnsFea04,input$rad_dis1_trnsFea04)
    histogrammer2(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,
                 n.bins=input$num_dis1_trnsFea04,x.log.scale=input$rad_dis1_trnsFea04)
  })
  
  # Plot numerical var in bins filled by transported either using R or user specified break values
  #create reactive vector of cut locations
  cuts<-reactive({
    c(input$n1,input$n2,input$n3,input$n4,input$n5)
  })
  
  #use switch to choose which type of output
  output$plot_sel_dis2_trnsFea04<-renderPlot({
    req(input$rad_dis2b_trnsFea04)
    switch(input$rad_dis2b_trnsFea04,
           R=bin_plotter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,num.breaks=input$num_dis2a_trnsFea04,
                         y.log.scale=input$rad_dis2a_trnsFea04),
           me=user_bin_plotter(dat=trainDF_nvI(),col=input$sel_dis1_trnsFea04,break.vals=cuts(), 
                               y.log.scale=input$rad_dis2a_trnsFea04)
    )
  })
  
  
  ## Ordinal Encoding
  # Display text
  output$text_sel_ordEnc_trnsFea04<-renderUI({
    req(input$sel_ordEnc_trnsFea04)
    paste("Insert text here")
  })
  
  # Display plot
  output$plot_sel_ordEnc_trnsFea04<-renderPlot({
    req(input$sel_ordEnc_trnsFea04)
    barplotter(trainDF_nvI(),input$sel_ordEnc_trnsFea04)
  })
  
  
  
  
  ## Rare Label Encoding
  # Display plots
  #raw
  output$plot_sel_rareEnc1_trnsFea04<-renderPlot({
    req(input$sel_rareEnc1_trnsFea04)
    rare_enc_barplotter(trainDF_nvI(),input$sel_rareEnc1_trnsFea04)
  })
  
  #combined categories
  output$plot_sel_rareEnc2_trnsFea04<-renderPlot({
    req(length(input$sel_rareEnc2_trnsFea04)>1)
    rare_enc_barplotter(trainDF_nvI(),var=input$sel_rareEnc1_trnsFea04,cats=input$sel_rareEnc2_trnsFea04)
  })
  

  
  
  #### Feature Creation-------------------------------------------------------------------------------------------------

  
  
  #### Feature Selection------------------------------------------------------------------------------------------------
    
  
  #Server 5: Data Partitioning==========================================================================================

  
  #Server 6: Modeling===================================================================================================
  
  #Server 7: Validation & Tuning========================================================================================

  
  #Server 8: Model Testing==============================================================================================
  
  
}

  

shinyApp(ui,server)



#------------------------------------------------
## NEED TO...
# create another function script with a server suffix (for more 'structural' functions) & create functions
# add ggtitles to rare label encoding(?)


#--------------------

## DONE
# created skeleton for feature selection tab



# LAST PUSHED COMMENT(S)
# created button to confirm feature scaling
# updated server code for feature scaling and outputted temp table to confirm it works
# updated reactive data frame name for feature engineering section
# began creating UI for feature creation tab


## IN PROGRESS
# feature creation code (skipped ahead)--backbone code + functions for two feature creation possibilities
# specifically working on backbone code (and adapting previous functions) to create server code


#---------------------


## TO DO
#spacing between tables and plots
#text size on plots (e.g., axes)
#add table titles--perhaps to correlation table
#deal with all the variable num categories
#update edaTabBuilder code to make dt outputs optional (to adjust for mult)
#ability to bin choices? (vars into factor, logical, etc)
#convert larger server 'patterns' to functions
#output of missing names submenu/tab is a new DF object...thus a user can skip to, but not past, this section
#for missing name tab--need to have the first output (plot or DT) output in the same area
#perhaps add an option to compare before/after datasets re imputation using vis_compare()
#swap out my missingness function (data check tab) with the one from naniar?
#in missingness tab, consider adding option for nsets (or to select variables) for gg_miss_upset()--perhaps there's
  #a first drop down selectize with option to choose all and then user can select the missingness pattern from there
#add modals for imputation options that are risky
#make selectizeInput functions more flexible (and change edaTabBuilder)
#conditionally display subset of main tabs based on where user is
#feature scaling plots--axis labels and plot types (e.g., density, qq)
#update barplotter() so that it sorts categories from most to least frequent (at least for univariate case)
#in transformations tab, perhaps use the specific terms for the transforms (e.g., scaling, discretization) and add some
  #type of hyperlink or colored text where you hover over to get a more thorough defintion
#make the feature extraction-discretization plot interactive so a user can pull values for breaks
#a notebook?
#update annotations and add annotations



#------------------------------------------------
#OUTLINE

### 5. Data Partitioning: Divide training data into four subsamples for v-fold cross-validation


### 6. Modeling
## Create recipe (identify col as id var, predictor, or outcome)
## Specify models
## Construct workflow
## Fit models 
## Assess model accuracy


### 7. Validation and tuning
## Fit models to cross-validation folds
## Tune models
## Select best model and finalize workflow
## Assess model characteristics


### 8. Testing
## Preprocess test data using same methods for training data
## Run trained and tuned model onto test data

