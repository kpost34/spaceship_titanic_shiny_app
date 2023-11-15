#### Load packages
pacman::p_load(shiny,conflicted,here,tidyverse,janitor,shinyjs,DT,visdat,finalfit,skimr,GGally,rstatix,naniar,mice,cowplot,
               GGally)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")



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


#### Load functions, objs, and modals
here("fns_objs_modals") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


         

#NAMING FORMULAS
#inputs: [abbrvInput]_[abbrvTask]_[Tabname]
#e.g., sel_quick_Chk01 (from Chk011 tab): sel = selectInput, quick = "Quick data check", chk01
#outputs: [abbrOutput]_[abbrvInput]_[# if 2+]_[tabName]
#e.g., tab_sel_Chk01 (from Chk01 tab): tab = table, sel = selectInput, chk01 (if 2 tables then append 1, 2, etc to before tabName)
#i.e., tab_[inputNameFormula]

ui<-navbarPage(title="Spaceship Titanic Shiny App", id="mainTab",position="static-top",
               useShinyjs(),
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
                       inline=TRUE,width="100%"),
          uiOutput("ui_chk_trnsFea04")
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
    ),
    #tab 2: feature creation--------------------------------------------------------------------------------------------
    tabPanel(title="Feature Creation",id="creFea04",
      titlePanel(title="Feature Creation"),
      h4("Now you have the opportunity to create new features for your model using the existing variables. Let's look
         at some possible options"),
      #inputs
      wellPanel(
        fluidRow(
          column(5,
            #select input for group size
            selectInput01(id="sel_exp1_creFea04",label="Create a group size variable that uses...",
                          choices=creFea04_grpSizeVec)
          ),
          column(2,
            linebreaks(5),    
            uiOutput("ui_btn_cfirm_creFea04")
          ),
          column(3,
            #select input for luxury expenses
            selectizeInput(inputId="sel_exp2_creFea04",label="Create a luxury expense variable that uses the sum of",
                          multiple=TRUE,choices=c("Choose at least two variables"="",creFea04_luxVec)),
          ),
          column(2,
            actionButton(inputId="btn_exp2_creFea04",label="Visualize results")
          )
        ),
        #vertically aligns button with selectizeInput
        tags$style(type="text/css", "#btn_exp2_creFea04 {width: 100%; margin-top: 25px;}")
      ),
      #outputs
      fluidRow(
        column(6,
          plotOutput("plot_sel_exp1a_creFea04"),
          plotOutput("plot_sel_exp1b_creFea04")
        ),
        column(6,
          plotOutput("plot_sel_exp2a_creFea04"),
          br(),
          plotOutput("plot_sel_exp2b_creFea04"),
          #temporary table--previews data after confirming selections
          tableOutput("plot_temp_table_creFea04")
        )
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