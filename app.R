#### Load packages
pacman::p_load(shiny,here,tidyverse,janitor,DT,visdat,finalfit,skimr,GGally,rstatix,conflicted)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


#### Load functions
source(here("functions","spaceship_titanic_app_func_ui.R"))
source(here("functions","spaceship_titanic_app_func_01.R"))


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
  mutate(across(c(home_planet,deck:destination),~as.factor(.x))) -> trainDF


#### Create vectors
### Col names
## All cols but character
trainDF %>% select(!where(is.character)) %>% names() -> trainDF_nchrVars

## All logical and factor cols
trainDF %>% select(where(is.logical)|where(is.factor)) %>% names() -> trainDF_catVars

## All numeric cols
trainDF %>% select(where(is.numeric)|where(is.integer)) %>% names() -> trainDF_numVars

## selectInput
Chk01_quickVec<-c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")
Chk01_summVec<-c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")


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
    ##preview
  #### 1: Menu-Data Checking======================================================================================================
  tabPanel(title="Data Check",id="Chk01",
    sidebarLayout(
      sidebarPanel(width=3,
        selectInput01(id="sel_quick_Chk01",label="Quick data check",choices=Chk01_quickVec),
        linebreaks(2),
        selectInput01(id="sel_summ_Chk01",label="Data summaries",choices=Chk01_summVec)
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
    tabPanel(title="Univariate",id="uniEDA02",
      titlePanel(title="Univariate Exploratory Data Analysis"),
      #inputs
      wellPanel(
        fluidRow(
          column(6,
            selectInput01(id="sel_var1_uniEDA02",label="",choices=trainDF_nchrVars)
          ),
          column(6,
            selectInput01(id="sel_var2_uniEDA02",label="",choices=trainDF_nchrVars)
          )
        )
      ),
      #outputs
      fluidRow(
        column(6,
          htmlOutput("text_sel_var1_uniEDA02"),
          DTOutput("tab_sel_var1_uniEDA02")
        ),
        column(6,
          htmlOutput("text_sel_var2_uniEDA02"),
          DTOutput("tab_sel_var2_uniEDA02")
        )
      ),
      fluidRow(
        column(6,
          plotOutput("plot_sel_var1_uniEDA02")
        ),
        column(6,
          plotOutput("plot_sel_var2_uniEDA02")
        )
      )
    ),
    #tab 2: bivariate EDA---------------------------------------------------------------------------------------------------------
    tabPanel(title="Bivariate",id="biEDA02",
      titlePanel(title="Bivariate Exploratory Data Analysis"),
      #inputs
      wellPanel(
        fluidRow(
          column(6,
            selectizeInput01(id="sel_var12_biEDA02",label="", choices=trainDF_nchrVars)
          ),
          column(6,
            selectizeInput01(id="sel_var34_biEDA02",label="",choices=trainDF_nchrVars)
          )
        )
      ),
      #outputs
      fluidRow(
        column(6,
          htmlOutput("text_sel_var12_biEDA02"),
          DTOutput("tab_sel_var12_biEDA02")
        ),
        column(6,
          htmlOutput("text_sel_var34_biEDA02"),
          DTOutput("tab_sel_var34_biEDA02")
        )
      ),
      fluidRow(
        column(6,
          plotOutput("plot_sel_var12_biEDA02")
        ),
        column(6,
          plotOutput("plot_sel_var34_biEDA02")
        )
      )
    )
  )
    #switch based on two or three vars & their type
    #correlation plots--together/individually
    #statistical comparisons--correlations,
      # fluidRow(
      #   plotOutput("bi_eda_sel_02b"),
      #   plotOutput("tri_eda_sel_02b")

  # #### 3: Menu-Missing Data====================================================================================================
  # navbarMenu(title="Missing Data",menuName="miss_03",
  #   tabPanel(title="Missing names", id="03a_miss_nam",
  #     sidebarLayout(
  #       sidebarPanel(
  #         h5("Notice that some passengers did not have names?"),
  #         selectInput(inputId="03a_miss_name",label="Would you like to look more closely at these missing data?",
  #                     choices=c("yes"),
  #                     selected=character(0)),
  #         br(),
  #         #uiOutput(""), #dynamic UI to provide options for exploring missing names
  #         selectInput(inputId="03a_",label="How would you like to handle missing names?",
  #                     choices=c("drop column"="drop_col",
  #                               "remove rows"="remove_row",
  #                               "populate using ticket info"="imp_pass_group",
  #                               "populate using room info"="imp_room"))),
  #         #uiOutput(""), #dynamic UI to remove rows with NA names afterward
  #       mainPanel(
  #         plotOutput("03a_"),
  #         plotOutput("03a_"),
  #       )
  #     )
  #   ),
  #   tabPanel(title="Overall missingness", id="03b_miss_miss",
  #     sidebarLayout(
  #       sidebarPanel(
  #         selectInput(inputId="03b_viz_miss_sel",
  #                     label="How would you like to visualize missingness?",
  #                     choices=c("overall"="overall",
  #                               "missing patterns"="patt")),
  #         selectInput(inputId="03b_stats_miss_sel",
  #                     label="Which variable would you like to test for MAR?",
  #                     choices="x" #SEE BOOK#)
  #         )
  #       ),
  #       mainPanel(
  #         plotOutput("03b_"),
  #         plotOutput("03b_"),
  #         tableOutput("03b_")
  #       )
  #     )
  #   ),
  #   tabPanel(title="Handle missingness",id="03c_miss_imp",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   )
  # ),
  # 
  # #### 4: Menu-Features=========================================================================================================
  # navbarMenu(title="Features",menuName="feat_04",
  #   tabPanel(title="Feature assessment", id="feat_assess_04a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Feature engineering",id="feat_eng_04b",
  #            sidebarLayout(
  #              sidebarPanel(),
  #              mainPanel()
  #            )
  #   )
  # ),
  # 
  # 
  # 
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

  
  ##### Server 2: EDA============================================================================================
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
      histogramer(trainDF,input$sel_var1_uniEDA02)
    }
    else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var1_uniEDA02)
    }
  })

  output$plot_sel_var2_uniEDA02<-renderPlot({
    if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
      histogramer(trainDF,input$sel_var2_uniEDA02)
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
    req(length(input$sel_var12_biEDA02)==2)
    if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
      barplotter(trainDF,input$sel_var12_biEDA02)
    }
    else if(input$sel_var34_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var12_biEDA02)
    }
  })
  
  # output$plot_sel_var2_uni_eda02<-renderPlot({
  #   if(input$sel_var2_uni_eda02 %in% trainDF_num_vars){
  #     histogramer(trainDF,input$sel_var2_uni_eda02)
  #   }
  #   else if(input$sel_var2_uni_eda02 %in% trainDF_cat_vars){
  #     barplotter(trainDF,input$sel_var2_uni_eda02)
  #   }
  # })
  # 


  
  #Server 3: Missing Data===================================================================================================

  
  #Server 4: Features========================================================================================================
  
  #Server 5: Data Partitioning==========================================================================================

  
  #Server 6: Modeling===================================================================================================
  
  #Server 7: Validation & Tuning========================================================================================

  
  #Server 8: Model Testing==============================================================================================
  
  
}

shinyApp(ui,server)



#------------------------------------------------
## DONE
# made barplotter function more flexible by 1) moving cat var with more levels to x (not fill), 2) using more colors, 3) including
  #na.rm arg, and 4) to take on 3 categorical variables
# updated naming formulas again
# boxplotter function

## IN PROGRESS
# dynamic UI on whether to include NAs in plots

## TO DO
#finish bivariate EDA tab


#------------------------------------------------
#OUTLINE
#### Preprocessing data
### Load packages and data

### 1. Data check and cleaning
## Preliminary data check: check numbers of rows and cols, appropriate col classes
## Data cleaning: names and order of cols; re-order factor levels; re-coding cols


### 2. Data imputation
## Assessed data for missingness and understand pattern of missingness
## imputate missing data
## Data check: data checked (e.g., ranges) to see if data make sense following imputation--consider figures here too (differentiating given)


### 3. Feature engineering
## Assess data for possible features
## Feature engineering: extracting "data" from cols to generate variables (e.g., extracting alpha prefixes from ticket #s)
## (Feature selection: vars of interest selected for epa)
## Another option to re-order cols


#### 4. EDA
### Summary stats and correlations: summary() and skim() performed on each col
### Predictors-only exploration
## Univariate, bivariate, multivariate
### Predictors & response
## Univariate of response
## Bivariate


### 5. Data Paritioning: Divide training data into four subsamples for v-fold cross-validation


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

