#load libraries
pacman::p_load(shiny,tidyverse)



ui<-navbarPage(title="Spaceship Titanic Shiny App", id="mainTab",position="static-top",
  #### 0: Title page and intro==========================================================================================
  tabPanel(title="Intro",id="00_intro"),
    ##option to skip intro-> advances to tab/menu 1
    ##preview
  #### 1: Menu-Data Checking=================================================================================
  tabPanel(title="Data Check",menuName="01_check",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId="01_multi_col_check",label="Multi-column check",
                    choices=c("dimensions"="dim",
                              "data structure"="dat_str",
                              "top 5 rows"="top5",
                              "bottom 5 rows"="bott5",
                              "missingness"="miss"),
                    selected=character(0)),
        h5("Single column check"),
        selectInput(inputId="01_sing_col_check",label="Which column?",
                    choices="SEE BOOK",selected=character(0)),
        selectizeInput(inputId="01_tab_graph_check",label="What type of exploration?",
                    choices=c("Tabular","Graphical"),selected=character(0),multiple=TRUE)
      ),
      mainPanel(
        textOutput("01_"),
        tableOutput("01"),
        plotOutput("01_")
      )
    )
  ),
  
  #### 2: Menu-Data Imputation==========================================================================================
  navbarMenu(title="Imputation",menuName="02_impute",
    tabPanel(title="Missing names", id="02a_name",
      sidebarLayout(
        sidebarPanel(
          h5("Some passengers did not have names"),
          selectInput(inputId="02a_",label="Would you like to look more closely at these missing data?",
                      choices=c("yes"),
                      selected=character(0)),
          br()
          selectInput(inputId="02b_",label=)),
        mainPanel(
          plotOutput(),
          
      )
    ),
    tabPanel(title="Missingness", id="02b_miss",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Impute data",id="02c_impute",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
  
  #### 3: Menu-Features=================================================================================================
  navbarMenu(title="Features",menuName="03_feature",
    tabPanel(title="Feature assessment", id="03a_feat_assess",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Feature engineering",id="03b_feat_eng",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
  
  #### 4: Menu-EDA======================================================================================================
  navbarMenu(title="EDA",menuName="04_eda",
    tabPanel(title="Summary stats",id="04a_summ",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Predictors only",id="04b_pred",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Predictors & response",id="04c_pred_resp",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
  
  #### 5: Tab-Data Partitioning=========================================================================================
  tabPanel(title="Data Partitioning",id="05_part",
    sidebarLayout(
      sidebarPanel(),
      mainPanel()
    )
  ),
  
  #### 6: Menu-Modeling=================================================================================================
  navbarMenu(title="Modeling",menuName="06_mod",
    tabPanel(title="Model specification", id="06a_mod_spec",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Fit models", id="06b_mod_fit",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Model assessment", id="06c_mod_assess",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
  
  #### 7: Menu-Validation & Tuning===========================================================================================
  navbarMenu(title="Validation & Tuning", menuName="07_val_tune",
    tabPanel(title="Cross-validate",id="07a_cross_val",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Model tuning", id="07b_mod_tune",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),  
    tabPanel(title="Model selection", id="07c_mod_sel",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Model assessment", id="07d_mod_assess",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
    
  #### 8: Tab-Model Testing============================================================================================
  tabPanel(title="Testing",id="08_mod_test",
    sidebarLayout(
      sidebarPanel(),
      mainPanel()
    )
  )
)

server<-function(input,output,session){
  
  #Server 1: Data Checking & Cleaning===================================================================================

  
  #Server 2: Data Imputation============================================================================================

  
  #Server 3: Features===================================================================================================

  
  #Server 4: EDA========================================================================================================
  
  #Server 5: Data Partitioning==========================================================================================

  
  #Server 6: Modeling===================================================================================================
  
  #Server 7: Validation & Tuning========================================================================================

  
  #Server 8: Model Testing==============================================================================================
  
  
}

shinyApp(ui,server)



#------------------------------------------------
## DONE
#finished rough outline of app






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

