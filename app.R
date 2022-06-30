pacman::p_load(shiny,here,tidyverse,janitor,visdat,finalfit,skimr,GGally,rstatix,conflicted)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")

#exit: trainDF
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

#load functions
source(here("functions","spaceship_titanic_app_func_01.R"))


ui<-navbarPage(title="Spaceship Titanic Shiny App", id="mainTab",position="static-top",
  #### 0: Title page and intro====================================================================================================
  tabPanel(title="Intro",id="00_intro"),
    ##option to skip intro-> advances to tab/menu 1
    ##preview
  #### 1: Menu-Data Checking======================================================================================================
  tabPanel(title="Data Check",menuName="01_chk",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId="sel_dat_quick_01",label="Quick data check",
                    choices=c("dimensions"="dim",
                              "data sample"="dat_samp",
                              "missingness"="miss"),
                    multiple=TRUE,
                    selected=character(0)),
        br(),
        selectInput(inputId="chk_dat_summ_sel",label="Data summaries",
                    choices=c("character"="chr",
                              "factor"="fct",
                              "logical"="lgl",
                              "numeric"="num"),
                    selected=character(0))
      ),
      mainPanel(
        tableOutput("sel_quick_tab_01"),
        tableOutput("summ_tab_sel_01")
      )
    )
  ),
  
  #### 2: Menu-EDA================================================================================================================
  navbarMenu(title="EDA",menuName="02_eda",
    tabPanel(title="Univariate",id="02a_eda_uni",    
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId="02a_var_sel",label="Select one or two variables to explore.",
                      choices="x",
                      selected=character(0)),
                      options=list(maxItems=2)
          ),
          #insert update selectizeInput #table, graph
        mainPanel(
          tableOutput("02_uni_eda1_tab_sel"),
          plotOutput("02_uni_eda1_plot_sel"),
          tableOutput("02_uni_eda2_tab_sel"),
          plotOutput("02_uni_eda2_plot_sel")
        ),
      ),
    ),
    tabPanel(title="Multivariate",id="02b_eda_mult",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId="02b_var_sel",label="Select two or three variables",
                         choices="x",
                         selected=character(0),
                         options=list(maxItems=3)
          )
        ),
          #switch based on two or three vars & their type
          #correlation plots--together/individually
          #statistical comparisons--correlations,
        mainPanel(
          plotOutput("02_bi_eda_sel"),
          plotOutput("02_tri_eda_sel")
        )
      )
    )
  ),

  #### 3: Menu-Missing Data====================================================================================================
  navbarMenu(title="Missing Data",menuName="03_miss",
    tabPanel(title="Missing names", id="03a_miss_nam",
      sidebarLayout(
        sidebarPanel(
          h5("Notice that some passengers did not have names?"),
          selectInput(inputId="03a_miss_name",label="Would you like to look more closely at these missing data?",
                      choices=c("yes"),
                      selected=character(0)),
          br(),
          #uiOutput(""), #dynamic UI to provide options for exploring missing names
          selectInput(inputId="03a_",label="How would you like to handle missing names?",
                      choices=c("drop column"="drop_col",
                                "remove rows"="remove_row",
                                "populate using ticket info"="imp_pass_group",
                                "populate using room info"="imp_room"))),
          #uiOutput(""), #dynamic UI to remove rows with NA names afterward
        mainPanel(
          plotOutput("03a_"),
          plotOutput("03a_"),
        )
      )
    ),
    tabPanel(title="Overall missingness", id="03b_miss_miss",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId="03b_viz_miss_sel",
                      label="How would you like to visualize missingness?",
                      choices=c("overall"="overall",
                                "missing patterns"="patt")),
          selectInput(inputId="03b_stats_miss_sel",
                      label="Which variable would you like to test for MAR?",
                      choices="x" #SEE BOOK#)
          )
        ),
        mainPanel(
          plotOutput("03b_"),
          plotOutput("03b_"),
          tableOutput("03b_")
        )
      )
    ),
    tabPanel(title="Handle missingness",id="03c_miss_imp",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  ),
  
  #### 4: Menu-Features=================================================================================================
  navbarMenu(title="Features",menuName="04_feat",
    tabPanel(title="Feature assessment", id="04a_feat_assess",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Feature engineering",id="04_feat_eng",
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             )
    ),
    
    tabPanel(title="Feature engineering",id="04_eng",
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
  navbarMenu(title="Validation & Tuning", menuName="07_valTune",
    tabPanel(title="Cross-validate",id="07a_valTune",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Model tuning", id="07b_valTune_mod",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),  
    tabPanel(title="Model selection", id="07c_valTune_sel",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    tabPanel(title="Model assessment", id="07d_valTune_assess",
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
  
  
  
  
  #Server 0: Intro================================================================================================================
  
  
  #Server 1: Data Checking========================================================================================================
  dat_check<-reactive({
    switch(input$sel_dat_quick_01,
           dim=dim_tbl(),
           dat_samp=slice_sample(n=5),
           miss=n_miss_tbl(0)
    )
  })
    
  output$sel_quick_tab_01<-renderTable({
    req(!is.na(input$sel_dat_quick_01))
    trainDF %>% dat_check()
  })

  dat_sum<-reactive({
    switch(input$chk_dat_summ_sel,
      chr=skim_tbl(type="character"),
      fct=skim_tbl(type="factor"),
      log=skim_tbl(type="logical"),
      num=skim_tbl(type="numeric"))
  })
  
  output$summ_tab_sel_01<-renderTable({
    trainDF %>%
      dat_sum()
  })

  
  #Server 2: EDA============================================================================================

  
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
#backbone: developed better framework for missingness; worked through eda for univariate and bivariate tables/plots & started
#multivariate eda
#app. re-arranged menu again to make more logical sense; changed naming schemes
#began building out server code
#started creating functions


## TO DO
#1. Get github working again
#2. Push code to github
#3. Start making functions
#4. Continue building out server


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

