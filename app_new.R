# Spaceship Titanic Shiny App

# Modularized structure
## Load packages
pacman::p_load(shiny, conflicted, here, tidyverse, janitor, shinyjs, DT, visdat, finalfit, skimr,
               GGally, rstatix, naniar, mice, cowplot, GGally, ggiraph, shinyWidgets, Hmisc, 
               simputation, shinydashboardPlus, shinycssloaders)

#address potential conflicts
filter <- dplyr::filter
chisq.test <- stats::chisq.test


## Source files
### Load objects and functions
here("fns_objs_modals") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


### Load modules
here("modules") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)




# App
spaceTitanicApp <- function() {
  ## UI
  ui <- navbarPage(title="Spaceship Titanic Shiny App", id="mainTab", #posiiton="static-top",
    useShinyjs(),
    tabPanel(title="Data Check",
      dataCheckUI("df"),
    ),
    navbarMenu(title="EDA", menuName="EDA02",
      edaUniUI("data1"),
      edaBiUI("data2"),
      edaMultUI("data3")
    ),
    navbarMenu(title="Missingness", menuName="Mis03",
      missNumUI("dat1"),
      missNameUI("dat2"),
      missOtherUI("dat3")
      ),
    navbarMenu(title="Feature Engineering", menuName="Fea04",
      featTrans_mainUI("df1"),
      featCreatUI("df2"),
      featSelUI("df3")
    )
  )
  
  ## Server
  server <- function(input, output, session) {
    #data checking
    dataCheckServer("df")
    
    #eda
    edaUniServer("data1")
    edaBiServer("data2")
    edaMultServer("data3")
    
    #missingness
    df_train_nd <- missNumServer("dat1")
    df_train_nd_nI <- missNameServer("dat2", df_train_nd)
    df_train_nd_nvI <- missOtherServer("dat3", df_train_nd_nI)

    #feature engineering
    df_train_nd_nvI_tF <- featTrans_mainServer("df1", df_train_nd_nvI) 
    df_train_nd_nvI_cF <- featCreatServer("df2", df_train_nd_nvI)
    featSelServer("df3", df_train_nd_nvI, df_train_nd_nvI_tF, df_train_nd_nvI_cF)
    

  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  


#----------------------
## DONE




# LAST PUSHED COMMENT(S)
#04_main:
  #reworded instructions, including fleshing out definitions, and coloring & italicizing transformation
    #methods


## IN PROGRESS


#---------------------------------------------------------------------------------------------------
## TO DO 



#---------------------------------------------------------------------------
#put this on hold and went back to start.....
# feature selection...
  #1. update barplotter so that it can take on ordered factors
  #2. use a color scheme for boxplotter?
#1. add floor_num to choices in rare label encoing and ordinal encoding
#2. feature selection code
  #a) add a simple bivariate EDA with each non-chr var and transported
  #b) use fn developed via backbone to incorporate logic such that if a user selects a column then 
    #all directly related columns will drop out



#---------------------------------------------------------------------------------------------------
#REMAINING OUTLINE (rough)
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
#develop code to save pre-processing steps
#preprocess test data using same methods for training data
#run trained and tuned model onto test data





#--------------------------------------------------------------------------------------------
#REMAINING UPDATES...
#general
  #UI labels are inconsistently coded as objects--figure out consistent way to handle them
  #update headers in fn and backbone codes--for accuracy
  #conditionally display tabs as user progresses through app (missingness, feature engineering, etc.)


#---------------------------------
#feature engineering- selection
  #UI: remove starting gray box (sidebarpanel)
  #take the reactive DF from transformation and the reactive DF from creation & join them
  #then use names() to populate choices in selectize
  #when user picks a variable that is related to other vars in the joined DF, those vars 
    #will drop out (i.e., the choices needs to be a reactiveValue)
    #e.g., if user selects 'spa_dis', then 'spa' disappears; if user selects 'spa_food_court'
      #then 'spa' and 'spa_dis" disappear
#---------------------------------









#---------------------------------------------------------------------------------------------------


#### LAST UPDATES (if at all)
  #need a title page where variables are defined--perhaps some type of accordion presentation
  #use ggiraph for EDA and all plots--make them interactive
  #hide all 'temp_tables' (eventually)
  #specify which vars being transformed: may apply to subset of 04a

  #04_feature scaling: 
    #choose type of scaling for each variable

  #04_discretization:
    #error messages if user enters 0 or negative value in # of bins for histogram or # of breaks
      #for binning data
    #list out variables where discretization was applied to

  #04_rareEnc:
    #need reactiveVal for pool of vars so that error does not generate if try to select
      #the same var with both selectors

  #04_main:
    #some sort of feedback system to let user know what's remaining (tried to have a dynamic
      #text output and to make radioButton choices bold but couldn't get either to work)





