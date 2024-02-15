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
#04-featSel: 
  #fixed issue where x-axis and color scale were flipped for logical vars and 2-level factors
  #removed empty gray box at start-up
  #barplotter updated so that it can plot ordered factors (thus updated func_01 code)
  #developed code to display selector for model, including dynamically updating choices
  #added code to display button for confirming variables for model



# LAST PUSHED COMMENT(S)
#all module scripts
  #used consistent line spacing & headers in ui & server code of modules, & all obj/fn scripts
  #made all exported reactive DFs under a ## header with an explicit return() 
#04a_main: added toast notification
#04_featCreat: added toast notification


## IN PROGRESS


#---------------------------------------------------------------------------------------------------
## TO DO 

#button in 04a_main: skip all transforms

#put this on hold and went back to start.....
#2. feature selection code
  #b) use fn developed via backbone to incorporate logic such that if a user selects a column then 
    #all directly related columns will drop out




#--------------------------------------------------------------------------------------------
#REMAINING UPDATES...
#general
  #UI labels are inconsistently coded as objects--figure out consistent way to handle them
  #update headers in fn and backbone codes--for accuracy
  #conditionally display tabs as user progresses through app (missingness, feature engineering, etc.)


#---------------------------------
#feature engineering- selection
  #then use names() to populate choices in selectize
  #when user picks a variable that is related to other vars in the joined DF, those vars 
    #will drop out (i.e., the choices needs to be a reactiveValue)
    #e.g., if user selects 'spa_dis', then 'spa' disappears; if user selects 'spa_food_court'
      #then 'spa' and 'spa_dis" disappear
#---------------------------------

#data imputation--does something to variable names and causes downstream problems???



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






#---------------------------------------------------------------------------------------------------


#### OPTIONAL UPDATES (if at all)
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
    #add floor_num to choices

  #04_ordEnc:
    #add floor_num

  #04_main:
    #some sort of feedback system to let user know what's remaining (tried to have a dynamic
      #text output and to make radioButton choices bold but couldn't get either to work)
    

#are modals used? if not, should they?
#consistent character spacing (and any remnant line spacing)
#shorten script names (and subfolder names?) and use [01-10][a-z][1-9]_ prefix?


#---------------------------------------------------------------------------------------------------
#NOTES:

#1. Spacing/formatting:
  #a. Server: 
    #use 1 full line space before/after ns  
    #for ## use 15? -s afterward and 3 line spaces before except for above case
    #for ### and greater, use 2 full line spaces beforehand unless it's a ### after a ## then
      #goes on next line
    #try to use a full line space below req()
    #add line spaces within render/reactive/observeEvent/eventReactive/etc. for clarity
    #if there is a 'psuedo' header (#psuedo-header) then only one line space b/t






