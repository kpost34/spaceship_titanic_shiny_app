# Spaceship Titanic Shiny App

# Modularized structure
## Load packages
pacman::p_load(shiny, conflicted, here, tidyverse, janitor, shinyjs, DT, visdat, finalfit, skimr,
               GGally, rstatix, naniar, mice, cowplot, GGally, ggiraph, shinyWidgets, Hmisc, 
               simputation, shinydashboardPlus, shinycssloaders, rsample)

#address potential conflicts
filter <- dplyr::filter
chisq.test <- stats::chisq.test
observe <- shiny::observe


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
    
    #data checking
    dataCheckUI("df"),
    
    #eda
    navbarMenu(title="EDA", menuName="EDA02",
      edaUniUI("data1"), 
      edaBiUI("data2"),
      edaMultUI("data3")
    ),
    
    #missingness
    navbarMenu(title="Missingness", menuName="Mis03",
      missNumUI("dat1"),
      missNameUI("dat2"),
      missOtherUI("dat3")
    ),
    
    #feature engineering
    navbarMenu(title="Feature Engineering", menuName="Fea04",
      featTrans_mainUI("df1"),
      featCreatUI("df2"),
      featSelUI("df3")
    ),
    
    #data partitioning
    dataPartUI("data"),
    
    #modelling
    navbarMenu(title="Modelling", menuName="Mod05",
      modSelUI("dat1"),
      modTuneUI("dat2")
    )
    
    #testing
    
    
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
    df_train_select <- featSelServer("df3", df_train_nd_nvI, df_train_nd_nvI_tF, df_train_nd_nvI_cF)
    
    #data partitioning
    df_vfold <- dataPartServer("data", df_train_select)
    
    #modelling
    model_type <- modSelServer("dat1", df_vfold)
    model_final <- modTuneSever("dat2", model_type)
    
    #testing

  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  


#----------------------
## DONE

  



# LAST PUSHED COMMENT(S)
#built out UI and server of app before developing new modules
#backbone:
  #03: removed travel_party_size as a feature
  #04: retained df_vfold through backbone scripts by adding it as neglected obj in rm()
  #05: updated code for cross-validation using logistic reg and decision trees by removing
    #travel_party_size from formula & updated naming in decision tree chunk
  #06: began creating tuning code for logistic reg and decision tree models






## IN PROGRESS


#---------------------------------------------------------------------------------------------------
## TO DO 

### 7. Validation and tuning
## Fit models to cross-validation folds
## Tune models
## Select best model and finalize workflow
## Assess model characteristics


#--------------------------------------------------------------------------------------------
#REMAINING UPDATES...
#general
  #UI labels are inconsistently coded as objects--figure out consistent way to handle them
  #update headers in fn and backbone codes--for accuracy
  #conditionally display tabs as user progresses through app (missingness, feature engineering, etc.)

#button in 04a_main: skip all transforms

#---------------------------------

#data imputation--does something to variable names and causes downstream problems???



#---------------------------------------------------------------------------------------------------
#REMAINING OUTLINE (rough)



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






