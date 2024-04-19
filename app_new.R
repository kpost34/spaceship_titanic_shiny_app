# Spaceship Titanic Shiny App

# Modularized structure
## Load packages
pacman::p_load(shiny, conflicted, here, tidyverse, janitor, shinyjs, DT, visdat, finalfit, skimr,
               GGally, rstatix, naniar, mice, cowplot, GGally, ggiraph, shinyWidgets, Hmisc, 
               simputation, shinydashboardPlus, shinycssloaders, rsample, tidymodels)

#address potential conflicts
filter <- dplyr::filter
select <- dplyr::select
chisq.test <- stats::chisq.test
observe <- shiny::observe
translate <- parsnip::translate


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
      modSelUI("dat1")
      # modTuneUI("dat2")
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
    model_type <- modSelServer("dat1", df_train_select, df_vfold)
    # model_final <- modTuneSever("dat2", model_type)
    
    #testing

  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  


#----------------------
## DONE




# LAST PUSHED COMMENT(S)
#app_new: updated annotations
#06_model_tuning: specified translate() is from parsnip
#...updated assess_model() such that if simple is TRUE then only .config drops
#module 06a:
  #UI:
    #replaced actionButtons with radioButtons
    #added checkbox group input and slider input (as ui output) for tuning
    #organized tuning outputs into a row
  #Server:
    #conditionally display model-specific checkboxes, hyperparameter slider, & confirmation button
    #created a set of reactives for the workflow that generates a tuned log reg model
    #created code to display tables of hyperparameters and tuned model metrics




## IN PROGRESS



#creating reactives for workflow of selected model--will need function--then afterward need to
  #display grid of grid selection followed by figures
#dynamically display options for user to select combination of hyperparameters

#add css_loader for multiple inputs on this page


#consider renaming fn and obj scripts--shorten to "ship_app_..."
#consider renaming module 06a to simply 06 and changing app_new UI so that it's not a dropdown


#---------------------------------------------------------------------------------------------------
## TO DO 
#need to create functions to easily fit models selected
#add spinners (to help with processing time)

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






