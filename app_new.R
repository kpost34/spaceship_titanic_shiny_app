# Spaceship Titanic Shiny App

# Modularized structure
## Load packages
pacman::p_load(shiny,conflicted,here,tidyverse,janitor,shinyjs,DT,visdat,finalfit,skimr,GGally,rstatix,
               naniar,mice,cowplot,GGally, ggiraph, shinyWidgets, Hmisc, mice, simputation)

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

    # #feature engineering
    df_train_nvI_tF <- featTrans_mainServer("df1", df_train_nd_nvI) 
    df_train_nvI_cF <- featCreatServer("df2", df_train_nd_nvI)
    featSelServer("df3", df_train_nd_nvI, df_train_nd_nvI_tF, df_train_nd_nvI_cF)
    

  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  


#----------------------
## DONE



# LAST PUSHED COMMENT(S)
#in app_new and modules--updated DF names to reflect upstream code updates
#supplied a .init arg for reduce() in 04_featTrans_dis



## IN PROGRESS


#---------------------------------------------------------------------------------------------------
## TO DO 
#1. add floor_num to choices in rare label encoing and ordinal encoding
#2. feature selection code
  #a) add a simple bivariate EDA with each non-chr var and transported
  #b) use fn developed via backbone to incorporate logic such that if a user selects a column then 
    #all directly related columns will drop out






#---------------------------------------------------------------------------------------------------

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
#general
  #need to use a pseduo-log scale (or some type of adjustment for 0s) in ALL PLOTS; otherwise
    #data become hidden
  #feature engineering (all tabs) can only be available after missingness tabs dealt with
    #same goes for next set of tabs
  #UI labels are inconsistently coded as objects--figure out consistent way to handle them
  #update headers in fn and backbone codes--for accuracy


#feature engineering- selection
  #UI: remove starting gray box (sidebarpanel)
  #take the reactive DF from transformation and the reactive DF from creation & join them
  #then use names() to populate choices in selectize
  #when user picks a variable that is related to other vars in the joined DF, those vars 
    #will drop out (i.e., the choices needs to be a reactiveValue)
    #e.g., if user selects 'spa_dis', then 'spa' disappears; if user selects 'spa_food_court'
      #then 'spa' and 'spa_dis" disappear



#04_feature engineering- transform
  #make button a different color or larger to make it stand out once all transforms done
  #whole module
    #all transforms need to be completed to move on
      #some sort of modal should appear that lists remaining items
        #or some other feedback system
  #in transformations tab, perhaps use the specific terms for the transforms (e.g., scaling, discretization)
  #add some type of hyperlink or colored text where you hover over to get a more thorough definition

    #for new 'submodules'
      #turn lists of outputs to purrr::map with tagList
      #there should be an option to 'skip' scaling/extraction (this will make debugging quicker too)



  #04_feature scaling
    #use waiter (or equivalent package) to let user know that processing is happening
    #when the scaling type is selected and confirmed, there needs to be feedback
      #1) toast notification
      #2) textOutput: "x has been selected"
    #later...
      #more nuanced scaling: choose type for each variable


  #04_discretization 
    #update caption to read "...for display on log10 scale"
    #need feedback after confirmation
      #1) toast notificaton
      #2) some type of text output [that stays] & is dynamic so it disappears if a user confirms
        #'no discretization'
    #for user cuts (bin boundaries)
      #set it up such that 0 (or negative values) are not possible--need to create user feedback here
    #got error after discretizing one var then not dis then confirming
    #confirm button needs to moved fully onto panel
    #alter size of sidePanel and plots
    #include a table that shows what user selected for each variable: 1) type of cuts and 2) if
      #user-selected, then what are they
    


  #04_ordinal encoding
    #feedback following button pressing
      #1) toast notification
      #2) some type of text
    #later.../
      #use purrr::map() to bundle output/render fns


  #04_rare label encoding
    #confirmation should yield feedback
      #1) toast notification selected
      #2) some type of text


#03_missNum:
  #add toast notification after submitting floor_num decision
    

#03_missName
  #Use accordion text for the description in the middle
  #Indicate somehow (red box?) that the last step must be completed to continue
  #in missingness tab, consider adding option for nsets (or to select variables) for gg_miss_upset()--perhaps there's
   #a first drop down selectize with option to choose all and then user can select the missingness pattern from there


#03_missOth
  #UI
    #input 3:
      #imputation method--selector from MICE package
  #this tab/page (and all of them after) should be hidden until user submits a name imputation method
  #need user feedback after 'submit' button is hit [unsure why this takes so long to run]




#---------------------------------------------------------------------------------------------------


#### LAST UPDATES (if at all)
  #perhaps add an option to compare before/after datasets re imputation using vis_compare()
  #need a title page where variables are defined--perhaps some type of accordion presentation
  #use ggiraph for EDA and all plots--make them interactive
  #look for spots in server code that can be functionalized
  #hide all 'temp_tables'




