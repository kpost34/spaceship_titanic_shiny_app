# Spaceship Titanic Shiny App

# Modularized structure
## Load packages
pacman::p_load(shiny,conflicted,here,tidyverse,janitor,shinyjs,DT,visdat,finalfit,skimr,GGally,rstatix,
               naniar,mice,cowplot,GGally, ggiraph, shinyWidgets)

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
      missNameUI("dat1"),
      missOtherUI("dat2")
      ),
    navbarMenu(title="Feature Engineering", menuName="Fea04",
      featTrans_mainUI("df0")
      # featCreatUI("df1")
      # featSelUI("df2")
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
    df_train_nvI <- missNameServer("dat1")
    missOtherServer("dat2", df_train_nvI)
    
    #feature engineering
    df_train_nvI_t <- featTrans_mainServer("df0", df_train_nvI)
    # featCreatServer("df2")
    # featSelServer("df3")
    

  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  


#----------------------
## DONE



# LAST PUSHED COMMENT(S)
#featTrans_ordEnc
  #changed input from being conditional (renderUI) to a simple UI input
  #updated IDs to be more user-friendly
  #added titles to plots


## IN PROGRESS



#---------------------





## TO DO 

#feature engineering- feature creation
  #add ns() where applicable
  #remove extraneous suffixes of inputs/outputs
  #get code to run







#feature engineering- transform
  #whole module
    #all transforms need to be completed to move on
      #some sort of modal should appear that lists remaining items
        #or some other feedback system

    #for new 'submodules'
      #turn lists of outputs to purrr::map with tagList
      #there should be an option to 'skip' scaling/extraction (this will make debugging quicker too)



  #feature scaling
    #use waiter (or equivalent package) to let user know that processing is happening
    #when the scaling type is selected and confirmed, there needs to be feedback
      #1) toast notification
      #2) textOutput: "x has been selected"
    #later...
      #more nuanced scaling: choose type for each variable


  #discretization 
    #need feedback after confirmation
      #1) toast notificaton
      #2) some type of text output [that stays] & is dynamic so it disappears if a user confirms
        #'no discretization'
    #for user cuts (bin boundaries)
      #set it up such that 0 (or negative values) are not possible--need to create user feedback here
    


  #ordinal encoding
    #feedback following button pressing
      #1) toast notification
      #2) some type of text
    #later...
      #use purrr::map() to bundle output/render fns


  #rare label encoding
    #use interpretable ids
    #switch to grouped bars
    #should add option for log10 y scale for both plots
    #move legend to below plot
    #confirmation should yield feedback
      #1) toast notification selected
      #2) some type of text


    





#missName
  #Use accordion text for the description in the middle
  #Indicate somehow (red box?) that the last step must be completed to continue


#missOther
  #this tab/page (and all of them after) should be hidden until user submits a name imputation method
  #make side panel narrower
  #last plot won't work with ticket
  #re-think the types of missingness plots that we should use
  #re-think how to assess testing for MAR
    #--> for both bullets, look at the marsh analysis that I started


#general (new)
  #add variable type in dropdowns when selecting variable
  #Note: use ggiraph for EDA and all plots--make them interactive
  #add a backward/forward arrows for users to navigate pages in sequence
  #feature engineering (all tabs) can only be available after missingness tabs dealt with
    #same goes for next set of tabs





### OLD COMMENTS ###-----------------
#general/unknown
  #add table titles--perhaps to correlation table
  #ability to bin choices? (vars into factor, logical, etc)
  #add modals for imputation options that are risky
  
  #fix group size plots not in descending order of frequency
  #from dataCheck module: swap out my missingness function (data check tab) with the one from naniar?
  #need a title page where variables are defined--perhaps some type of accordion presentation


#missingness
  #perhaps add an option to compare before/after datasets re imputation using vis_compare()
  #in missingness tab, consider adding option for nsets (or to select variables) for gg_miss_upset()--perhaps there's
   #a first drop down selectize with option to choose all and then user can select the missingness pattern from there


#functions
  #make selectizeInput functions more flexible (and change edaTabBuilder)
  #convert larger server 'patterns' to functions & populate into separate script
  #update functions so that they don't carry so many extraneous cols/vars
  #consider making group size variable switch code a function





#feature engineering
  #feature scaling plots--axis labels and plot types (e.g., density, qq)
  #in transformations tab, perhaps use the specific terms for the transforms (e.g., scaling, discretization) and add some
    #type of hyperlink or colored text where you hover over to get a more thorough defintion
  #discretization first plot--log scale y axis as option (and thus update formula)
  # add ggtitles to rare label encoding(?)
  #make the feature extraction-discretization plot interactive so a user can pull values for breaks
  #user feedback: add it if user chooses beyond range and if user does not select at least two vars for luxury expense
    #variable


#------------------------------------------------
### BEFORE PAUSING ON DEVELOPMENT###
## NEED TO...
#1) change inputs for log10 x-axis and bin numbers to dynamic version
#2) enable a way to reset the input values when someone selects yes or no
#3) selecting yes/no leads to a change/non-change in growing data frame
#4) connect all the growing data frames (for discretization) 
#5) check by outputting a preview table


## Update (9/22/22): putting this app on pause because of lack of progress. Right now I am stuck on feature extraction-
#discretization server code. The code to generate plots and produce DFs works fine. The issue is that I am stuck
#attempting to develop a way to connect each DF (associated with each numerical variable). Ideally I'd like to join
#them on passenger_id and create a DF that has one or more vars that end with "_dis." Unfortunately when I try to use
#an actionButton that triggers (confirms) a "dis" variable and lack of pressing the button generates a reactive DF, 
#which contains only passenger_id, they won't join. Shiny still says that the var(s) where the button was not pressed
#is not reactive and I can't join reactive and non-reactive DFs. I've tried various observeEvents eventReactives
#to cirumvent this but I get either errors or nonsense output. I've tried a second button but shiny seems to "dislike"
#having inputs control different types of outputs via an observeEvent (or two) or two eventReactives. When I figure this
#out, I will return. A temporary solution would be to force the user to go through each variable, but that is cumbrersome
#and it will make testing a PitA. Also I will need to do something similar shortly after this because I want to join
#the temporary reactive DFs that come from the four types of data transforms/feature extraction methods. Then I will
#need to join that reactive DF with a feature creation DF.

#--------------------



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

