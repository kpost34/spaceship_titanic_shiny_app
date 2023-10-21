# library(here)
# 
# source(here("ui.R"))
# source(here("server.R"))
# 
# shinyApp(ui,server)






# Modularized structure
## Load packages
pacman::p_load(shiny,conflicted,here,tidyverse,janitor,shinyjs,DT,visdat,finalfit,skimr,GGally,rstatix,
               naniar,mice,cowplot,GGally)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


## Source files
### Load objects and functions
here("fns_objs_modals") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


### Load modules
here("modules") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)




## App
spaceTitanicApp <- function() {
  ui <- navbarPage(title="Spaceship Titanic Shiny App", id="mainTab", #posiiton="static-top",
    useShinyjs(),
    tabPanel(title="Data Check",
      dataCheckUI("df"),
    ),
    navbarMenu(title="EDA", menuName="EDA02",
      edaUniUI("data1"),
      edaBiUI("data2"),
      edaMultUI("data3")
      # )
    ),
    navbarMenu(title="Missingness", menuName="Mis03",
      # missNameUI("dat1"),
      # missOtherUI("dat2")
      ),
    # navbarMenu(title="Feature Engineering", menuName="Fea04",
    #   featureUI("input")
    #   )
    )
  
  server <- function(input, output, session) {
    #data checking
    dataCheckServer("df")
    
    #eda
    edaUniServer("data1")
    edaBiServer("data2")
    edaMultServer("data3")
    #missingness
    # missNameServer("dat1")
    # missOtherServer("dat2")
    # featureServer("input")
  }
  
  shinyApp(ui, server)
  
}
  
spaceTitanicApp()
  
  
          
        
                  






#------------------------------------------------
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

## DONE




# LAST PUSHED COMMENT(S)
# added code so that confirmation button generates new reactive DF
# added code so that variable-specific buttons are used to populate UI output



## IN PROGRESS


#---------------------


## TO DO
#spacing between tables and plots
#text size on plots (e.g., axes)
#add table titles--perhaps to correlation table
#deal with all the variable num categories
#update edaTabBuilder code to make dt outputs optional (to adjust for mult)
#ability to bin choices? (vars into factor, logical, etc)
#convert larger server 'patterns' to functions
#output of missing names submenu/tab is a new DF object...thus a user can skip to, but not past, this section
#for missing name tab--need to have the first output (plot or DT) output in the same area
#perhaps add an option to compare before/after datasets re imputation using vis_compare()
#swap out my missingness function (data check tab) with the one from naniar?
#in missingness tab, consider adding option for nsets (or to select variables) for gg_miss_upset()--perhaps there's
  #a first drop down selectize with option to choose all and then user can select the missingness pattern from there
#add modals for imputation options that are risky
#make selectizeInput functions more flexible (and change edaTabBuilder)
#conditionally display subset of main tabs based on where user is
#feature scaling plots--axis labels and plot types (e.g., density, qq)
#in transformations tab, perhaps use the specific terms for the transforms (e.g., scaling, discretization) and add some
  #type of hyperlink or colored text where you hover over to get a more thorough defintion
#make the feature extraction-discretization plot interactive so a user can pull values for breaks
#a notebook?
#update annotations and add annotations
#user feedback: add it if user chooses beyond range and if user does not select at least two vars for luxury expense
  #variable
#fix group size plots not in descending order of frequency
#consider making group size variable switch code a function
#add option to barplotting function(s) to use different color schemes
#update functions so that they don't carry so many extraneous cols/vars
#discretization first plot--log scale y axis as option (and thus update formula)
# create another function script with a server suffix (for more 'structural' functions) & create functions
# add ggtitles to rare label encoding(?)



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

