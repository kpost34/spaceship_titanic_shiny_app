#Created by Keith Post on 12/3/23

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 4 of x:  data partitioning

# Load Packages and Address Conflicts, Import Data, & Source Fns via Sourcing 03 Backbone===========
## Load packages
pacman::p_load(rsample)

## Source script
source(here("backbone", "03_feature_eng_code.R"))


# V-fold Cross Validation Examples==================================================================
#enter: df_train_select
#exit: df_vfold

#plan to use the following ranges:5 <= v <= 10; 1 <= repeats <= 10

## Assess times of various vfold settings
# v = 5, repeats = 1
system.time(
  df_train_select %>%
    vfold_cv(v=5, repeats=1) -> df_train_5_1
) #.02

# v = 10, repeats = 1
system.time(
  df_train_select %>%
    vfold_cv(v=10, repeats = 1) -> df_train_select_10_1
) #.016


#v = 10, repeats = 2
system.time(
  df_train_select %>%
    vfold_cv(v=10, repeats=2) -> df_train_select_10_2
) #.030


#v = 10, repeats = 4
system.time(
  df_train_select %>%
    vfold_cv(v=10, repeats=4) -> df_train_select_10_4
) #.052


#v = 10, repeats = 6
system.time(
  df_train_select %>%
    vfold_cv(v=10, repeats=10) -> df_train_select_10_10
) #.106


#v = 20, repeats = 5
system.time(
  df_train_select %>%
    vfold_cv(v=20, repeats=5) -> df_train_select_20_5
) #.084

#v = 20, repeats = 20
system.time(
  df_train_select %>%
    vfold_cv(v=20, repeats=20) -> df_train_select_20_20
) #.684 (less than 1 second)

#add strata = transported; stratified random sampling of transported (DV)

system.time(
  df_train_select_vfold <- df_train_select %>%
    vfold_cv(v=10, repeats=10, strata=transported)
) #0.147


## Run vfold partitioning 
### Stratify by transported with v = 10, no repeats
df_vfold <- df_train_select %>%
  vfold_cv(v=10, repeats=2, strat=transported)


#NOTE: the more important questions is...how long does it take to do the modelling if v and repeats
  #are high?

#NOTE: will need to add information on why choose greater/smaller v and repeats 


# Data Hygiene======================================================================================
## Remove extraneous obj
rm(list=setdiff(ls(), "df_vfold"))






