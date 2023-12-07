#Created by Keith Post on 12/3/23

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 4 of x:  data partitioning

#load packages
pacman::p_load(here, tidyverse, janitor, rsample)


# Read in data & initially clean data===============================================================
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ### reclassify vars
  mutate(across(c(ticket, home_planet, deck:destination, transported), ~as.factor(.x))) -> df_train


# V-fold Cross Validation Examples==================================================================
#plan to use the following ranges:5 <= v <= 10; 1 <= repeats <= 10

# v = 5, repeats = 1
system.time(
  df_train %>%
    vfold_cv(v=5, repeats=1) -> df_train_5_1
) #.011

# v = 10, repeats = 1
system.time(
  df_train %>%
    vfold_cv(v=10, repeats = 1) -> df_train_10_1
) #.017



#v = 10, repeats = 2
system.time(
  df_train %>%
    vfold_cv(v=10, repeats=2) -> df_train_10_2
) #.029


#v = 10, repeats = 4
system.time(
  df_train %>%
    vfold_cv(v=10, repeats=4) -> df_train_10_4
) #.050


#v = 10, repeats = 6
system.time(
  df_train %>%
    vfold_cv(v=10, repeats=10) -> df_train_10_10
) #.100


#v = 20, repeats = 5
system.time(
  df_train %>%
    vfold_cv(v=20, repeats=5) -> df_train_20_5
) #.087

#v = 20, repeats = 20
system.time(
  df_train %>%
    vfold_cv(v=20, repeats=20) -> df_train_20_20
) #.339

#add strata = transported; stratified random sampling of transported (DV)

system.time(
  df_train_vfold <- df_train %>%
    vfold_cv(v=10, repeats=10, strata=transported)
) #0.186


#NOTE: the more important questions is...how long does it take to do the modelling if v and repeats
  #are high?

#NOTES: will need to add information on why choose greater/smaller v and repeats





