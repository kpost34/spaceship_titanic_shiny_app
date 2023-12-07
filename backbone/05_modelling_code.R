#Created by Keith Post on 12/4/23

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 5 of x:  modelling

#load packages
pacman::p_load(here, tidyverse, janitor, recipes, parsnip, workflows, yardstick)


# Read in Data from Data Partitioning===============================================================
source(here("backbone", "04_data_partitioning_code.R"))
#generates df_train_vfold


# Modelling Code====================================================================================
## Create recipe
ship_recipe <- recipe(df_train) %>%
  update_role(passenger_id, new_role="id variable") %>%
  update_role(ticket, home_planet, cryo_sleep, deck, side, destination, age, vip, room_service,
              food_court, shopping_mall, spa, vr_deck, new_role="predictor") %>%
  update_role(transported, new_role="outcome")

ship_recipe


## Specify model
### Logistic model
ship_mod_log <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm") %>%
  translate()

ship_mod_log


### Decision tree
ship_mod_dec_tree <- decision_tree(tree_depth=integer(1), min_n=integer(1), cost_complexity=double(1)) %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  translate()


## Construct workflow
### Logistic model
ship_mod_log_wflow <- workflow() %>%
  add_recipe(ship_recipe) %>%
  add_model(ship_mod_log)

ship_mod_log_wflow 

### Decision tree
ship_mod_dec_tree_wflow <- workflow() %>%
  add_recipe(ship_recipe) %>%
  add_model(ship_mod_dec_tree)


## Fit models
### Logistic model
ship_mod_log_wflow_fit <- parsnip::fit(ship_mod_log_wflow, data=df_train)
ship_mod_log_wflow_fit


### Decision tree
ship_mod_dec_tree_wflow_fit <- parsnip::fit(ship_mod_dec_tree_wflow, data=df_train)


## Assess model accuracy
### Logistic model
pred_ship_log <- predict(ship_mod_log_wflow_fit, new_data=df_train)
yardstick::accuracy_vec(truth=df_train$transported,
                        estimate=pred_ship_log$.pred_class) #0.797605
count(df_train, transported) #4315 FALSE, 4378 TRUE
count(pred_ship_log, .pred_class) #3202 FALSE, 3562 TRUE, 1929 NA



