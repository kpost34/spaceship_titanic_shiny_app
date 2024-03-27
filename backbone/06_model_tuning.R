#Created by Keith Post on 3/22/24

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 6 of x:  model tuning


#load packages
pacman::p_load(here, tidymodels, rpart.plot, vip)


# Read in Data from Data Partitioning===============================================================
source(here("backbone", "05_modelling_code.R"))
#generates df_vfold and the various model specification, workflow, and fit_resamples objects



# Tuning Hyperparameters============================================================================
## Logistic regression--------------------
### Create model specification
tune_spec_log <- logistic_reg(
    penalty=tune(),
    mixture=tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  translate()

tune_spec_log

### Create grid of hyperparameters
log_grid <- grid_regular(penalty(), 
                          mixture(),
                          levels=3) #app will have slider with 2-5

log_grid


### Construct workflow
log_wf <- workflow() %>%
  add_model(tune_spec_log) %>% #new model specification
  add_formula(transported ~ home_planet + side + destination + floor + age_scale + ticket_rare + 
                room_service__spa__vr_deck_lux)

log_wf


### Model tuning with a grid
log_res <- log_wf %>%
  tune_grid(
    resamples=df_vfold,
    grid=log_grid
  )

log_res


### Assess results
#### Statistically
collect_metrics(log_res) %>% View()


#### Visually
log_res %>%
  collect_metrics() %>%
  mutate(mixture=factor(mixture)) %>%
  ggplot(aes(penalty, mean, color=mixture)) +
  geom_line(linewidth=1.5, alpha=0.6) +
  geom_point(size=2) +
  facet_wrap(~ .metric, scales="free", nrow=2) +
  scale_x_log10(labels=scales::label_number()) +
  scale_color_viridis_d(option="plasma", begin=0.9, end=0)


### Show best models
#### Top five
log_res %>%
  show_best("accuracy")

log_res %>%
  show_best("roc_auc")


#### Single best
log_res %>%
  select_best("accuracy") #smallest penalty and mixture = 0.75

best_log <- log_res %>%
  select_best("roc_auc") #same as best using accuracy


### Finalize model
final_log_wf <- log_wf %>%
  finalize_workflow(best_log)


### Fit model to training data
final_log_fit <- final_log_wf %>%
  last_fit()



## Decision trees via CART--------------------
### Create model specification
tune_spec_tree <- decision_tree(
  cost_complexity=tune(),
  tree_depth=tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  translate()

tune_spec_tree


### Create grid of hyperparameters
tree_grid <- grid_regular(cost_complexity(), 
                          tree_depth(),
                          levels=3)

tree_grid


## Construct workflow
tree_wf <- workflow() %>%
  add_model(tune_spec_tree) %>% #new model specification
  add_formula(transported ~ home_planet + side + destination + floor + travel_party_size +
              age_scale + ticket_rare + room_service__spa__vr_deck_lux)


### Model tuning with a grid
tree_res <- tree_wf %>%
  tune_grid(
    resamples=df_vfold,
    grid=tree_grid
  )

tree_res


### Assess results
#### Statistically
collect_metrics(tree_res) %>% View()


#### Visually
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth=factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color=tree_depth)) +
  geom_line(linewidth=1.5, alpha=0.6) +
  geom_point(size=2) +
  facet_wrap(~ .metric, scales="free", nrow=2) +
  scale_x_log10(labels=scales::label_number()) +
  scale_color_viridis_d(option="plasma", begin=0.9, end=0)
#roc_auc
#tree_depth=1 performs poorly at all cost_complexity values
#at highest complexity, all tree_depth levels perform poorly
#at tree_depth=4, performance is similar across the lowest four cost_complexity values
#tree_depth=8 has greater performance at the lowest three cost_complexity values
#tree_depths of 8, 11, and 15 have peak performance at second highest cost_complexity


### Show best models
#### Top five
tree_res %>%
  show_best("accuracy")

tree_res %>%
  show_best("roc_auc")
#looks like they are cost_complexity = 0.000562 and tree_depth of 8, 11, or 15


#### Single best
tree_res %>%
  select_best("accuracy") #tree_depth = 15

best_tree <- tree_res %>%
  select_best("roc_auc") #tree_depth = 8


### Finalize model
final_tree_wf <- tree_wf %>%
  finalize_workflow(best_tree)


### Fit model to training data
final_tree_fit <- final_tree_wf %>%
  last_fit()














