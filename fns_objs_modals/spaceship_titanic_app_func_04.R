#Created by Keith Post on 3/30/24

#Functions for spaceship titanic R shiny project: part 4 of x
#Code for functions that match backbone 05 and 06 scripts: modelling and model tuning code

#Load packages
pacman::p_load(tidyverse)


# Modelling=========================================================================================
## Convert DF to formula
grab_formula <- function(df) {
  #extract colnames from df
  nm <- names(df)
  
  #grab predictors--all names that are not the id or dep var
  predictors <- nm[!nm %in% c("passenger_id", "transported")]
  
  #create formula
  form <- as.formula(paste0("transported ~ ", paste(predictors, collapse=" + ")))
  
  #return formula
  return(form)
}


## Create and fit model
create_fit_model <- function(type, formula, folds) {
  
  #define model
  mod <- if(type=="log_reg") {
    logistic_reg(penalty=0) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      translate()
  } else if(type=="dec_tree") {
    decision_tree() %>%
      set_engine("rpart") %>%
      set_mode("classification") %>%
      translate()
  } else if(type=="knn") {
    nearest_neighbor() %>%
      set_engine("kknn") %>%
      set_mode("classification") %>%
      translate()
  }
  
  #construct workflow
  wf <- workflow() %>%
    add_model(mod) %>%
    add_formula(formula)
  
  #fit models via resampling
  fit_rs <- wf %>%
    fit_resamples(folds)
  
  #return fit obj
  return(fit_rs)
}
    


# Model Tuning======================================================================================


