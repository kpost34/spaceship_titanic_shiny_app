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
### No tuning
create_fit_model <- function(type, formula, folds) {
  
  #define model
  mod <- if(type=="log_reg") {
    logistic_reg(penalty=0.0000000001) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      translate()
    
  } else if(type=="dec_tree") {
    decision_tree() %>%
      set_engine("rpart") %>%
      set_mode("classification") %>%
      translate()
    
  } else if(type=="forest") {
    rand_forest() %>%
      set_engine("ranger") %>%
      set_mode("classification") %>%
      translate()
  }
    
  
  #construct workflow
  wf <- workflow() %>%
    add_model(mod) %>%
    add_formula(formula)
  
  #fit models via resampling
  fit_rs <- wf %>%
    fit_resamples(resamples=folds)
  
  #return fit obj
  return(fit_rs)
}


## Store model by selection order
store_model <- function(sel, mod_log, mod_tree, mod_forest) {

  mod_obj <- if(sel=="log_reg") {
    mod_log
  
  } else if(sel=="dec_tree") {
   mod_tree
  
  } else if(sel=="forest") {
    mod_forest
  }
}


## Assess model
assess_model <- function(fit_obj, simple=TRUE) {
  fit_obj %>%
    collect_metrics() %>%
    select(-.estimator) %>%
    rename(metric=".metric") %>%
    {if(simple) select(., -.config) else .} %>%
    mutate(across(where(is.double), ~signif(.x, 3)))
    
}


# Model Tuning======================================================================================


