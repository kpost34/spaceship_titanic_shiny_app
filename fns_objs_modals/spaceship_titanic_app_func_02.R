#Created by Keith Post on 8/5/22

#Functions for spaceship titanic R shiny project: part 2 of x
#Code for functions that match backbone 01 script: exploring missing data and data imputation

#Load packages
pacman::p_load(tidyverse, skimr, janitor, purrr, rstatix)


# Character Data Missingness========================================================================
## num missingness--------------------
### Helper function to extract and reduce upper limit by one
grab_reduce_floor <- function(dat) {
  dat %>%
    mutate(floor_num_temp=as.character(floor_num_temp),
           new_upr=str_extract(floor_num_temp, "(?<=\\,)[0-9]{1,2}") %>% as.integer(),
           new_upr=as.character(new_upr - 1),
           floor_num_temp=str_remove_all(floor_num_temp, "\\[|\\]|\\)"),
           floor_num_temp=str_replace(floor_num_temp, ",", "_"),
           floor_num_temp=str_replace(floor_num_temp, "[0-9]{1,2}$", new_upr),
           floor_num=as.factor(floor_num_temp)) %>%
    select(-c(floor_num_temp, new_upr))
}


### Function to bin num into floor groups
group_floors <- function(dat, nbin) {
  dat %>%
    mutate(num_num=as.numeric(num),
           floor=num_num %/% 100) %>%
    {if(nbin > 1)
      mutate(., 
             floor_num_temp=cut_width(floor, 
                                      width=nbin, 
                                      boundary=0, 
                                      closed="left")) %>%
      grab_reduce_floor()
      else mutate(., floor_num=as.factor(floor))} %>%
    select(-c(num_num, floor))
}


### Function to display toast notification
create_floor_num_msg <- function(number) {
  if(number==1){
    return("New variable 'floor_num' comprises individual floors")
  } else if(number > 1) {
    msg <- paste("New variable 'floor_num' comprises", number, "floors")
    return(msg)
  }
}
  


## Name missingness--------------------
### Function to provide summary table of missingness
chr_miss_tabler<-function(dat){
  dat %>%
    pivot_longer(cols=contains("name"), names_to="name_type", values_to="name") %>%
    group_by(name_type) %>%
    summarize(across(name, list(present=~sum(!is.na(.x)), missing=~sum(is.na(.x)), total=length)))
}


### Relationship between name missingness and passenger_group or room (cabin) occupancy & imputation
#### Function that provides of counts of named passengers grouped by another variable
mis_name_tabler<-function(dat, name, group){
  dat %>%
    #filter for missing names
    filter(is.na({{name}}) & !is.na({{group}})) %>%
    #pull out grouping variable
    pull({{group}}) -> filter_var
  
  dat %>%
    #select only groups containing at least one NA name
    filter({{group}} %in% filter_var) %>%
    #count number of named passengers and group size in each group
    group_by({{group}}) %>%
    summarize(num_name=sum(!is.na({{name}})), 
              group_size=length({{name}})) %>%
    #summarize by calculated metrics
    group_by(num_name, group_size) %>%
    summarize(n=n(), 
              group_comp=list({{group}})) %>%
    ungroup() 
}


#### Function that provides column graphs using above table output
col_plotter<-function(dat,  group,  count,  input){
  
  fill_value <- if(input=="passenger_group") {
    "darkgreen"
  } else if(input=="cabin") {
    "purple"
  }

  dat %>%
    ggplot(aes(x=as.character({{group}}), y={{count}})) +
    geom_col(fill=fill_value,  color="black") +
    labs(x="Number of named passengers", 
         y=paste("Number of groups", sep=" "), 
         caption="- Each group,  regardless of size,  has one unnamed passenger.") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    theme_bw(base_size=18) +
    theme(plot.caption=element_text(hjust=0,  vjust=0))
}


#### Function to impute last names using passenger group size or cabin occupancy
name_imputer<-function(tab, col, range, dat, var) {
  tab %>%
    #filter summary table of missing names by range of number named
    filter(between({{col}}, range[1], range[2])) %>%
    #extract the group composition (e.g.,  list of passenger_groups)
    pull(group_comp) %>%
    unlist() -> filter_var
  
  dat %>%
    #use subset of groups for filtering DF
    filter({{var}} %in% filter_var) %>%
    group_by({{var}}) %>%
    #assumes that dat contains cols f_name,  l_name,  and name (for full name)
    #populate l_name using grouping var
    fill(l_name) %>%
    ungroup() %>%
    #mutate (full) name after imputing l_name
    mutate(name=case_when(
      !is.na(name)                   ~ name, 
      is.na(f_name) & !is.na(l_name) ~ paste(f_name, l_name), 
      is.na(f_name) & is.na(l_name)  ~ "", 
      TRUE                           ~ "CHECK")) %>% 
    bind_rows(dat %>% 
                filter(!{{var}} %in% filter_var)) 
}


#### Function to display toast notification
impute_name_msg <- function(action) {
  if(action=="drop_cols") {
    return("Name columns have been dropped")
    
  } else if(action=="remove_rows") {
    return("Rows with missing names have been removed")
    
  } else if(action=="imp_pass_group") {
    return("Names have been populated using passenger group")
    
  } else if(action=="imp_cabin") {
    return("Names have been populated using cabin info")
  }
}



## Predictor missingness--------------------
### Function to display toast notification after imputation
impute_predictor_msg <- function(action) {
  if(action=="lwise_del") {
    return("Incomplete cases removed")
    
  } else if(action=="mean_imp") {
    return("Mean imputation completed")
    
  } else if(action=="med_imp") {
    return("Median imputation completed")
    
  } else if(action=="mult_imp") {
    return("Predictive mean matching completed")
  }
}



# Archive===========================================================================================
## Function to provide summary barplot of missingness
chr_miss_barplotter<-function(dat){
  dat %>%
    summarize(across(contains("name"), ~ifelse(!is.na(.x), "Present", "Missing"))) %>%
    pivot_longer(cols=everything(), names_to="name_type", values_to="name") %>%
    ggplot() +
    geom_bar(aes(x=name_type, fill=name), color="black") +
    scale_x_discrete(labels=c("first name", "last name", "full name")) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_fill_viridis_d(end=0.5) +
    labs(x="") +
    theme_bw() +
    theme(legend.title=element_blank())
}