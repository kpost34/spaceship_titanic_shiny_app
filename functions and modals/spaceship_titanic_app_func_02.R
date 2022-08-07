#Created by Keith Post on 8/5/22

#Functions for spaceship titanic R shiny project: part 2 of x
#Code for functions that match backbone 01 script: exploring missing data and data imputation

#Load packages
pacman::p_load(tidyverse,skimr,janitor,purrr,rstatix)


#### Character Data Missingness==========================================================================
### Exploring character data missingness-----------------------------------------------------------------
## Function to provide summary table of missingness
chr_miss_tabler<-function(dat){
  dat %>%
    pivot_longer(cols=contains("name"),names_to="name_type",values_to="name") %>%
    group_by(name_type) %>%
    summarize(across(name,list(present=~sum(!is.na(.x)),missing=~sum(is.na(.x)),total=length)))
}

## Function to provide summary barlot of missingness
chr_miss_boxplotter<-function(dat){
  dat %>%
    summarize(across(contains("name"),~ifelse(!is.na(.x),"Present","Missing"))) %>%
    pivot_longer(cols=everything(),names_to="name_type",values_to="name") %>%
    ggplot() +
    geom_bar(aes(x=name_type,fill=name),color="black") +
    scale_x_discrete(labels=c("first name","last name","full name")) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    scale_fill_viridis_d(end=0.5) +
    labs(x="") +
    theme_bw() +
    theme(legend.title=element_blank())
}


### Relationship between name missingness and passenger_group or room (cabin) occupancy & imputation
## Function that provides of counts of named passengers grouped by another variable
mis_name_tabler<-function(dat,name,group){
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
    group_by(num_name,group_size) %>%
    summarize(n=n(),
              group_comp=list({{group}})) %>%
    ungroup()
}


## Function that provides column graphs using above table output
col_plotter<-function(dat,group,count){
  dat %>%
    ggplot(aes(x={{group}},y={{count}})) +
    geom_col(fill="darkgreen") +
    labs(x="Number of named passengers",
         y=paste("Number of groups",sep=" ")) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw()
}


### Function to impute last names using passenger group size or cabin occupancy
name_imputer<-function(tab,col,range,dat,var) {
  tab %>%
    #filter summary table of missing names by range of number named
    filter(between({{col}},range[1],range[2])) %>%
    #extract the group composition (e.g., list of passenger_groups)
    pull(group_comp) %>%
    unlist() -> filter_var
  
  dat %>%
    #use subset of groups for filtering DF
    filter({{var}} %in% filter_var) %>%
    group_by({{var}}) %>%
    #assumes that dat contains cols f_name, l_name, and name (for full name)
    #populate l_name using grouping var
    fill(l_name) %>%
    ungroup() %>%
    #mutate (full) name after imputing l_name
    mutate(name=case_when(
      !is.na(name)                   ~ name,
      is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
      is.na(f_name) & is.na(l_name)  ~ "",
      TRUE                           ~ "CHECK")) %>% 
    bind_rows(dat %>% 
                filter(!{{var}} %in% filter_var)) 
}
