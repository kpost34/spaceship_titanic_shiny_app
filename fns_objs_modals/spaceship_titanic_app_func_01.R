#Created by Keith Post on 6/28/22

#Functions for spaceship titanic R shiny project: part 1 of x
#Codes for functions that match backbone 01 script: reading in data, data checking, and eda

#Load packages
pacman::p_load(tidyverse,skimr,janitor,purrr,rstatix)

# Data Checking===================================================================
## Basic checking
### Function to return dimensions of data frame as tibble
dim_tbl<-function(dat){
  dim(dat) %>%
    t() %>%
    as_tibble(.name_repair="minimal") %>%
    set_names(c("n_row","n_col")) 
}


### Function to return number of missing values per col as tibble
n_miss_tbl<-function(dat){
  apply(dat,2,function(x) sum(is.na(x))) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("variable","n_missing")) %>%
    as_tibble(.name_repair="minimal") %>%
    arrange(desc(n_missing)) %>%
    mutate(`complete?`=ifelse(n_missing==0, "Yes", "No"))
}


## Look at variable types more closely
### Function to provide summary by variable type
skim_tbl<-function(dat, type="character"){
  col_type <- paste0("is.",type)
  
  nm <- if(type=="character") {
    c("variable", "n_missing", "complete_rate", "min_chr", "max_chr", "empty_chr", "n_unique",
      "whitespace_chr")
    
  } else if(type=="factor") {
    c("variable", "n_missing", "complete_rate", "ordered?", "n_unique", "top_categories")
    
  } else if(type=="logical") {
    c("variable", "n_missing", "complete_rate", "prop_true", "counts")
    
  } else if(type=="numeric") {
    c("variable", "n_missing", "complete_rate", "mean", "sd", "min", "1st quartile", "median",
      "3rd quartile", "max", "histogram")
  }
  
  
  dat %>%
    skim(where(!!col_type)) %>%
    as_tibble(.name_repair="minimal") %>%
    select(-skim_type) %>%
    mutate(across(where(is.numeric),~signif(.x,3))) %>%
    set_names(nm)
}


##### EDA===============================================================================
#### Univariate-------------------------------------------------------------------------
### Tables
## Function to create tabyl with numerical values signifed
#for use with col in quotes
tabylize<-function(dat,vec){
  n<-length(vec)
  
  if(n==1){
    x1<-sym(vec)
    
    dat %>%
      tabyl(!!x1) %>%
      mutate(across(where(is.numeric),~signif(.x,3)),
             {{x1}} := factor(!!x1) %>% fct_explicit_na()) %>%
      rename_with(.cols=contains("percent"), ~str_replace(.x, "percent", "proportion"))
  }
  else if(n==2){
    x1<-sym(vec[1])
    x2<-sym(vec[2])
    
    dat %>%
      tabyl(!!x1,!!x2) %>%
      mutate({{x1}} := factor(!!x1) %>% fct_explicit_na())
  }
  else if(n==3){
    x1<-sym(vec[1])
    x2<-sym(vec[2])
    x3<-sym(vec[3])
    
    dat %>%
      tabyl(!!x1,!!x2,!!x3) %>%
      mutate({{x1}} := factor(!!x1) %>% fct_explicit_na())
  }
  else{return("Please use 1-3 variables")}
}


### Function to create tidyverse equivalent of summary() (for use with 1-2 vars)
#for use with character string or vector; 2nd var could be grouping variable (in quotes)
summaryize<-function(dat,vec,group=NA){
  
  #error message: size of vector
  n<-length(vec)
  if(n>2) {return("Use 1-2 variables")}
  
  #error message: class(es) of vector element(s)
  dat[vec] %>% 
    map_chr(class) %>%
    str_count("numeric|integer") %>%
    sum() -> num_cat
  
  if(num_cat!=1) {return("Use one numeric variable")}
  
  #store inputs as symbols
  s_var<-setdiff(vec,group) %>% sym()
  if(!is.na(group)) {
    s_group<-sym(group)
  }
  
  dat %>%
    #conditionally groups data by categorical variable (if present)
    {if(!is.na(group)) group_by(.,!!s_group) else .} %>%
    summarize(across(!!s_var,list(n=length,
                                  n_missing=~sum(is.na(.x)),
                                  complete_rate=~sum(!is.na(.x))/length(.x),
                                  mean=~mean(.x,na.rm=TRUE),
                                  sd=~sd(.x, na.rm=TRUE),
                                  min=~min(.x,na.rm=TRUE),
                                  `1st quartile`=~quantile(.x,probs=0.25,na.rm=TRUE),
                                  median=~median(.x,na.rm=TRUE),
                                  `3rd quartile`=~quantile(.x,probs=0.75,na.rm=TRUE),
                                  max=~max(.x,na.rm=TRUE)),
                     .names="{.fn}")) %>%
    mutate(across(where(is.numeric),~signif(.x,3))) %>%
    bind_cols(variable=vec[1], .)
}

### Figures
## Function to create histogram of numeric variable
#for use with col name in quotes
histogrammer<-function(dat,col){
  dat %>%
    ggplot() +
    geom_histogram(aes_string(col),fill="darkred",color="black") +
    {if(col!="age") scale_x_log10()} +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw(base_size=18) #+
    # theme(axis.text=element_text(size=12),
    #       axis.title=element_text(size=13))
}

## Function to create bar plot of numeric variable
#for use with col name(s) in quotes stored as an object
barplotter<-function(dat,vec,na.rm=FALSE){
    
  #pull number of vars
  n<-length(vec)
    
  #end if 4+ vars
  if(n>3){
    return("Use 1-3 columns only")
  }
  
  #end if numerical vars
  if(sum(map_chr(dat[vec],class) %in% c("logical","factor")==0)) {
    return("Use categorical variables only")
  }
    
  #allow re-sorting of vector (if n=2 or 3) based on # of cats
  if(n %in% 2:3) {
    dat[vec] %>%
      map_int(n_distinct) %>%
      sort(decreasing=TRUE) %>%
      names() -> vec
  }
    
  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    #order first cat var by frequency
    mutate(vec1=as.factor(!!sym(vec[1])) %>% fct_infreq() %>% fct_explicit_na()) %>%
    ggplot() +
    geom_bar(aes_string(x=quote(vec1),fill=ifelse(n %in% 2:3,vec[2],vec[1])),
             color="black") +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    xlab(vec[1]) +
    theme_bw(base_size=18) -> p
    # theme(axis.text=element_text(size=12),
    #       axis.title=element_text(size=13)) -> p
    
    
    if(n==1) {
      p + theme(legend.position="none") +
          scale_fill_manual(values=rep("darkblue",n_distinct(dat[vec])))
    }
    else if(n==2) (p + scale_fill_viridis_d())
    else if(n==3) {p + facet_wrap(vec[3]) +
                     scale_fill_viridis_d(na.value="grey50")
    }
  }

#### Bivariate-------------------------------------------------------------------------
### Tabular
## Cat-cat
#see tabylize() above

## Cat-num
#see summaryize() above

## Num-num
# Function to run spearman correlation test and signif results using character vector
corrtester<-function(dat,vec) {
  dat %>%
    cor_test(all_of(vec),method="spearman") %>%
    select(!starts_with("var")) %>%
    mutate(across(where(is.numeric),~signif(.x,3)),
           statistic=formatC(statistic,digits=3))
}


### Figures
## Cat-cat
#see barplotter()


## Cat-num
# Function to create boxplots for 2-3 variables (only one numeric)
boxplotter<-function(dat, vec, na.rm=FALSE) {
  
  n<-length(vec)
  #errors...too many columns/variables
  if(!n %in% 2:3) {
    return("Use only 2-3 columns")
  }
  
  #wrong categories
  if(sum(map_chr(dat[vec],class) %in% c("integer","numeric"))!=1|
     sum(map_chr(dat[vec],class) %in% c("logical","factor"))==0){
       return("Need one numeric and one to two categorical variables")
  }
  
  #re-order variables
  dat[vec] %>%
    map_int(n_distinct) %>%
      #col with most categories is in first position
      sort(decreasing=TRUE) %>%
      names() -> vec

  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    ggplot() +
      geom_boxplot(aes_string(x=vec[2],y=vec[1],color=ifelse(n==3,vec[3],vec[2]))) +
      coord_flip() +
      theme_bw() -> p
  
  if(n==2) {
 p + theme(legend.position="none") +
     scale_color_manual(values=rep("darkblue",n_distinct(dat[vec[2]])))
      #scale_color_viridis_d(end=.8,na.value="grey50")
  }
  else if(n==3) {
    p + theme(legend.position="bottom") +
        scale_color_viridis_d(end=.7,na.value="grey50")
  }
}


## Num-num
# Function to build scatterplots (and color points if third variable is chosen)
scatterplotter<-function(dat,vec,na.rm=FALSE){
  
  n<-length(vec)
  #errors...too many columns/variables
  if(!n %in% 2:3) {
    return("Use only 2-3 variables")
  }
  
  #wrong categories
  if(!sum(map_chr(dat[vec],class) %in% c("integer","numeric")) %in% 2:3|
     sum(map_chr(dat[vec],class) %in% c("logical","factor")) > 1){
    return("Need 2-3 numeric and max 1 categorical variables")
  }
  
  #re-order variables
  dat[vec] %>%
    map_int(n_distinct) %>%
    #col with most categories is in first position
    sort(decreasing=TRUE) %>%
    names() -> vec
  
  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    ggplot(aes_string(x=vec[2],y=vec[1])) +
    theme_bw() +
    theme(legend.position="bottom") -> p
      
  #if/else if/else
  if(n==2) {
    p + geom_point(color="darkred")
  }
  else if(n==3 & class(dat[[vec[3]]]) %in% c("logical","factor")) {
    p + geom_point(aes_string(color=vec[3])) + scale_color_viridis_d(na.value="grey50")
  }
  else if(n==3 & class(dat[[vec[3]]]) %in% c("intger","numeric")) {
    p + geom_point(aes_string(color=vec[3])) + scale_color_viridis_c()
  }
}


#### Multivariate (figures only)-------------------------------------------------------------------------
### Cat-cat-cat
## see barplotter() above


### Cat-num-cat
## see boxplotter() above


### Num-num-cat/num
## see scatterplotter() above)






