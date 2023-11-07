#Created by Keith Post on 6/28/22

#Functions for spaceship titanic R shiny project: part 1 of x
#Codes for functions that match backbone 01 script: reading in data, data checking, and eda

#Load packages
pacman::p_load(tidyverse, skimr, janitor, purrr, rstatix)


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
  #grab col classes
  df_classes <- dat %>%
    sapply(class) %>%
    enframe(name="variable", value="type")
  
  
  apply(dat,2,function(x) sum(is.na(x))) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("variable","n_missing")) %>%
    as_tibble(.name_repair="minimal") %>%
    arrange(desc(n_missing)) %>%
    mutate(`complete?`=ifelse(n_missing==0, "Yes", "No")) %>%
    left_join(df_classes) %>%
    select(variable, type, n_missing, `complete?`)
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


# EDA===============================================================================================
## Univariate--------------------
### Tables
#### Function to create tabyl with numerical values signifed
#for use with col in quotes
tabylize <- function(dat, vec){
  n <- length(vec)
  
  if(n==1){
    x1<-sym(vec)
    
    dat %>%
      tabyl(!!x1) %>%
      mutate(across(where(is.numeric), ~signif(.x,3)),
             {{x1}} := factor(!!x1) %>% fct_na_value_to_level(level="NA")) %>%
      rename_with(.cols=contains("percent"), ~str_replace(.x, "percent", "proportion"))
  }
  else if(n==2){
    x1<-sym(vec[1])
    x2<-sym(vec[2])
    
    dat %>%
      # tabyl(!!x1,!!x2) %>%
      mutate({{x1}} := factor(!!x1) %>% fct_na_value_to_level(level="NA"),
             {{x2}} := factor(!!x2) %>% fct_na_value_to_level(level="NA")) %>%
      tabyl(!!x1,!!x2)
  }
  else if(n==3){
    x1<-sym(vec[1])
    x2<-sym(vec[2])
    x3<-sym(vec[3])
    
    dat %>%
      tabyl(!!x1,!!x2,!!x3)
  }
  else{return("Please use 1-3 variables")}
}


#### Function to create tidyverse equivalent of summary() (for use with 1-2 vars)
#for use with character string or vector; 2nd var could be grouping variable (in quotes)
summaryize <- function(dat, vec, group=NA){
  
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
  s_var <- setdiff(vec,group) %>% sym()
  
  if(!is.na(group)) {
    s_group <- sym(group)
  }
  
  
  dat %>%
    #conditionally groups data by categorical variable (if present)
    {if(!is.na(group)) group_by(.,!!s_group) else .} %>%
    summarize(across(!!s_var,list(n=length,
                                  n_missing=~sum(is.na(.x)),
                                  complete_rate=~sum(!is.na(.x))/length(.x),
                                  mean=~mean(.x, na.rm=TRUE),
                                  sd=~sd(.x, na.rm=TRUE),
                                  min=~min(.x, na.rm=TRUE),
                                  `1st quartile`=~quantile(.x,probs=0.25, na.rm=TRUE),
                                  median=~median(.x, na.rm=TRUE),
                                  `3rd quartile`=~quantile(.x,probs=0.75, na.rm=TRUE),
                                  max=~max(.x, na.rm=TRUE)),
                     .names="{.fn}")) %>%
    mutate(across(where(is.numeric),~signif(.x, 3))) %>%
    {if(!is.na(group))
      mutate(., "{{s_group}}" := factor(!!s_group) %>%
               fct_na_value_to_level(level="NA")) else .} %>%
    {if(n==1) bind_cols(variable=vec[1], .) else .}
}

### Figures
#### Function to create histogram of numeric variable
#for use with col name in quotes
histogrammer<-function(dat, col){
  #convert & rename col as 's_col'
  s_col <- sym(col)
  
  dat %>%
    ggplot() +
    #use bang-bang operator
    geom_histogram(aes(!!s_col),fill="darkred",color="black") +
    {if(col!="age") scale_x_log10()} +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw(base_size=19) 
}

#### Function to create bar plot of numeric variable
#for use with col name(s) in quotes stored as an object
barplotter <- function(dat, vec, na.rm=FALSE){
    
  #pull number of vars
  n<-length(vec)
    
  #end if 4+ vars
  if(n>3){
    return("Use 1-3 columns only")
  }
  
  #end if numerical vars
  if(sum(map_chr(dat[vec], class) %in% c("logical", "factor")==0)) {
    return("Use categorical or logical variables only")
  }
    
  #allow re-sorting of vector (if n=2 or 3) based on # of cats
  if(n %in% 2:3) {
    dat[vec] %>%
      map_int(n_distinct) %>%
      sort(decreasing=TRUE) %>%
      names() -> vec
  }
  
  #turn vector components into symbols
  vec1 <- sym(vec[1])
  
  if(n > 1) {
    vec2 <- sym(vec[2])
  }

  if(n==3) {
    vec3 <- sym(vec[3])
  }
  
  fill_value <- if(n > 1) {
    vec2
  } else {vec1}
  
  
  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE) (filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    ggplot() +
    geom_bar(aes(x=!!vec1, fill=!!fill_value), color="black") + 
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw(base_size=19) -> p
  
    if(n==1) {
        p + theme(legend.position="none") +
            scale_fill_manual(values=rep("darkblue",n_distinct(dat[vec])), na.value="grey50")
      }
      else if(n==2) (
        p + scale_fill_viridis_d(na.value="grey50") +
            theme(legend.position="bottom"))
      else if(n==3) {
        p + facet_wrap(vars(!!vec3), dir="v", scale="free_y") +
            scale_fill_viridis_d(na.value="grey50") +
            theme(legend.position="bottom")
      }
}
  

## Bivariate--------------------
### Tabular
#### Cat-cat
#see tabylize() above

## Cat-num
#see summaryize() above

## Num-num
# Function to run spearman correlation test and signif results using character vector
corrtester<-function(dat,vec) {
  dat %>%
    cor_test(all_of(vec),method="spearman") %>%
    select(!starts_with("var")) %>%
    mutate(statistic=formatC(statistic,format="g", digits=3),
           p=formatC(p, format="f", digits=3),
           p=ifelse(p=="0.000",
                  "< 0.001",
                  p))
}


### Figures
#### Cat-cat
#see barplotter()


#### Cat-num
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
  
  #turn vector components into symbols
  vec1 <- sym(vec[1])
  vec2 <- sym(vec[2])

  if(n==3) {
    vec3 <- sym(vec[3])
  }
  
  #set color value conditionally--it's a symbol
  color_value <- if(n == 3) {
    vec3
  } else {vec2}

  
  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    #code retains a string argument to pass as column name, modifies, and uses it later by
      #original name; it's critical here to a) create an NA level and b) reverse the levels
    {if(n==3) mutate(., {{vec3}} := factor(!!vec3) %>%
                       fct_na_value_to_level() %>% 
                       fct_rev()) else .} %>%
    ggplot() +
      #evaluate vec1 & vec2 (they are symbols) & use curly-curly convention for color
      geom_boxplot(aes(x=!!vec1, y=!!vec2, color={{color_value}})) +
      #log-transform axis if not age
      {if(vec[1]!="age") scale_x_log10()} +
      #limits=rev puts NA at bottom on y-axis
      scale_y_discrete(limits=rev) +
      theme_bw(base_size=19) -> p
  
  if(n==2) {
 p + theme(legend.position="none") +
     scale_color_manual(values=rep("darkblue",n_distinct(dat[vec[2]])))
  }
  else if(n==3) {
    p + theme(legend.position="bottom") +
        #na.value still works if NA is an explicit level; how to manually state breaks to align
          #plot & legend level orders
        scale_color_viridis_d(end=.7, direction=-1, na.value="grey50", breaks=unique(dat[[vec[3]]]))
  }
}


#### Num-num
##### Function to build scatterplots (and color points if third variable is chosen)
scatterplotter<-function(dat,vec,na.rm=FALSE){
  
  n<-length(vec)
  #errors...too many columns/variables
  if(!n %in% 2:3) {
    return("Use only 2-3 variables")
  }
  
  #wrong categories
  n_num <- sum(map_chr(dat[vec],class) %in% c("integer","numeric"))
  n_cat <- sum(map_chr(dat[vec],class) %in% c("logical","factor"))
  
  if(!n_num %in% 2:3|n_cat >1) {
    return("Need 2-3 numeric and max 1 categorical variables")
  }
  
  #sort and make vars symbols
  if(n_cat==1) {
    #sort 
    dat[vec] %>%
      map_int(n_distinct) %>%
      sort(decreasing=TRUE) %>%
      names() -> vec
  } 
  
  if(n==3) {
    vec3 <- sym(vec[3])
  }
    
  vec1 <- sym(vec[1])
  vec2 <- sym(vec[2])
  
  #make plot
  dat %>%
    #conditional filter on na.rm arg
    {if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
    ggplot(aes(x=!!vec2, y=!!vec1)) +
    {if(vec[2]!="age") scale_x_log10()} +
    {if(vec[1]!="age") scale_y_log10(expand=expansion(mult=c(0,0.1))) 
      else scale_y_continuous(expand=expansion(mult=c(0,0.1)))} +
    theme_bw(base_size=19) +
    theme(legend.position="bottom") -> p
      
  #if/else if/else
  if(n==2) {
    p + geom_point(color="darkred", size=2, alpha=0.8)
  }
  else if(n==3 & class(dat[[vec[3]]]) %in% c("logical","factor")) {
    p + 
      geom_point(aes(color=!!vec3), size=2, alpha=0.7) + 
      scale_color_viridis_d(end=.7, na.value="grey50") +
      theme(legend.key.width=unit(3, "cm")) +
      guides(color=guide_legend(override.aes=list(size=4)))
  }
  else if(n==3 & class(dat[[vec[3]]]) %in% c("integer","numeric")) {
    p + 
      geom_point(aes(color=!!vec3), size=2, alpha=0.7) + 
      {if(vec[3]!="age") 
        scale_color_viridis_c(end=.8, na.value="grey50", trans="log", 
                              labels=label_number(accuracy=10, big.mark=","))
        else scale_color_viridis_c(end=.8, na.value="grey50")} +
      theme(legend.key.width=unit(3, "cm")) 
  }
}


## Multivariate (figures only)--------------------
### Cat-cat-cat
## see barplotter() above


### Cat-num-cat
## see boxplotter() above


### Num-num-cat/num
## see scatterplotter() above)






