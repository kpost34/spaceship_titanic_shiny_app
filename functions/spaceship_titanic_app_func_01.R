#Functions for part 1 of x


#### Data Checking===================================================================
### Basic checking
## Function to return dimensions of data frame as tibble
dim_tbl<-function(x){
  require(tibble)
  dim(x) %>%
    t() %>%
    as_tibble(.name_repair="minimal") %>%
    setNames(c("rows","cols")) 
}


## Function to return number of missing values per col as tibble
n_miss_tbl<-function(x){
  require(tibble)
  apply(trainDF,2,function(x) sum(is.na(x))) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    setNames(c("col","n_missing")) %>%
    as_tibble(.name_repair="minimal") %>%
    arrange(desc(n_missing))
}


### Look at variable types more closely
### Function to provide summary by variable type
skim_tbl<-function(x,type="character"){
  require(skimr,tibble)
  col_type<-paste0("is.",type)
  x %>%
    skim(where(!!col_type)) %>%
    as_tibble(.name_repair="minimal") %>%
    select(-skim_type) %>%
    mutate(across(where(is.numeric),~signif(.x,3)))
}


#### EDA===============================================================================
### Univariate
## Tables
# Function to create tabyl with numerical values signifed
#for use with col in quotes
tabylize<-function(dat,col){
  require(janitor,dplyr)
  dat %>%
    tabyl(!!col) %>%
    mutate(across(where(is.numeric),~signif(.x,3)))
}


## Function to create tidyverse equivalent of summary()
#for use with col in quotes
summaryize<-function(dat,col){
  require(dplyr)
  dat %>%
    select(!!col) %>%
    summarize(across(!!col,list(minimum=~min(.x,na.rm=TRUE),
                              q1=~quantile(.x,probs=0.25,na.rm=TRUE),
                              median=~median(.x,na.rm=TRUE),
                              mean=~mean(.x,na.rm=TRUE),
                              q3=~quantile(.x,probs=0.75,na.rm=TRUE),
                              maximum=~max(.x,na.rm=TRUE),
                              na=~sum(is.na(.x))))) %>%
    mutate(across(where(is.numeric),~signif(.x,3)))
}


## Figures
# Function to create histogram of numeric variable
#for use with col name in quotes
histogramer<-function(dat,col){
  require(dplyr, ggplot2)
  dat %>%
    ggplot() +
    geom_histogram(aes_string(col),fill="darkred",color="black") +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13))
}

# Function to create bar plot of numeric variable
#for use with col name in quotes
barplotter<-function(dat,col){
  require(dplyr, ggplot2)
  dat %>%
    ggplot() +
    geom_bar(aes_string(col),fill="steelblue",color="black") +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13))
}






