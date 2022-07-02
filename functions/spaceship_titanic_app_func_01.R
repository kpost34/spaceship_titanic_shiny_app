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

dim_tbl(trainDF)


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

n_miss_tbl(trainDF)


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

skim_tbl(trainDF,type="factor")


