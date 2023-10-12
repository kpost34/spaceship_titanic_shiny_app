#### Create vectors
### Col names
## All character cols
trainDF %>% select(where(is.character)) %>% names() -> trainDF_chrVars
## All cols but character
trainDF %>% select(!where(is.character)) %>% names() -> trainDF_nchrVars
#excluding dep var
trainDF_nchrVars[trainDF_nchrVars!="transported"] -> trainDF_nchrPreds

## All logical and factor cols
trainDF %>% select(where(is.logical)|where(is.factor)) %>% names() -> trainDF_catVars

## All numeric cols
trainDF %>% select(where(is.numeric)|where(is.integer)) %>% names() -> trainDF_numVars

## All factor cols except for num
trainDF %>% select(where(is.factor),-num) %>% names() -> trainDF_fct_nonumVars

## Numeric vars + num
trainDF %>% select(where(is.numeric),num) %>% names() -> trainDF_disVars

## Cabin component cols
cabinVars<-c("deck","num","side")

## Dependent variable
depVar<-"transported"

## choices vectors
Chk01_quickVec<-c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")
Chk01_summVec<-c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")
namMis03_expVec<-c("missing example"="miss_samp","non-missing example"="nmiss_samp","summary table"="sum_tab","bar plot"="plot")
namMis03_impOptVec<-c("drop name columns"="drop_cols",
                      "remove with rows with missing names"="remove_rows",
                      "populate using passenger group"="imp_pass_group",
                      "populate using cabin info"="imp_cabin")
nchrMis03_expVec<-c("missing values occurrences"="miss_occur",
                   "missing values per variable" = "miss_var",
                   "missing values per observation" = "miss_obs",
                   "missing pattern"="miss_patt")
nchrMis03_impVec<-c("retain complete cases only"="lwise_del",
                    "remove variable(s) missing data"="var_del",
                    "mean imputation (numeric vars only)" = "mean_imp")
trnsFea04_transVec<-c("Feature Scaling",
                      "Discretization",
                      "Ordinal Encoding",
                      "Rare Label Encoding")
trnsFea04_transOptVec<-c("leave unchanged"="raw",
                          "log transform"="log",
                          "min-max scale"="mm_scale",
                          "standardize"="standize")
creFea04_grpSizeVec<-c("do not create a group size variable"="none",
                       "ticket group size (same passenger group)"="ticket_group_size",
                       "family size (passenger group & last name)"="family_size",
                       "travel party size (cabin)"="travel_party_size")
creFea04_luxVec<-c("do not create a luxury expense variable"="none", 
                   "room_service","food_court","shopping_mall","spa","vr_deck")