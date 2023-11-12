# Read in data
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ### reclassify vars
  mutate(across(c(ticket,home_planet,deck:destination),~as.factor(.x))) -> df_train




# Create vectors
## Col names
### All character cols
df_train %>% select(where(is.character)) %>% names() -> chrVars

### All cols but character
df_train %>% select(!where(is.character)) %>% names() -> nchrVars
#excluding dep var
df_train_nchrVars[df_train_nchrVars!="transported"] -> nchrPreds

### All logical and factor cols
df_train %>% select(where(is.logical)|where(is.factor)) %>% names() -> catVars

### All numeric cols
df_train %>% select(where(is.numeric)) %>% names() -> numVars

### All factor cols except for num
df_train %>% select(where(is.factor),-num) %>% names() -> fct_nonumVars

### Numeric vars + num
df_train %>% select(where(is.numeric),num) %>% names() -> disVars

## Missing exploration cols
vars_miss_exp <- c("passenger_id", "passenger_group", "ticket", "cabin", "home_planet", 
                   "destination", "name")

### Cabin component cols
cabinVars<-c("deck","num","side")

### Dependent variable
depVar<-"transported"


## Choices vectors
vec_quick_chk <- c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")
# Chk01_quickVec<-c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")

vec_summ_chk <- c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")
# Chk01_summVec<-c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")

namMis03_expVec<-c("missing example"="miss_samp","non-missing example"="nmiss_samp",
                   "summary table"="sum_tab")

namMis03_impOptVec<-c("drop name columns"="drop_cols",
                      "remove rows with missing names"="remove_rows",
                      "populate names using passenger group"="imp_pass_group",
                      "populate names using cabin info"="imp_cabin")

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


## Long strings
### 
str_missName1a <- "Two hundred out of 8693 passengers (in the training data) lack names. That's 2.3%."
str_missName1b <- "Although first names, and thus full names, will be impossible to impute from the other"
str_missName1c <- "variables, last names may be populated with confidence if we assume passengers"
str_missName1d <- "traveled together as families."

str_missName1 <- paste(str_missName1a, str_missName1b, str_missName1c, str_missName1d)

str_missName2a <- "Two ways the traveling party is a family is by either..."

str_missName2b <- "  1) purchasing tickets together (same passenger group)"
str_missName2c <- "  2) staying in the same room (cabin)." 

str_missName2d <- "Let's look at frequencies of passenger and cabin group sizes."

# str_missName2 <- paste("<h4>", str_missName2a, "<br>", 
#                       str_missName2b, "<br>",
#                       str_missName2c, "<br>",
#                       "<br>",
#                       str_missName2d, "</h4>")

str_missName2 <- paste("<h4>", str_missName2a, "<br>",
                      str_missName2b, "<br>",
                      str_missName2c, "</h4>")








