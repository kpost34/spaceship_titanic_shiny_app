# Objects

# Read in data======================================================================================
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"), sep="_", remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"), sep="/", remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"), sep=" ", remove=FALSE) %>% 
  ### reclassify vars
  mutate(across(c(ticket, home_planet, deck, side, destination), ~as.factor(.x))) -> df_train


#temporarily create a full DF
df_train %>% 
  mutate(across(where(is.double), ~log(.x + 1), .names="{.col}_scale")) %>%
  #remove NAs by dropping rows
  filter(if_all(everything(), ~!is.na(.x))) -> df_train_full



# Create vectors====================================================================================
## Col names (variables)--------------------
### All character cols
df_train %>% select(where(is.character)) %>% names() -> chrVars


### All cols but character
#### Colnames
df_train %>% select(!where(is.character)) %>% names() -> nchrVars
nchrVarswFn <- c(nchrVars, "floor_num")
#excluding dep var
nchrVars[nchrVars!="transported"] -> nchrPreds
#with floor_num
nchrPredswFn <- c(nchrPreds, "floor_num")


#### Classes
df_train %>% select(!where(is.character)) %>% map(class) %>% as.character() -> nchrClass
ncharVarClass <- nchrVars %>%
  set_names(paste0(nchrVars, " (", nchrClass, ")"))


### All logical and factor cols
df_train %>% select(where(is.logical)|where(is.factor)) %>% names() -> catVars


### All numeric cols
df_train %>% select(where(is.numeric)) %>% names() -> numVars


### All factor cols 
df_train %>% select(where(is.factor)) %>% names() -> fct_nonumVars


### Numeric vars
df_train %>% select(where(is.numeric)) %>% names() -> disVars


## Missing exploration cols
vars_miss_exp <- c("passenger_id", "passenger_group", "ticket", "cabin", "home_planet", 
                   "destination", "name")


### Cabin component cols
cabinVars<-c("deck", "num", "side")


### Dependent variable
depVar<-"transported"



## Choices vectors--------------------
ch_quick_dataCheck <- c("dimensions"="dim",
                         "data sample"="dat_samp",
                         "missingness"="miss")

ch_summ_dataCheck <- c("character"="chr",
                       "factor"="fct",
                       "logical"="lgl",
                       "numeric"="num")

ch_exp_nm_missName <- c("missing example"="miss_samp",
                        "non-missing example"="nmiss_samp",
                        "summary table"="sum_tab")

ch_imp_opt_missName <- c("drop name columns"="drop_cols",
                         "remove rows with missing names"="remove_rows",
                         "populate names using passenger group"="imp_pass_group",
                         "populate names using cabin info"="imp_cabin")

ch_exp_nnm_missOther <- c("missing values occurrences"="miss_occur",
                          "missing values per variable" = "miss_var",
                          "missing values per observation" = "miss_obs")


ch_impute_missOther <-c("retain complete cases only"="lwise_del",
                        "mean imputation (numeric vars) with most frequent category" = "mean_imp",
                        "median imputation (numeric vars) with most frequent category" = "med_imp",
                        "multiple imputation - predictive mean matching" = "mult_imp")

ch_trans_featTrans <- c("Feature Scaling",
                        "Discretization",
                        "Ordinal Encoding",
                        "Rare Label Encoding")

ch_trans_opt_featTrans <- c("leave unchanged"="raw",
                            "log transform"="log",
                            "min-max scale"="mm_scale",
                            "standardize"="standize")

ch_bin_opt_featTrans <- c("Equal intervals"="cut_int", 
                           "User specifications"="user")

ch_grp_size_featCreat <- c("do not create a group size variable"="none",
                           "ticket group size (same passenger group)"="ticket_group_size",
                           "family size (passenger group & last name)"="family_size",
                           "travel party size (cabin)"="travel_party_size")

# ch_lux_featCreat <- c("do not create a luxury expense variable"="none", 
ch_lux_featCreat <- c("room_service",
                      "food_court",
                      "shopping_mall",
                      "spa",
                      "vr_deck")

ch_mod_type <- c("Logistic Regression"="log_reg",
                 "Decision Trees"="dec_tree",
                 "K-Nearest Neighbors"="knn")


## Long strings--------------------
### missNum module
chr_1a_missNum <- "cabin is a character string composed of three variables: deck, num, and side. The former"
chr_1b_missNum <- "and latter can and have been easily converted to a factor. But num has over 1800 values,"
chr_1c_missNum <- "which do not possess any numeric value. Thus, it may be easiest to convert these into"
chr_1d_missNum <- "coarser groups, preferably according to some multiple of the hundreds digit, which often"
chr_1e_missNum <- "indicates the floor number. Thus, there will be an option to bin num by individual floors"
chr_1f_missNum <- "or ranges of 2, 3, or 4 floors."

chr_1_missNum <- paste(chr_1a_missNum, chr_1b_missNum, chr_1c_missNum, chr_1d_missNum, 
                       chr_1e_missNum, chr_1f_missNum)


### missName module
chr_1a_missName <- "Two hundred out of 8693 passengers (in the training data) lack names. That's 2.3%."
chr_1b_missName <- "Although first names, and thus full names, will be impossible to impute from the other"
chr_1c_missName <- "variables, last names may be populated with confidence through group membership."

chr_1_missName <- paste(chr_1a_missName, chr_1b_missName, chr_1c_missName)

chr_2a_missName <- "If we assume passengers traveled together as families, then families could be"
chr_2b_missName <- "identified by..."
chr_2ab_missName <- paste(chr_2a_missName, chr_2b_missName)

chr_2c_missName <- "  1) purchasing tickets together (same passenger group)"
chr_2d_missName <- "  2) staying in the same room (cabin)." 

chr_2_missName <- paste("<h4>", chr_2ab_missName, "<br>",
                        chr_2c_missName, "<br>",
                        chr_2d_missName, "</h4>")

chr_2d_missName <- "Let's look at frequencies of passenger and cabin group sizes."



## Strings for selector labels--------------------
varViz_feat<-"Please select a variable to visualize"
varSel_feat<-"Please select which variables for ordinal encoding"
varSelOrd_feat<-c("Rank all from least to most important"="")
scaleOpt_feat<-"Please select the type of scaling for the list of numerical variables"












