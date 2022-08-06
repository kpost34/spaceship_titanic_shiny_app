#Created by Keith Post on 8/6/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 2 of x:  exploring missing data, imputing names, and imputing non-character variables

#load packages
pacman::p_load(here,tidyverse,janitor,visdat,finalfit,skimr,GGally,rstatix,conflicted)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


#### Read in data & initially clean data==========================================================================================
#exit: trainDF
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ### reclassify vars
  mutate(across(c(home_planet,deck:destination),~as.factor(.x))) -> trainDF


##### Character string imputation==================================================================================================
#enter: trainDF
#exit: trainDF_nI

#### Assessment--------------------------------------------------------------------------------------------------------------------
### Non-missing vs missing names
## Tabular
# Sample
#missing
trainDF %>%
  filter(is.na(name)) %>%
  slice_sample(n=2)

#non-missing
trainDF %>%
  filter(!is.na(name)) %>%
  slice_sample(n=2)


# Summary table
trainDF %>%
  pivot_longer(cols=contains("name"),names_to="name_type",values_to="name") %>%
  group_by(name_type) %>%
  summarize(across(name,list(present=~sum(!is.na(.x)),missing=~sum(is.na(.x)),total=length)))


# Graphical
trainDF %>%
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


## Name missingness + passenger group
# Pull passenger groups containing one NA name
trainDF %>%
  #filter for NA l_name
  filter(is.na(l_name)) %>%
  #extract passenger group
  pull(passenger_group) -> pass_group_NA_name

# Calculate numbers of each size of passenger_group and the numbers of named passengers
trainDF %>%
  #select only passenger_groups containing at least one NA l_name
  filter(passenger_group %in% pass_group_NA_name) %>% 
  group_by(passenger_group) %>%
  #calculate number of non-NA names out of number of inds in pass group
  summarize(num_name=sum(!is.na(l_name)),
            pass_group_size=length(l_name)) %>%
  #group by calculated metrics
  group_by(num_name,pass_group_size) %>%
  #returns counts and lists of passenger groups
  summarize(n=n(),
            pass_groups=list(passenger_group)) %>%
  ungroup() -> passGroupNAnameSizes_tab
#NA l_names--number of groups with at least one non-NA l_name and number of groups without any others
#or by number of non-NA l_names (0, 1, 2, etc.)

# Plot results
passGroupNAnameSizes_tab %>%
  ggplot(aes(x=num_name,y=n)) +
  geom_col(fill="darkgreen") +
  labs(x="Number of named passengers",y="Number of passenger groups") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() 


## Name missingness + same room
# Note: two passenger ids where cabin and last name NA (so only 198 NA names where there is a cabin)
trainDF %>% filter(is.na(cabin) & is.na(l_name))

# Pull non-NA cabins containing at least one passenger with NA name
trainDF %>%
  filter(!is.na(cabin) & is.na(l_name)) %>%
  pull(cabin) -> room_group_NA_name

# Calculate numbers of each room size (i.e., # of inds) and the numbers of named passengers
trainDF %>%
  filter(cabin %in% room_group_NA_name) %>%
  group_by(cabin) %>%
  summarize(num_name=sum(!is.na(l_name)),
            room_group_size=length(l_name)) %>% 
  group_by(num_name,room_group_size) %>%
  summarize(n=n(),
            room_groups=list(cabin)) %>%
  ungroup() -> roomGroupNAnameSizes_tab

#conclusions from both assessments: if num_name==0 then can't populate; the larger the group then logically the greater the 
#confidence in filling name--this could be part of the method

# Plot results
roomGroupNAnameSizes_tab %>%
  ggplot(aes(x=num_name,y=n)) +
  geom_col(fill="tan4") +
  labs(x="Number of named passengers",y="Number of cabins") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() 

#### Option? Ignore, drop col(s), remove rows, populate some using passenger group, populate with same room-----------------------
### Ignore
trainDF -> trainDF_nI

### Drop cols
trainDF %>% 
  select(-contains("name")) -> trainDF_nI

### Remove rows with NAs for name (i.e., list-wise deletion)
trainDF %>%
  filter(!is.na(name)) -> trainDF_nI


### Populate some name values using passenger group (e.g. num_name of 1, 2, or 3)
## Pull out vector of pass_groups (NOTE: turn this into function)
passGroupNAnameSizes_tab %>%
  filter(between(num_name,1,3)) %>%
  pull(pass_groups) %>% 
  unlist() %>%
  as.integer()-> passGroupNAnames1_3

## Filter by passenger groups then fill by named passengers in pass groups (also a function)
trainDF %>%
  #filter by passenger_group
  filter(passenger_group %in% passGroupNAnames1_3) %>%
  #populate l_name by passenger_group
  group_by(passenger_group) %>%
  fill(l_name) %>%
  ungroup() %>% 
  #re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK")) %>% 
  #recombine with the filtered out rows based on passenger_group
  bind_rows(trainDF %>%
              filter(!passenger_group %in% passGroupNAnames1_3)) -> trainDF_nI #trainDF_nameImputed


### Populate some name values using cabin components (e.g., num_name of at least 2)
## Pull out vector of room groups
roomGroupNAnameSizes_tab %>%
  filter(num_name >=2) %>% 
  pull(room_groups) %>%
  unlist() -> roomGroupNAnames2larger

## Filter by passenger groups then fill by named passengers in pass groups (also a function)
trainDF %>%
  #filter by cabin
  filter(cabin %in% roomGroupNAnames2larger) %>% 
  #populate l_name by passenger_group
  group_by(cabin) %>%
  fill(l_name) %>%
  ungroup() %>% 
  #re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK")) %>% 
  #recombine with the filtered out rows based on passenger_group
  bind_rows(trainDF %>%
              filter(!cabin %in% roomGroupNAnames2larger)) -> trainDF_nI #trainDF_nameImputed


#### Handling NAs after filling names --------------------------------------------------------------------------------------------
### Remove NA names after filling
trainDF_nI %>%
  filter(!is.na(name)) -> trainDF_nI

### Don't remove NA names after filling
trainDF_nI


##### Data imputation (numerical and categorical)==================================================================================
#enters trainDF_nI
#exits i_trainDF

#### Missingness------------------------------------------------------------------------------------------------------------------
### Code variables
explanatory<-trainDF_nI %>%
  select(-c(passenger_id:ticket,cabin,name:last_col())) %>%
  names()
dependent<-"transported"


### Identify missing values in each variable
## Tabular visualization
trainDF_nI %>%
  ff_glimpse(dependent,explanatory)

trainDF_nI %>%
  skim()

## Graphical visualization
vis_dat(trainDF_nI)
vis_miss(trainDF_nI)

trainDF_nI %>%
  missing_plot(dependent,explanatory)
#cols with missing data: home_planet, cryo_sleep, deck, num, side, destination, age, vip, room_service, food_court, 
#shopping_mall, spa, vr_deck (aside from character vars...all but transported)


### Look for patterns of missingness
trainDF_nI %>%
  missing_pattern(dependent,explanatory) #%>% dim()
#there are 77 patterns of missingness



### Include missing data in demographics tables
## Redefine variables (explanatory and confounders)
explanatory_focus<-c("destination")
confounders<-c("home_planet","vip","cryo_sleep","room_service","food_court","shopping_mall","spa","vr_deck")

trainDF_nI %>%
  summary_factorlist(explanatory_focus,confounders,na_include=TRUE,na_include_dependent=TRUE,
                     total_col=TRUE,add_col_totals=TRUE,p=TRUE)



### Check for associations between missing and observed data
## Visually
trainDF_nI %>%
  missing_pairs(dependent,explanatory)


## Statistically
# Test vr_deck
#redefine variables (note that dependent is simply the variable being tested for missingness)
exp_vr_deck<-c(explanatory,dependent) %>%
  setdiff("vr_deck")
dep_vr_deck<-"vr_deck"

#run test
trainDF_nI %>%
  missing_compare(dep_vr_deck,exp_vr_deck) %>%
  filter(p!="")
#nothing significant


# Test deck
#redefine variables (note that dependent is simply the variable being tested for missingness)
exp_deck<-c(explanatory,dependent) %>%
  setdiff(c("deck","num","side")) #remove deck ("dep" var) + num and side (because missingness is the same)
dep_deck<-"deck"

#run test
trainDF_nI %>%
  missing_compare(dep_deck,exp_deck) %>%
  filter(p!="")
#cryo_sleep, room_service, and spa


# Test home_planet
#redefine variables (note that dependent is simply the variable being tested for missingness)
exp_home_planet<-c(explanatory,dependent) %>%
  setdiff("home_planet")
dep_home_planet<-"home_planet"

#run test
trainDF_nI %>%
  missing_compare(dep_home_planet,exp_home_planet) %>% 
  filter(p!="")
#nothing significant

#### Handling missing data--------------------------------------------------------------------------------------------------------
### MCAR
#list-wise deletion
#imputation

### MAR
#imputation 
#omit the variable


### Impute data
## individually
l_name


## all


### Post-imputation data check
## remaining missingness?

## new ranges/averages/counts--perhaps do a pre- vs post

## graphically

#use vis_compare()




#### Feature engineering==========================================================================================================
### Name
i_trainDF %>%
  separate(name,into=c("f_name","l_name",sep=" "),remove=FALSE) -> fi_trainDF


### Cabin



### Family size
## l_name
trainDF %>%
  group_by(l_name) %>%
  mutate(fam_size=n()) -> train

# l_name + passenger_group
train %>%
  group_by(l_name,passenger_group) %>%
  mutate(fam_size=n()) -> train

# l_name + cabin
train %>%
  group_by(l_name,cabin) %>%
  mutate(fam_size=n()) -> train

# l_name + passenger_group + cabin
train %>%
  group_by(l_name,passenger_group,cabin) %>%
  mutate(fam_size=n()) -> train


### Traveling party size
## Number of different tickets in a group
train %>%
  group_by(passenger_group) %>%
  mutate(party_size=n_distinct(ticket)) -> train

## Number of people in same cabin (room)
train %>%
  group_by(cabin) %>%
  mutate(party_size=n())


### Age groups


### Visualize new cols
## Descriptive stats
tabyl(train$fam_size)

## Graphically


#### Other possibilities
### provide user option to type in new col
