# R code to develop Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 1 of x: data checking, character vector imputation, missingness, data imputation, feature engineering

#load packages
pacman::p_load(here,tidyverse,janitor,visdat,finalfit,skimr)


#### Read in data=================================================================================================================
train_inputDF<-read_csv(here("data","train.csv")) %>%
  clean_names()


#### Data checking================================================================================================================
### Check numbers of rows and cols
dim(train_inputDF)

### Check appropriate rows and cols
str(train_inputDF)
glimpse(train_inputDF)
apply(train_inputDF,2,class)

### Preview data
head(train_inputDF)
sample(train_inputDF)
tail(train_inputDF)


### Missingness
apply(train_inputDF,2,function(x) sum(is.na(x))) 


### Descriptive stats
## Numerical data
summary(train_inputDF["age"])
summary(train_inputDF["food_court"])

## Categorical data
tabyl(train_inputDF,home_planet)
tabyl(train_inputDF,cryo_sleep)
tabyl(train_inputDF,destination)


### Graphically
## Histogram
train_inputDF %>%
  ggplot() +
  geom_histogram(aes(spa),fill="steelblue") +
  theme_bw()

## Barplot
train_inputDF %>%
  ggplot() +
  geom_bar(aes(home_planet),fill="steelblue") +
  theme_bw()


##IDEAS##
#use switch() based on col type and then outputs summary/tabyl and histogram/bar plot


#### Simple feature engineering====================================================================================================
#enter: train_inputDf
#exit: trainDF

train_inputDF %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) -> trainDF


##### Character string imputationn=================================================================================================
#enter: trainDF
#exit: trainDF_nI

#### Assessment
trainDF %>%
  mutate(last_name=ifelse(!is.na(l_name),"Yes","NA")) %>%
  ggplot() +
  geom_bar(aes(x=last_name)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1)),scales::pseudo_log_trans()) +
  labs(x="Last name",
       y="Number of individuals") +
  theme_bw() 

#### Option? Ignore, drop col(s), remove rows, populate some using passenger group, populate with same room
### Ignore
trainDF -> trainDF_nI

### Drop col
trainDF %>% 
  select(-contains("name")) -> trainDF_nI

### Remove rows with NAs for name
trainDF %>%
  filter(!is.na(name)) -> trainDF_nI

### Populate some name values using passenger group
## Test run
trainDF %>%
  ## populate l_name using passenger_group 
  select(passenger_id,passenger_group,ticket,l_name) %>% 
  mutate(l_name_fill=l_name) %>% 
  group_by(passenger_group) %>%
  fill(l_name_fill) %>% View()
  #populates l_name if there is a non-NA l_name in same group

## Fill via passenger_group
trainDF %>%
  # populate l_name by passenger_group
  group_by(passenger_group) %>%
  fill(l_name) %>%
  ungroup() %>% 
  # re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK")) -> trainDF_nI #trainDF_nameImputed


### Populate some name values using cabin components
## Test run
trainDF %>%
  ## populate l_name_fill using deck, num, and side
  select(deck,num,side,l_name) %>% 
  mutate(l_name_fill=l_name) %>% 
  group_by(deck,num,side) %>% 
  fill(l_name_fill) %>% View()

## Fill via passenger_group
trainDF %>%
  ## populate l_name using deck, num, and side
  group_by(deck,num,side) %>% 
  fill(l_name) %>% 
  ungroup() %>% 
  # re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK")) -> trainDF_nI

### Remove NA names after filling
trainDF_nI %>%
  filter(!is.na(name)) -> trainDF_nI

### Don't remove NA names after filling
trainDF_nI


#### Data imputation (numerical and categorical)==================================================================================
#enters trainDF_nI
#exits i_trainDF

### Assess missingness
## graphical
vis_dat(trainDF_nI)
vis_miss(trainDF_nI)
trainDF_nI %>%
  missing_plot()


## tabular
trainDF_nI %>%
  ff_glimpse()

trainDF_nI %>%
  skim()


## understand pattern--MCAR, MAR, etc.
#notes: 
#Missing completely at random (MCAR)
#Missing at random (MAR)
#missing not at random (MNAR)
trainDF %>%
  missing_pattern()

train_inputDF %>%
  missing_pattern()



### Imputate data
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



