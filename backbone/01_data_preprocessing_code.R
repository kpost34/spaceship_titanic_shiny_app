# R code to develop Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 1 of x: data checking, simple feature engineering, data imputation, feature engineering

#load packages
pacman::p_load(here,tidyverse,janitor,visdat,finalfit)


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


##### Character string imputationn=================================================================================================
#enters train_inputDF
#exits trainDF

#### Option? Ignore, drop col(s), remove rows, populate some using passenger group, populate with same room?
### Ignore
train_inputDF -> trainDF

### Drop col
train_inputDF %>% 
  select(-name) -> trainDF

### Remove rows with NAs for name
train_inputDF %>%
  filter(!is.na(name)) -> trainDF

### Populate some name values using passenger group
## Test run
train_inputDF %>%
  ### split passenger_id into passenger_group and ticket
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### split (full) name into f_name and l_name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ## populate l_name using passenger_group 
  select(passenger_id,passenger_group,ticket,l_name) %>% 
  mutate(l_name_fill=l_name) %>% 
  group_by(passenger_group) %>%
  fill(l_name_fill) %>% View()
  #populates l_name if there is a non-NA l_name in same group

## Fill via passenger_group
train_inputDF %>%
  ## separate passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  # separate name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  # populate l_name by passenger_group
  group_by(passenger_group) %>%
  fill(l_name) %>%
  ungroup() %>% 
  # re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK"),
    .keep="unused") -> trainDF_nI #trainDF_nameImputed


### Populate some name values using cabin components
## Test run
train_inputDF %>%
  ### split cabin into deck, num, and side
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### split (full) name into f_name and l_name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ## populate l_name_fill using deck, num, and side
  select(deck,num,side,l_name) %>% 
  mutate(l_name_fill=l_name) %>% 
  group_by(deck,num,side) %>% 
  fill(l_name_fill) %>% View()

## Fill via passenger_group
train_inputDF %>%
  ### split cabin into deck, num, and side
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### split (full) name into f_name and l_name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ## populate l_name using deck, num, and side
  group_by(deck,num,side) %>% 
  fill(l_name) %>% 
  ungroup() %>% 
  # re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name,
    is.na(f_name) & !is.na(l_name) ~ paste(f_name,l_name),
    is.na(f_name) & is.na(l_name)  ~ "",
    TRUE                           ~ "CHECK"),
    .keep="unused") -> trainDF_nI

### Remove NA names after filling
trainDF_nI %>%
  filter(!is.na(name)) -> trainDF

### Don't remove NA names after filling
trainDF_nI -> trainDF


#### Data imputation (numerical and categorical)==================================================================================
#enters trainDF
#exits 

### Assess missingness
## graphical
vis_dat(trainDF)
vis_miss(trainDF)
trainDF %>%
  missing_plot()


## tabular
trainDF %>%
  ff_glimpse()


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
## family size
# l_name
train %>%
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


## traveling party size
# number of different tickets in a group
train %>%
  group_by(passenger_group) %>%
  mutate(party_size=n_distinct(ticket)) -> train

# by cabin
train %>%
  group_by(cabin) %>%
  mutate(party_size=n())


### Visualize new cols
## Descriptive stats
tabyl(train$fam_size)

## Graphically


#### Other possibilities
### provide user option to type in new col



