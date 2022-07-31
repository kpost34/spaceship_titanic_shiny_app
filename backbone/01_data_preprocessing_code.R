# R code to develop Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 1 of x: reading in data and splitting variables; data checking, eda, character vector imputation, missingness, 
# data imputation

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


#### Data checking================================================================================================================
### Check numbers of rows and cols
dim(trainDF)

### Check appropriate rows and cols
str(trainDF)
glimpse(trainDF)
apply(trainDF,2,class)

### Preview data
head(trainDF,n=5)
tail(trainDF,n=5)
slice_sample(trainDF,n=5) #put in DT::datatable


### Missingness
apply(trainDF,2,function(x) sum(is.na(x))) 
apply(trainDF,2,function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c("col","n_missing")) %>%
  as_tibble() %>%
  arrange(desc(n_missing))


### Data summaries
skim(trainDF)
skim(trainDF,where(is.character)) %>% as_tibble() %>% select(-skim_type) %>% mutate(across(where(is.numeric),~signif(.x,3)))
skim(trainDF,where(is.factor)) %>% as_tibble() %>% select(-skim_type) %>% mutate(across(where(is.numeric),~signif(.x,3)))
skim(trainDF,where(is.logical)) %>% as_tibble() %>% select(-skim_type) %>% mutate(across(where(is.numeric),~signif(.x,3)))
skim(trainDF,where(is.numeric)) %>% as_tibble() %>% select(-skim_type) %>% mutate(across(where(is.numeric),~signif(.x,3)))


##### Exploratory Data Analysis====================================================================================================
#### Set colors for variables


#### Univariate--------------------------------------------------------------------------------------------------------------------
### Text/Tabular
## Numerical data
summary(trainDF["age"]) 
summary(trainDF["food_court"])

#tidyverse equivalent
trainDF %>%
  select(age) %>%
  summarize(across(age,list(n=length,
                   minimum=~min(.x,na.rm=TRUE),
                   q1=~quantile(.x,probs=0.25,na.rm=TRUE),
                   median=~median(.x,na.rm=TRUE),
                   mean=~mean(.x,na.rm=TRUE),
                   q3=~quantile(.x,probs=0.75,na.rm=TRUE),
                   maximum=~max(.x,na.rm=TRUE),
                   na=~sum(is.na(.x)))))
  

## Categorical or logical data
tabyl(trainDF,home_planet)
tabyl(trainDF,cryo_sleep)
tabyl(trainDF,destination)
tabyl(trainDF,transported)


### Graphical
## Histogram
trainDF %>%
  ggplot() +
  geom_histogram(aes(spa),fill="darkred",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))

## Barplot
trainDF %>%
  ggplot() +
  geom_bar(aes(home_planet),fill="steelblue",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))



#### Bivariate---------------------------------------------------------------------------------------------------------------------
### Tabular
## Cat-cat
tabyl(trainDF,home_planet,destination)
tabyl(trainDF,home_planet,transported)

## Cat-num
trainDF %>%
  group_by(transported) %>%
  summarize(across(age,list(n=length,
                            min=~min(.,na.rm=TRUE),
                            Q1=~quantile(.,probs=0.25,na.rm=TRUE),
                            median=~median(.,na.rm=TRUE),
                            mean=~mean(.,na.rm=TRUE),
                            Q3=~quantile(.,probs=0.75,na.rm=TRUE),
                            max=~max(.,na.rm=TRUE),
                            NAs=~sum(is.na(.))))) 

### Num-num
trainDF %>%
  cor_test(room_service,food_court,method="spearman")

trainDF %>%
  cor_test(room_service,food_court,method="spearman")


#### Graphical
### Cat-cat
## Bubble plot
# With NAs
trainDF %>%
  ggplot(aes(x=side,y=vip)) +
  geom_count() +
  theme_bw()

# Without NAs
trainDF %>%
  filter(!is.na(side),
         !is.na(vip)) %>%
  ggplot(aes(x=side,y=vip)) +
  geom_count() +
  theme_bw()


## Stacked bar plot
# With NAs
#count data
trainDF %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar() +
  scale_fill_manual(values=c("steelblue","coral4")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw()

#proportion data
trainDF %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar(position="fill") +
  theme_bw()

# Without NAs
#count data
trainDF %>%
  filter(!is.na(side),
         !is.na(transported)) %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar() +
  theme_bw()

#proportion data
trainDF %>%
  filter(!is.na(side),
         !is.na(transported)) %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar(position="fill") +
  theme_bw()


### Cat-num
## Bar plot
# With NAs (for Deck)
trainDF %>% 
  #remove NA values for spa
  filter(!is.na(spa)) %>%
  mutate(deck=fct_reorder(deck,spa,.fun=mean,.desc=TRUE)) %>% 
  ggplot(aes(x=deck,y=spa)) +
  stat_summary(geom="col",fun="mean",fill="steelblue",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,.1))) +
  theme_bw()

# Without NAs
trainDF %>%
  filter(!is.na(spa),
         !is.na(deck)) %>%
  mutate(deck=fct_reorder(deck,spa,.fun=mean,.desc=TRUE)) %>%
  ggplot(aes(x=deck,y=spa)) +
  stat_summary(geom="col",fun="mean",fill="steelblue",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,.1))) +
  theme_bw()


## Boxplots
# With NAs
trainDF %>%
  ggplot(aes(destination,shopping_mall,color=destination)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()


# Without NAs
trainDF %>%
  filter(!is.na(destination)) %>%
  ggplot(aes(destination,shopping_mall,color=destination)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()


### Num-num (scatterplots)
## room_service-vr_deck
trainDF %>%
  ggplot(aes(room_service,vr_deck)) +
  geom_point(alpha=0.5) +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="pseudo_log",expand=expansion(mult=c(0,.05))) +
  theme_bw()

## shopping_mall-spa
trainDF %>%
  ggplot(aes(shopping_mall,spa)) +
  geom_point(alpha=0.5) +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="pseudo_log",expand=expansion(mult=c(0,.05))) +
  theme_bw()



#### Multivariate (graphically only)----------------------------------------------------------------------------------------------
### Cat-cat-cat
trainDF %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar() +
  scale_fill_manual(values=c("steelblue","coral4")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  facet_wrap(~side) +
  theme_bw()


### Num-num-cat
trainDF %>%
  ggplot(aes(x=room_service,y=food_court,color=side)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


### Cat-cat-num
## Column plot
# With NAs
trainDF %>%
  ggplot(aes(x=cryo_sleep,y=age,fill=transported)) +
  stat_summary(geom="col",fun="mean",position="dodge") +
  stat_summary(geom="errorbar",fun.data=mean_se,position=position_dodge(0.95),width=0.5) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme_bw()

# Without NAs
trainDF %>%
  filter(!is.na(cryo_sleep),
         !is.na(transported)) %>%
  ggplot(aes(x=cryo_sleep,y=age,fill=transported)) +
  stat_summary(geom="col",fun="mean",position="dodge") +
  stat_summary(geom="errorbar",fun.data=mean_se,position=position_dodge(0.95),width=0.5) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme_bw()


## Boxplot (without NAs)
trainDF %>%
  #filter(!is.na(side)) %>%
  ggplot(aes(x=transported,y=food_court,color=side)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  theme(legend.position="bottom") +
  theme_bw()



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

### Drop col
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
explanatory_focus<-c("side")
confounders<-c("home_planet","destination","age","vip","cryo_sleep","room_service","food_court","shopping_mall","spa","vr_deck")

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



