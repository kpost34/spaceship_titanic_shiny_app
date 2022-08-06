#Created by Keith Post on 5/30/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 1 of x: reading in data and splitting variables; data checking, and eda

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

