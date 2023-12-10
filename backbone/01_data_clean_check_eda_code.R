#Created by Keith Post on 5/30/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 1 of x: reading in data and splitting variables; data checking, and eda

# Load Packages & Address Conflicts=================================================================
## Load packages
pacman::p_load(here, tidyverse, janitor, visdat, finalfit, skimr, GGally, rstatix)

## Address conflicts
filter <- dplyr::filter
chisq.test <- stats::chisq.test



# Read in data & initially clean data===============================================================
#exit: df_train
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


# Data Checking=====================================================================================
## Check numbers of rows and cols
dim(df_train)

## Check appropriate rows and cols
str(df_train)
glimpse(df_train)
apply(df_train, 2, class)

## Preview data
head(df_train, n=5)
tail(df_train, n=5)
slice_sample(df_train, n=5) 


## Missingness
#missing cases per variable...
#as a vector
apply(df_train, 2, function(x) sum(is.na(x))) 

#as a DF
apply(df_train, 2, function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c("col", "n_missing")) %>%
  as_tibble() %>%
  arrange(desc(n_missing)) %>% View()


## Data summaries
### Character variables
skim(df_train, where(is.character)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))

### Factors
skim(df_train, where(is.factor)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))

### Logical variables
skim(df_train, where(is.logical)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))

### Numeric variables
skim(df_train, where(is.numeric)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))


# Exploratory Data Analysis=========================================================================
## Set colors for variables


## Univariate----------------------------------------
### Text/Tabular--------------------
## Numerical data
summary(df_train["age"]) 
summary(df_train["food_court"])

#tidyverse equivalent
df_train %>%
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
tabyl(df_train,home_planet)
tabyl(df_train,cryo_sleep)
tabyl(df_train,destination)
tabyl(df_train,transported)


### Graphical--------------------
## Histogram
df_train %>%
  ggplot() +
  geom_histogram(aes(spa),fill="darkred",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))

## Barplot
df_train %>%
  ggplot() +
  geom_bar(aes(home_planet),fill="steelblue",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))



## Bivariate----------------------------------------
### Tabular--------------------
#### Cat-cat
tabyl(df_train, home_planet, destination)
tabyl(df_train, home_planet, transported)

#### Cat-num
df_train %>%
  group_by(transported) %>%
  summarize(across(age, list(n=length,
                             min=~min(.,na.rm=TRUE),
                             Q1=~quantile(.,probs=0.25,na.rm=TRUE),
                             median=~median(.,na.rm=TRUE),
                             mean=~mean(.,na.rm=TRUE),
                             Q3=~quantile(.,probs=0.75,na.rm=TRUE),
                             max=~max(.,na.rm=TRUE),
                             NAs=~sum(is.na(.)))))

#### Num-num (correlations)
df_train %>%
  cor_test(room_service, food_court, method="spearman")

df_train %>%
  cor_test(room_service, food_court, method="spearman")


### Graphical--------------------
#### Cat-cat
##### Bubble plot
#with NAs
df_train %>%
  ggplot(aes(x=side, y=vip)) +
  geom_count() +
  theme_bw()

#without NAs
df_train %>%
  filter(!is.na(side),
         !is.na(vip)) %>%
  ggplot(aes(x=side,y=vip)) +
  geom_count() +
  theme_bw()


##### Stacked bar plot
###### With NAs
#count data
df_train %>%
  ggplot(aes(x=side,fill=transported)) +
  geom_bar() +
  scale_fill_manual(values=c("steelblue", "coral4")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw()

#proportion data
df_train %>%
  ggplot(aes(x=side, fill=transported)) +
  geom_bar(position="fill") +
  theme_bw()

###### Without NAs
#count data
df_train %>%
  filter(!is.na(side),
         !is.na(transported)) %>%
  ggplot(aes(x=side, fill=transported)) +
  geom_bar() +
  theme_bw()

#proportion data
df_train %>%
  filter(!is.na(side),
         !is.na(transported)) %>%
  ggplot(aes(x=side, fill=transported)) +
  geom_bar(position="fill") +
  theme_bw()


#### Cat-num
##### Bar plot
#with NAs (for Deck)
df_train %>% 
  #remove NA values for spa
  filter(!is.na(spa)) %>%
  mutate(deck=fct_reorder(deck, spa, .fun=mean, .desc=TRUE)) %>% 
  ggplot(aes(x=deck, y=spa)) +
  stat_summary(geom="col", fun="mean", fill="steelblue", color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,.1))) +
  theme_bw()

#without NAs
df_train %>%
  filter(!is.na(spa),
         !is.na(deck)) %>%
  mutate(deck=fct_reorder(deck,spa,.fun=mean,.desc=TRUE)) %>%
  ggplot(aes(x=deck,y=spa)) +
  stat_summary(geom="col",fun="mean",fill="steelblue",color="black") +
  scale_y_continuous(expand=expansion(mult=c(0,.1))) +
  theme_bw()


##### Boxplots
#with NAs
df_train %>%
  ggplot(aes(destination, shopping_mall, color=destination)) +
  geom_boxplot() +
  scale_y_continuous(trans="pseudo_log",expand=expansion(mult=c(0,.05))) +
  scale_y_log10() +
  theme_bw()


#without NAs
df_train %>%
  filter(!is.na(destination)) %>%
  ggplot(aes(destination, shopping_mall, color=destination)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()


##### Num-num (scatterplots)
#room_service-vr_deck
df_train %>%
  ggplot(aes(room_service, vr_deck)) +
  geom_point(alpha=0.5) +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="pseudo_log", expand=expansion(mult=c(0,.05))) +
  theme_bw()

#shopping_mall-spa
df_train %>%
  ggplot(aes(shopping_mall, spa)) +
  geom_point(alpha=0.5) +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="pseudo_log", expand=expansion(mult=c(0,.05))) +
  theme_bw()



## Multivariate (graphically only)----------------------------------------
### Cat-cat-cat
df_train %>%
  ggplot(aes(x=side, fill=transported)) +
  geom_bar() +
  scale_fill_manual(values=c("steelblue", "coral4")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  facet_wrap(~side) +
  theme_bw()


### Num-num-cat
df_train %>%
  ggplot(aes(x=room_service, y=food_court, color=side)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


### Cat-cat-num
#### Column plot
#with NAs
df_train %>%
  ggplot(aes(x=cryo_sleep, y=age, fill=transported)) +
  stat_summary(geom="col", fun="mean", position="dodge") +
  stat_summary(geom="errorbar", fun.data=mean_se, position=position_dodge(0.95), width=0.5) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme_bw()

#without NAs
df_train %>%
  filter(!is.na(cryo_sleep),
         !is.na(transported)) %>%
  ggplot(aes(x=cryo_sleep, y=age, fill=transported)) +
  stat_summary(geom="col", fun="mean", position="dodge") +
  stat_summary(geom="errorbar", fun.data=mean_se, position=position_dodge(0.95),width=0.5) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme_bw()


#### Boxplot (without NAs)
df_train %>%
  filter(!is.na(side)) %>%
  ggplot(aes(x=transported, y=food_court, color=side)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  theme(legend.position="bottom") +
  theme_bw()

