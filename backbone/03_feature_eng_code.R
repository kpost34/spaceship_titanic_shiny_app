#Created by Keith Post on 8/14/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 3 of x:  feature engineering

# Load Packages, Address Conflicts, Import Data, & Source Fns=======================================
## Load packages
pacman::p_load(here, tidyverse, janitor, cowplot, GGally)

## Address conflicts
filter  <-  dplyr::filter
chisq.test  <-  stats::chisq.test

## Data import (via sourcing)
fp_data  <-  here("backbone", "02_missing_imp_code.R")
source(fp_data)
#load: df_train_nd_nvI

## Source fns
fp_fn1 <- here("fns_objs_modals", "spaceship_titanic_app_func_01.R")
fp_fn3 <- here("fns_objs_modals", "spaceship_titanic_app_func_03.R")
source(fp_fn1)
source(fp_fn3)

# Data Transformation and Feature Extraction========================================================
#enter: df_train_nd_nvI
#exit: df_train_nd_nvI_tF

## Feature scaling--------------------
### Exploratory plots
p1 <- df_train_nd_nvI %>% 
  ggplot(aes(x=age)) +
  ggtitle("raw") +
  geom_density() +
  theme_bw()

p2 <- df_train_nd_nvI %>% 
  ggplot(aes(sample=age)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p3 <- df_train_nd_nvI %>% 
  ggplot(aes(x=log(age))) +
  ggtitle("log-transformed") +
  geom_density() +
  theme_bw()

p4 <- df_train_nd_nvI %>% 
  ggplot(aes(sample=log(age))) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p5 <- df_train_nd_nvI %>% 
  mutate(age_mm=(age-min(age, na.rm=TRUE))/(max(age, na.rm=TRUE)-min(age, na.rm=TRUE))) %>%
  ggplot(aes(x=age_mm)) +
  ggtitle("min-max scaled") +
  geom_density() +
  theme_bw()

p6 <- df_train_nd_nvI %>% 
  mutate(age_mm=(age-min(age, na.rm=TRUE))/(max(age, na.rm=TRUE)-min(age, na.rm=TRUE))) %>%
  ggplot(aes(sample=age_mm)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p7 <- df_train_nd_nvI %>% 
  mutate(age_stdize=((age-mean(age, na.rm=TRUE))/sd(age, na.rm=TRUE))) %>% 
  ggplot(aes(x=age_stdize)) +
  ggtitle("standardized") +
  geom_density() +
  theme_bw()

p8 <- df_train_nd_nvI %>% 
  mutate(age_stdize=((age-mean(age, na.rm=TRUE))/sd(age, na.rm=TRUE))) %>%
  ggplot(aes(sample=age_stdize)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()
  
plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8)

plot_grid(plotlist=plot_list, nrow=4)


### Conduct feature scaling [pick log-transform]
#### Raw
# df_train_nd_nvI_s <- df_train_nd_nvI

#### Log
df_train_nd_nvI_s <- df_train_nd_nvI %>%
  mutate(across(where(is.numeric), ~log(.x + 1), .names="{.col}_scale")) %>%
  select(passenger_id, ends_with("scale"))

#### Min-max scale
# df_train_nd_nvI_s <- df_train_nd_nvI %>%
#   mutate(across(where(is.numeric), ~min_max_scaler(.x), .names="{.col}_scale")) %>%
#   select(passenger_id, ends_with("scale"))

#### Standardized
# df_train_nd_nvI_s <- df_train_nd_nvI %>%
#   mutate(across(where(is.numeric), ~standardizer(.x), .names="{.col}_scale")) %>%
#   select(passenger_id, ends_with("scale"))


## Discretization--------------------
### Exploratory Plots
#### Raw data with fill=transported; user could choose num of bins and log scale
df_train_nd_nvI %>%
  mutate(room_service=if_else(room_service==0, .001, room_service, NA_real_)) %>%
  ggplot(aes(room_service)) +
  geom_histogram(aes(fill=transported), bins=40, color="black") +
  scale_x_log10() +
  scale_fill_viridis_d() +
  theme_bw() 


#### Binned data plots
#ggplot binning num var + filling by transported status
df_train_nd_nvI %>%
  mutate(room_service=if_else(room_service==0, .001, room_service, NA_real_)) %>%
  ggplot(aes(room_service)) +
  scale_x_binned(n.breaks=3, nice.breaks=TRUE) +
  scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  geom_bar(aes(fill=transported), position="dodge") +
  scale_fill_viridis_d() +
  theme_bw() 


### Manually binning boundaries
#### Plot discretized data
df_train_nd_nvI %>% 
  user_cutter(col="spa", break.vals=c(1, 10, 1000)) %>%
  ggplot() +
  geom_bar(aes(x=spa_dis, fill=transported), position="dodge") +
  scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  scale_fill_viridis_d() +
  theme_bw() 


#### Create DF [will use R-selected intervals]
# df_train_nd_nvI_d <- df_train_nd_nvI %>% 
#   user_cutter(col="spa", break.vals=c(1, 10, 1000)) %>%
#   select(-transported)


### Binning by equal intervals (R-selected) 
#### Plot discretized data
df_train_nd_nvI %>%
  equal_cutter(col="spa", n.breaks=3) %>%
  ggplot() +
  geom_bar(aes(x=spa_dis, fill=transported), position="dodge") +
  scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  scale_fill_viridis_d() +
  theme_bw() 


#### Create DF
df_train_nd_nvI_d <- df_train_nd_nvI %>%
  equal_cutter(col="spa", n.breaks=3) %>%
  select(-transported)



## Ordinal Encoding--------------------
  #(home_planet,  deck,  side,  destination,  ticket)
### Exploratory Plots
df_train_nd_nvI %>%
  #use barplotting function from eda
  barplotter("home_planet")

df_train_nd_nvI %>%
  barplotter("deck")

df_train_nd_nvI %>%
  barplotter("side")

df_train_nd_nvI %>%
  barplotter("destination")

df_train_nd_nvI %>% 
  barplotter("ticket")


### Perform ordinal encoding on ticket
#### Create levels for new ordered factor
lev <- c("08", "04", "02", "01", "07", "03", "06", "05")

### Create ordered factor with new levels
df_train_nd_nvI_o <- df_train_nd_nvI %>%
  mutate(ticket_ord=as.ordered(ticket),
         ticket_ord=fct_relevel(ticket_ord, lev)) %>% 
  select(passenger_id, ticket_ord) 



## Rare Label Encoding--------------------     
### Exploratory Plots
#### Barplots of counts
#ticket-raw
df_train_nd_nvI %>%
  mutate(ticket=fct_infreq(ticket)) %>%
  barplotter2("ticket")

#ticket-combine 07 & 08
df_train_nd_nvI %>%
  mutate(ticket=fct_collapse(ticket, other=c("07", "08")), 
         ticket=fct_infreq(ticket)) %>%
  barplotter2("ticket")


### Perform rare label encoding
df_train_nd_nvI_r <- df_train_nd_nvI %>%
  mutate(ticket_rare=fct_collapse(ticket, other=c("07", "08"))) %>%
  select(passenger_id, ticket_rare)


## Combine transformations--------------------
df_train_nd_nvI_tF <- df_train_nd_nvI_s %>%
  left_join(df_train_nd_nvI_d) %>%
  left_join(df_train_nd_nvI_o) %>%
  left_join(df_train_nd_nvI_r) %>%
  as_tibble()
  

# Feature Creation==================================================================================
#enter: df_train_nd_nvI
#exit: df_group_size


## Group size--------------------
#options: 
#1) ticket group size (same passenger group); 
#2) family size (passenger group + last name); 
#3) travel party size (cabin)
#4) none


### Ticket group size
#### Create feature
df_train_nd_nvI %>%
  group_by(passenger_group) %>%
  mutate(ticket_group_size=n(), 
         ticket_group_size=as.factor(ticket_group_size)) %>%
  ungroup() -> df_tix_grp_size

#### Plots
##### Group size only
df_tix_grp_size %>%
  barplotter("ticket_group_size")

# Group size + transported status
df_tix_grp_size %>%
  barplotter(c("ticket_group_size", "transported"))

df_tix_grp_size %>%
  barplotter2("ticket_group_size", title=FALSE)


### Family size
#### Create feature
df_train_nd_nvI %>%
  group_by(l_name, passenger_group) %>%
  mutate(fam_size=n(),
         fam_size=as.factor(fam_size)) %>%
  ungroup() -> df_fam_size


#### Plot feature
df_fam_size %>%
  # mutate(fam_size=fct_infreq(fam_size)) %>%
  barplotter("fam_size")

df_fam_size %>%
  # mutate(fam_size=fct_infreq(fam_size)) %>%
  barplotter2("fam_size", title=FALSE)



### Traveling party size
#### Create feature
df_train_nd_nvI %>%
  group_by(cabin) %>%
  mutate(n=n(),
         travel_party_size=ifelse(n > 100, NA_character_, paste(n)),
         travel_party_size=as.factor(travel_party_size)) %>%
  ungroup() -> df_trav_party_size

#### Plot feature
df_trav_party_size %>%
  mutate(travel_party_size=fct_infreq(travel_party_size)) %>%
  barplotter("travel_party_size")

df_trav_party_size %>%
  mutate(travel_party_size=fct_infreq(travel_party_size)) %>%
  barplotter2("travel_party_size", title=FALSE)


### Choose a feature
df_group_size <- df_trav_party_size %>%
  select(passenger_id, ends_with("size"))


## Luxury Expenses--------------------
#enter: df_train_nd_nvI
#exit: df_lux_expense

### Exploratory plots
#### Heat map
df_train_nd_nvI %>%
  select(room_service:vr_deck) %>%
  ggcorr(label=TRUE, digits=3) 

df_train_nd_nvI %>%
  select(room_service, food_court) %>%
  ggcorr(label=TRUE, digits=3)

#all scatterplots
df_train_nd_nvI %>%
  select(room_service:vr_deck) %>%
  pairs()
#note: takes a while to plot

#### Scatterplot of one pair
df_train_nd_nvI %>%
  ggplot(aes(room_service, food_court)) +
  geom_point() +
  facet_wrap(~transported) +
  geom_smooth(method="lm") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


#### Boxplots
#one var
df_train_nd_nvI %>%
  ggplot() +
  geom_boxplot(aes(x=transported, y=room_service)) +
  scale_y_log10() +
  theme_bw()

#summing two vars
df_train_nd_nvI %>%
  rowwise() %>%
  mutate(luxury=sum(room_service, food_court, na.rm=TRUE)) %>% 
  ungroup() %>%
  ggplot() +
  geom_boxplot(aes(x=transported, y=luxury)) +
  scale_y_log10() +
  theme_bw()

#summing three vars
df_train_nd_nvI %>%
  rowwise() %>%
  mutate(luxury=sum(room_service, spa, vr_deck, na.rm=TRUE)) %>% 
  ungroup() %>%
  ggplot() +
  geom_boxplot(aes(x=transported, y=luxury)) +
  scale_y_log10() +
  theme_bw()
#choose this for feature creation


### Create luxury expense 
df_lux_expense <- df_train_nd_nvI %>%
  mutate(room_service_spa_vr_deck_lux=sum(room_service, spa, vr_deck, na.rm=TRUE)) %>%
  select(passenger_id, ends_with("lux"))


## Combine all new features## Group size--------------------
#enter: df_train_nd_nvI
#exit: df_train_nd_nvI_tcF

df_train_nd_nvI_tcF <- df_train_nd_nvI %>% #entering DF
  left_join(df_train_nd_nvI_tF) %>% #pass id + transformed features
  left_join(df_group_size) %>% #pass id + group size var
  left_join(df_lux_expense) %>%  #pass id + lux expense
  as_tibble()











