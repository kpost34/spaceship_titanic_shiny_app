#Created by Keith Post on 8/14/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 3 of x:  feature engineering

# Load Packages, Address Conflicts, Source Fns, & Import Data=======================================
## Load packages
pacman::p_load(here, tidyverse, janitor, cowplot, GGally)

## Address conflicts
filter  <-  dplyr::filter
chisq.test  <-  stats::chisq.test

## Source fns


## Data import (via sourcing)
fp_data  <-  here("backbone", "02_missing_imp_code.R")
source(fp_data)
#enter: df_train_nd_nvI

# Data transformation and feature extraction========================================================
## Data transformation (feature scaling)
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


### Transformation
#### Raw
df_train_nd_nvI_s <- df_train_nd_nvI

#### Log
df_train_nd_nvI_s <- df_train_nd_nvI %>%
  mutate(across(where(is.numeric), ~log(.x + 1), .names="{.col}_scale")) %>%
  select(passenger_id, ends_with("scale"))

#### Min-max scale
df_train_nd_nvI_s <- df_train_nd_nvI %>%
  mutate(across(where(is.numeric), ~min_max_scaler(.x), .names="{.col}_scale")) %>%
  select(passenger_id, ends_with("scale"))

#### Standardized
df_train_nd_nvI_s <- df_train_nd_nvI %>%
  mutate(across(where(is.numeric), ~standardizer(.x), .names="{.col}_scale")) %>%
  select(passenger_id, ends_with("scale"))


## Discretization
### Exploratory Plots
#### Raw data with fill=transported; user could choose num of bins and log scale
df_train_nd_nvI %>%
  mutate(room_service=if_else(room_service==0, .001, room_service, NA_real_)) %>%
  ggplot(aes(room_service)) +
  geom_histogram(aes(fill=transported), bins=40, color="black") +
  scale_x_log10() +
  scale_fill_viridis_d() +
  theme_bw() 


## Binned data plots
# ggplot binning num var + filling by transported status
df_train_nd_nvI %>%
  #mutate(room_service=if_else(room_service==0, .001, room_service, NA_real_)) %>%
  ggplot(aes(room_service)) +
  scale_x_binned(n.breaks=3, nice.breaks=FALSE) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  #scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() 


# Manually binning boundaries and plotting 
#specify bin boundaries (i.e.,  break locations) and plot as binned plot
cuts <- c(1, 10, 100)


df_train_nd_nvI %>%
  #cutting numerical variable by min,  selected value,  and max,  and including lowest value
  mutate(room_service_cut=cut(room_service, 
                              breaks=c(min(room_service, na.rm=TRUE), cuts, max(room_service, na.rm=TRUE)), 
                              include.lowest=TRUE)) %>% 
  #plot with binned num var
  ggplot(aes(room_service_cut)) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  #scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() 


# Pull R-created breaks from ggplot object
#create ggplot object
df_train_nd_nvI %>%
  ggplot(aes(room_service)) +
  scale_x_binned(n.breaks=3, nice.breaks=FALSE) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() -> p

#pull information
cuts_r <- layer_scales(p)$x$breaks

#pull information to create cuts in data
df_train_nd_nvI %>%
  #cutting numerical variable by min,  R-selected values,  and max,  and including lowest value
  mutate(room_service_cut=cut(room_service, 
                              breaks=c(min(room_service, na.rm=TRUE), cuts_r, max(room_service, na.rm=TRUE)), 
                              include.lowest=TRUE)) %>% 
  #plot with binned num var
  ggplot(aes(room_service_cut)) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  #scale_y_log10(expand=expansion(mult=c(0, 0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() 


## Joining data frames if they exist
# Create toy list elements
nam <- c("cat", "cat", "dog", "dog", "bear", "cat", "bear")
df1 <- tibble(nam)
df2 <- tibble(nam, x=c(1, 3, 4, 3, 2, 6, 3))
df4 <- tibble(nam, y=c(11, 12, 15, 23, 22, 20, 80))

# Create empty list and populate
df_list <- vector("list", length=4)
df_list[[1]] <- df1
df_list[[2]] <- df2
df_list[[4]] <- df4

# Remove null elements
df_list %>% discard(is.null) -> df_list

# Join non-null elements
df_list %>% reduce(left_join, by="nam")

df1 %>%
  left_join(df2, by="nam") %>%
  left_join(df4, by="nam")



### Ordinal Encoding (home_planet,  deck,  side,  destination,  ticket)
## Exploratory Plots
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


## Find levels of factors and change them
fac <- c("ticket", "home_planet")
lev <- c("08", "04", "02", "01", "07", "03", "06", "05")


## Mutate into ordinal factor if column is in character vector
df_train_nd_nvI %>%
  mutate(across(.cols=fac, ~as.ordered(.x))) %>% 
  #mutate(ticket=fct_relevel(ticket, lev)) %>%
  {if("ticket" %in% fac) mutate(., ticket_ord=fct_relevel(ticket, lev)) else .} %>%
  head() 


### Rare Label Encoding       
## Exploratory Plots
# Barplots of counts
#deck-raw
df_train_nd_nvI %>%
  mutate(deck=fct_infreq(deck)) %>%
  barplotter(c("deck", "transported"))

#deck-combine A & T
df_train_nd_nvI %>%
  mutate(deck=fct_collapse(deck, other=c("A", "T")), 
         deck=fct_infreq(deck)) %>%
  barplotter(c("deck", "transported"))

#deck-combine D,  A,  & T
df_train_nd_nvI %>%
  mutate(deck=fct_collapse(deck, other=c("A", "D", "T")), 
         deck=fct_infreq(deck)) %>%
  barplotter(c("deck", "transported"))


# Table of counts and percentages
#deck-raw
df_train_nd_nvI %>%
  tabylize(c("deck", "transported")) %>%
  setNames(c("deck", "stayed", "transported")) %>%
  rowwise() %>%
  mutate(total=sum(stayed, transported), 
    across(c(stayed, transported), ~(.x/total)*100, .names="{.col} (%)")) %>%
  rename_with(.cols=c(stayed, transported), .fn=~paste(.x, "(n)")) %>%
  arrange(desc(total))
#make this a separate function or a modification to tabylize
  

#### Feature Creation==================================================================================================
### Group Size
#options: 
#1) ticket group size (same passenger group); 
#2) family size (passenger group + last name); 
#3) travel party size (cabin)

## Calculate feature
df_train_nd_nvI %>%
  group_by(!!sym("passenger_group")) %>%
  mutate(ticket_group_size=n(), 
    ticket_group_size=as.factor(ticket_group_size)) -> ticket_grp_sum


## Plots
# Group size only
ticket_grp_sum %>%
  barplotter("ticket_group_size")

# Group size + transported status
ticket_grp_sum %>%
  barplotter(c("ticket_group_size", "transported"))

  

### Luxury Expenses
## Exploratory plots
# Heat map
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

# Scatterplot of one pair
df_train_nd_nvI %>%
  ggplot(aes(room_service, food_court)) +
  geom_point() +
  facet_wrap(~transported) +
  geom_smooth(method="lm") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# Boxplots
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


### Name
i_df_train_nd_nvI %>%
  separate(name, into=c("f_name", "l_name", sep=" "), remove=FALSE) -> fi_df_train_nd_nvI


### Cabin



### Family size
## l_name
df_train_nd_nvI %>%
  group_by(l_name) %>%
  mutate(fam_size=n()) -> train

# l_name + passenger_group
train %>%
  group_by(l_name, passenger_group) %>%
  mutate(fam_size=n()) -> train

# l_name + cabin
train %>%
  group_by(l_name, cabin) %>%
  mutate(fam_size=n()) -> train

# l_name + passenger_group + cabin
train %>%
  group_by(l_name, passenger_group, cabin) %>%
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









