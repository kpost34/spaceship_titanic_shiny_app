#Created by Keith Post on 8/14/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 3 of x:  feature engineering

#load packages
pacman::p_load(conflicted,here,tidyverse,janitor,cowplot,GGally)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


#### Read in data & initially clean data================================================================================
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
  mutate(across(c(ticket,home_planet,deck:destination),~as.factor(.x))) -> trainDF


#### Data transformation and feature extraction=========================================================================
### Data transformation (feature scaling)
## Exploratory plots
p1<-trainDF %>% 
  ggplot(aes(x=age)) +
  ggtitle("raw") +
  geom_density() +
  theme_bw()

p2<-trainDF %>% 
  ggplot(aes(sample=age)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p3<-trainDF %>% 
  ggplot(aes(x=log(age))) +
  ggtitle("log-transformed") +
  geom_density() +
  theme_bw()

p4<-trainDF %>% 
  ggplot(aes(sample=log(age))) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p5<-trainDF %>% 
  mutate(age_mm=(age-min(age,na.rm=TRUE))/(max(age,na.rm=TRUE)-min(age,na.rm=TRUE))) %>%
  ggplot(aes(x=age_mm)) +
  ggtitle("min-max scaled") +
  geom_density() +
  theme_bw()

p6<-trainDF %>% 
  mutate(age_mm=(age-min(age,na.rm=TRUE))/(max(age,na.rm=TRUE)-min(age,na.rm=TRUE))) %>%
  ggplot(aes(sample=age_mm)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()

p7<-trainDF %>% 
  mutate(age_stdize=((age-mean(age,na.rm=TRUE))/sd(age,na.rm=TRUE))) %>% 
  ggplot(aes(x=age_stdize)) +
  ggtitle("standardized") +
  geom_density() +
  theme_bw()

p8<-trainDF %>% 
  mutate(age_stdize=((age-mean(age,na.rm=TRUE))/sd(age,na.rm=TRUE))) %>%
  ggplot(aes(sample=age_stdize)) +
  ggtitle("") +
  geom_qq(color="darkblue") +
  geom_qq_line() +
  theme_bw()
  
plot_list<-list(p1,p2,p3,p4,p5,p6,p7,p8)

plot_grid(plotlist=plot_list,nrow=4)


## Scale/transform data following selection



### Discretization
## Exploratory Plots
# Raw data with fill=transported; user could choose num of bins and log scale
trainDF %>%
  mutate(room_service=if_else(room_service==0,.001,room_service,NA_real_)) %>%
  ggplot(aes(room_service)) +
  geom_histogram(aes(fill=transported),bins=40,color="black") +
  #geom_histogram(bins=40,color="black") +
  scale_x_log10() +
  #scale_y_log10() +
  scale_fill_viridis_d() +
  theme_bw() 


## Binned data plots
# ggplot binning num var + filling by transported status
trainDF %>%
  #mutate(room_service=if_else(room_service==0,.001,room_service,NA_real_)) %>%
  ggplot(aes(room_service)) +
  scale_x_binned(n.breaks=3,nice.breaks=FALSE) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  #scale_y_log10(expand=expansion(mult=c(0,0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() 


# Manually binning boundaries and plotting 
#specify bin boundaries (i.e., break locations) and plot as binned plot
cuts<-c(1,10,100)


trainDF %>%
  #cutting numerical variable by min, selected value, and max, and including lowest value
  mutate(room_service_cut=cut(room_service,
                              breaks=c(min(room_service,na.rm=TRUE),cuts,max(room_service,na.rm=TRUE)),
                              include.lowest=TRUE)) %>% 
  #plot with binned num var
  ggplot(aes(room_service_cut)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  #scale_y_log10(expand=expansion(mult=c(0,0.05))) +
  geom_bar(aes(fill=transported)) +
  scale_fill_viridis_d() +
  theme_bw() 
  


### Categorical Encoding (home_planet, deck, side, destination, ticket)
## Exploratory Plots
trainDF %>%
  #use barplotting function from eda
  barplotter("home_planet")

trainDF %>%
  barplotter("deck")

trainDF %>%
  barplotter("side")

trainDF %>%
  barplotter("destination")

trainDF %>% 
  barplotter("ticket")
  
  


### Rare Label Encoding       
## Exploratory Plots
# Barplots of counts
#deck-raw
trainDF %>%
  mutate(deck=fct_infreq(deck)) %>%
  barplotter(c("deck","transported"))

#deck-combine A & T
trainDF %>%
  mutate(deck=fct_collapse(deck,other=c("A","T")),
         deck=fct_infreq(deck)) %>%
  barplotter(c("deck","transported"))

#deck-combine D, A, & T
trainDF %>%
  mutate(deck=fct_collapse(deck,other=c("A","D","T")),
         deck=fct_infreq(deck)) %>%
  barplotter(c("deck","transported"))


# Table of counts and percentages
#deck-raw
trainDF %>%
  tabylize(c("deck","transported")) %>%
  setNames(c("deck","stayed","transported")) %>%
  rowwise() %>%
  mutate(total=sum(stayed,transported),
    across(c(stayed,transported),~(.x/total)*100,.names="{.col} (%)")) %>%
  rename_with(.cols=c(stayed,transported),.fn=~paste(.x,"(n)")) %>%
  arrange(desc(total))
#make this a separate function or a modification to tabylize
  

#### Feature Creation==================================================================================================
### Group Size
#options: 
#1) ticket group size (same passenger group); 
#2) family size (passenger group + last name; 
#3) travel party size (cabin)

## Calculate feature
trainDF %>%
  group_by(passenger_group) %>%
  mutate(ticket_group_n=n(),
    ticket_group_n=as.factor(ticket_group_n)) -> ticket_grp_sum


## Plots
# Group size only
ticket_grp_sum %>%
  barplotter("ticket_group_n")

# Group size + transported status
ticket_grp_sum %>%
  barplotter(c("ticket_group_n","transported"))

  

### Luxury Expenses
## Exploratory plots
#heat map
trainDF %>%
  select(room_service:vr_deck) %>%
  ggcorr(label=TRUE,digits=3)

#scatter plots
trainDF %>%
  select(room_service:vr_deck) %>%
  ggpairs()
#note: takes a while to plot

trainDF %>%
  select(room_service:vr_deck) %>%
  pairs()
#note: takes a while to plot

trainDF %>%
  ggplot(aes(room_service,food_court)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_x_log10() +
  scale_y_log10()


## Statistics
trainDF %>%
  select(room_service:vr_deck) %>%
  filter(across(everything(),~!is.na(.x))) %>%
  cor() %>%
  signif(3)

## Calculate feature
trainDF %>%
  mutate(lux_exp=sum(room_service,food_court,na.rm=TRUE))









