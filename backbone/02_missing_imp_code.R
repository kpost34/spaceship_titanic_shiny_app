#Created by Keith Post on 8/6/22

# R code to build Shiny app for developing and testing machine learning algorithm on Spaceship Titanic data
# Part 2 of x:  exploring missing data, discretizing num, imputing names, and imputing non-character variables

# Load Packages and Address Conflicts===============================================================
## Load Packages
pacman::p_load(here, tidyverse, janitor, visdat, finalfit, skimr, GGally, rstatix, naniar, mice)

## Address conflicts
filter <- dplyr::filter
chisq.test <- stats::chisq.test
summarize <- dplyr::summarize



# Read in and Initially Clean Data==================================================================
#exit: df_train
read_csv(here("data", "train.csv")) %>%
  clean_names() %>%
  #passenger_id
  separate(passenger_id,into=c("passenger_group", "ticket"), sep="_", remove=FALSE) %>%
  #cabin
  separate(cabin,into=c("deck", "num", "side"), sep="/", remove=FALSE) %>%
  #name
  separate(name,into=c("f_name", "l_name"), sep=" ", remove=FALSE) %>% 
  #reclassify vars
  mutate(across(c(ticket, home_planet, deck, side, destination), ~as.factor(.x))) -> df_train


# num Discretization================================================================================
#enter: df_train
#exit: df_train_nd

#context: num is the room number of the cabin (which also contains the deck and side); num ranges
  #from 1-1800+, but these are strings. They could easily be alphabetical or alphanumerical codes. 
  #strings with 1800+ categories have little value in prediction, but they can be discretized into
  #a reasonable number of groups/bins, making it a factor variable. Let's do this coarsely by
  #splitting num into groups by counts of 100, 200, 300, or 400. But, instead of simply dividing
  #by these values, let's consider the hundreds digit a floor, and group by an individual floor
  #up to 4 floors to make the values more interpretable

## Cut num 
### Grabbing floor then binning by 1-4 floors
df_train %>%
  mutate(num=as.numeric(num),
         floor_1=num %/% 100,
         floor_2=cut_width(floor_1, width=2, boundary=0, closed="left"),
         floor_3=cut_width(floor_1, width=3, boundary=0, closed="left"),
         floor_4=cut_width(floor_1, width=4, boundary=0, closed="left"),
         floor_1=as.factor(floor_1)) %>%
  select(num, starts_with("floor"), transported) -> df_train_floor


## Assess bins according to transported status
### Tabular outputs
### Summary outputs
df_train_floor %>%
  skim()

df_train_floor %>%
  summary()

df_train_floor %>%
  glimpse()


### With transported status
#### floor_1
tabyl(df_train_floor, floor_1, transported) %>%
  clean_names() %>%
  mutate(prop_transported={true/(false + true)} %>% signif(3),
         prop_gt_0.5=prop_transported > 0.5) %>%
  reframe(rng=range(prop_transported))


#### floor_2
tabyl(df_train_floor, floor_2, transported) %>%
  clean_names() %>%
  mutate(prop_transported={true/(false + true)} %>% signif(3),
         prop_gt_0.5=prop_transported > 0.5) %>%
  reframe(rng=range(prop_transported))


#### floor_3
tabyl(df_train_floor, floor_3, transported) %>%
  clean_names() %>%
  mutate(prop_transported={true/(false + true)} %>% signif(3),
         prop_gt_0.5=prop_transported > 0.5) %>%
  reframe(rng=range(prop_transported))


#### floor_4
tabyl(df_train_floor, floor_4, transported) %>%
  clean_names() %>%
  mutate(prop_transported={true/(false + true)} %>% signif(3),
         prop_gt_0.5=prop_transported > 0.5) %>%
  reframe(rng=range(prop_transported))

### Graphical outputs
#### Develop function
plot_floors <- function(dat, var, label) {
  dat %>% 
    ggplot() +
    geom_bar(aes(x={{var}}, fill=transported), position="dodge") +
    scale_x_discrete(limits=rev) +
    scale_y_continuous(expand=expansion(mult=c(0, .02))) +
    scale_fill_viridis_d(end=0.8) +
    labs(x=label) +
    coord_flip() +
    theme_bw(base_size=14) +
    theme(legend.position="bottom")
}

#### Hard-coded floor_1
df_train_floor %>% 
  ggplot() +
  geom_bar(aes(x=floor_1, fill=transported), position="dodge") +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(expand=expansion(mult=c(0, .02))) +
  scale_fill_viridis_d(end=0.8) +
  labs(x="floor") +
  coord_flip() +
  theme_bw(base_size=14) +
  theme(legend.position="bottom")

#### Functionalized
plot_floors(df_train_floor, floor_1, "floor")
plot_floors(df_train_floor, floor_2, "floor range")
plot_floors(df_train_floor, floor_3, "floor range")
plot_floors(df_train_floor, floor_4, "floor range")


## Select a discretization
#floor_1 is more granular and has the largest deviation in prop_transported, so will use it going
  #forward
df_train %>%
  mutate(num_num=as.numeric(num),
         floor=num_num %/% 100,
         floor=as.factor(floor)) %>%
  select(-num_num) -> df_train_nd


# Character String Imputation=======================================================================
#enter: df_train_nd (training DF with num discretized)
#exit: df_train_nd_nI

## Assessment of Missingness----------------------------------------
### Non-missing vs missing names
#### Tabular
##### Sample
#missing
df_train_nd %>%
  filter(is.na(name)) %>%
  slice_sample(n=2)

#non-missing
df_train_nd %>%
  filter(!is.na(name)) %>%
  slice_sample(n=2)


##### Summary table
df_train_nd %>%
  pivot_longer(cols=contains("name"), names_to="name_type", values_to="name") %>%
  group_by(name_type) %>%
  summarize(across(name, list(present=~sum(!is.na(.x)), missing=~sum(is.na(.x)), total=length)))


#### Graphical
df_train_nd %>%
  summarize(across(contains("name"), ~ifelse(!is.na(.x), "Present","Missing"))) %>%
  pivot_longer(cols=everything(), names_to="name_type", values_to="name") %>%
  ggplot() +
  geom_bar(aes(x=name_type, fill=name), color="black") +
  scale_x_discrete(labels=c("first name", "last name", "full name")) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
  scale_fill_viridis_d(end=0.5) +
  labs(x="") +
  theme_bw() +
  theme(legend.title=element_blank())


### Name missingness + passenger group
#### Pull passenger groups containing one NA name
df_train_nd %>%
  #filter for NA l_name
  filter(is.na(l_name)) %>%
  #extract passenger group
  pull(passenger_group) -> pass_group_NA_name


#### Calculate numbers of each size of passenger_group and the numbers of named passengers
df_train_nd %>%
  #select only passenger_groups containing at least one NA l_name
  filter(passenger_group %in% pass_group_NA_name) %>% 
  group_by(passenger_group) %>%
  #calculate number of non-NA names out of number of inds in pass group
  summarize(num_name=sum(!is.na(l_name)), 
            pass_group_size=length(l_name)) %>%
  #group by calculated metrics
  group_by(num_name, pass_group_size) %>%
  #returns counts and lists of passenger groups
  summarize(n=n(), 
            pass_groups=list(passenger_group)) %>%
  ungroup() -> passGroupNAnameSizes_tab
#NA l_names--number of groups with at least one non-NA l_name and number of groups without any others
#or by number of non-NA l_names (0,  1,  2,  etc.)


#### Plot results
passGroupNAnameSizes_tab %>%
  ggplot(aes(x=num_name, y=n)) +
  geom_col(fill="darkgreen") +
  labs(x="Number of named passengers", y="Number of passenger groups") +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
  theme_bw() 


### Name missingness + same room
#### Note: two passenger ids where cabin and last name NA (so only 198 NA names where there is a cabin)
df_train_nd %>% filter(is.na(cabin) & is.na(l_name))

#### Pull non-NA cabins containing at least one passenger with NA name
df_train_nd %>%
  filter(!is.na(cabin) & is.na(l_name)) %>%
  pull(cabin) -> room_group_NA_name

#### Calculate numbers of each room size (i.e.,  # of inds) and the numbers of named passengers
df_train_nd %>%
  filter(cabin %in% room_group_NA_name) %>%
  group_by(cabin) %>%
  summarize(num_name=sum(!is.na(l_name)), 
            room_group_size=length(l_name)) %>% 
  group_by(num_name, room_group_size) %>%
  summarize(n=n(), 
            room_groups=list(cabin)) %>%
  ungroup() -> roomGroupNAnameSizes_tab

#conclusions from both assessments: if num_name==0 then can't populate; the larger the group then logically the greater the 
#confidence in filling name--this could be part of the method

#### Plot results
roomGroupNAnameSizes_tab %>%
  ggplot(aes(x=num_name, y=n)) +
  geom_col(fill="tan4") +
  labs(x="Number of named passengers", y="Number of cabins") +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
  theme_bw() 


## Imputation Options----------------------------------------
### Ignore
df_train_nd #-> df_train_nd_nI


### Drop cols
df_train_nd %>% 
  select(-contains("name")) #-> df_train_nd_nI


### Remove rows with NAs for name (i.e.,  list-wise deletion)
df_train_nd %>%
  filter(!is.na(name)) #-> df_train_nd_nI


### Populate some name values using passenger group (e.g. num_name of 1,  2,  or 3)
#### Pull out vector of pass_groups (NOTE: turn this into function)
passGroupNAnameSizes_tab %>%
  filter(between(num_name, 1, 7)) %>% #trying 1-7
  pull(pass_groups) %>% 
  unlist() %>%
  as.integer() -> passGroupNAnames1_7
  # as.integer() -> passGroupNAnames1_3


#### Filter by passenger groups then fill by named passengers in pass groups (also a function)
df_train_nd %>%
  #filter by passenger_group
  filter(passenger_group %in% passGroupNAnames1_7) %>%
  # filter(passenger_group %in% passGroupNAnames1_3) %>%
  #populate l_name by passenger_group
  group_by(passenger_group) %>%
  fill(l_name) %>%
  ungroup() %>% 
  #re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name, 
    is.na(f_name) & !is.na(l_name) ~ paste(f_name, l_name), 
    is.na(f_name) & is.na(l_name)  ~ "", 
    TRUE                           ~ "CHECK")) %>% 
  #recombine with the filtered out rows based on passenger_group
  bind_rows(df_train_nd %>%
              filter(!passenger_group %in% passGroupNAnames1_3)) -> df_train_nd_nI #df_train_nameImputed


### Populate some name values using cabin components (e.g.,  num_name of at least 2)
#### Pull out vector of room groups
roomGroupNAnameSizes_tab %>%
  filter(num_name >=2) %>% 
  pull(room_groups) %>%
  unlist() -> roomGroupNAnames2larger


#### Filter by passenger groups then fill by named passengers in pass groups (also a function)
df_train_nd %>%
  #filter by cabin
  filter(cabin %in% roomGroupNAnames2larger) %>% 
  #populate l_name by passenger_group
  group_by(cabin) %>%
  fill(l_name) %>%
  ungroup() %>% 
  #re-populate name field
  mutate(name=case_when(
    !is.na(name)                   ~ name, 
    is.na(f_name) & !is.na(l_name) ~ paste(f_name, l_name), 
    is.na(f_name) & is.na(l_name)  ~ "", 
    TRUE                           ~ "CHECK")) %>% 
  #recombine with the filtered out rows based on passenger_group
  bind_rows(df_train_nd %>%
              filter(!cabin %in% roomGroupNAnames2larger)) #-> df_train_nI #df_train_nameImputed


#### Handling NAs after filling names
##### Remove NA names after filling
df_train_nd_nI %>%
  filter(!is.na(name)) -> df_train_nd_nI

##### Don't remove NA names after filling
# df_train_nI


# Data Imputation (numerical and categorical)=======================================================
#enters df_train_nd_nI
#exits df_train_nd_nvI

## Missingness----------------------------------------
### Code variables
#all non-chr explanatory variables
explanatory <- df_train_nd_nI %>%
  select(-c(passenger_id, passenger_group, cabin, name:last_col())) %>%
  names()
#dependent variable
dependent<-"transported"


### Identify missing values in each variable
#### Tabular visualization
##### Detailed missingingness data and summary stats
df_train_nd_nI %>%
  ff_glimpse(dependent, explanatory)

##### Number missing,  complete cases,  and summary stats
df_train_nd_nI %>%
  skim()

##### Row number,  number of missing values,  and pct missing
df_train_nd_nI %>%
  miss_case_summary()

##### Number and pct of cases with 0-n missing values
df_train_nd_nI %>%
  miss_case_table()

#### Graphical visualization
##### Occurrences
vis_dat(df_train_nd_nI)
vis_miss(df_train_nd_nI)

df_train_nd_nI %>%
  missing_plot(dependent, explanatory) 
#cols with missing data: home_planet,  cryo_sleep,  deck,  num,  side,  destination,  age,  vip,  room_service,  food_court,  
#shopping_mall,  spa,  vr_deck (aside from character vars...all but transported)

##### Per variable or row
df_train_nd_nI %>%
  #remove chr vars
  select(all_of(c(explanatory, dependent))) %>%
  gg_miss_var()

df_train_nd_nI %>%
  #remove chr vars
  select(all_of(c(explanatory, dependent))) %>%
  gg_miss_case()
  

##### Look for patterns of missingness
df_train_nd_nI %>%
  missing_pattern(dependent, explanatory) 
#there are 77 patterns of missingness

df_train_nd_nI %>%
  #remove chr vars
  select(all_of(c(explanatory, dependent))) %>%
  #nset specifies the number of most common patterns to return
  gg_miss_upset(nset=3)


### Statistically test for MCAR
df_train_nd_nI %>%
  #select non-chr predictors
  select(all_of(explanatory)) %>% 
  naniar::mcar_test()
#p > 0.97 indicating that it's NS and thus the data are MCAR


## Handling missing data----------------------------------------
### Listwise deletion (retain complete cases only)
na.omit(df_train_nd_nI) #-> df_train_nd_nvI
#drops rows from 8559 to 6606


### Mean imputation (numeric vars) & most frequent categories
df_train_nd_nI %>% 
  mutate(across(!where(is.character), ~Hmisc::impute(.x, fun=mean))) 



### Median imputation (numeric vars) & most frequent categories
df_train_nd_nI %>% 
  mutate(across(!where(is.character), ~Hmisc::impute(.x, fun=median))) 


### Multiple imputation - PMM
df_train_nd_nI %>% 
  mice(method="pmm", m=2, maxit=2) %>%
  complete() -> df_train_nd_nvI
#takes ~7 seconds


#### Cart
# df_train_nd_nI %>% 
#   mice(method="cart", m=2, maxit=2) %>%
#   complete() %>%
#   skim()
#takes a while--so use PMM


## Post-imputation check----------------------------------------
### Remaining missingness?
skim(df_train_nd_nvI) #only char vars have missing values


### Visualize missingness
missing_plot(df_train_nd_nvI)
gg_miss_var(df_train_nd_nvI)
gg_miss_case(df_train_nd_nvI)


### Compare pre- vs post-imputation
vis_compare(df_train_nd_nvI, df_train_nd_nI) 



# Data Hygiene======================================================================================
## Remove all but final data frame
rm(list=setdiff(ls(), "df_train_nd_nvI"))








