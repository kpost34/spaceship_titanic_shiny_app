#Created by Keith Post on 8/5/22

#Functions for spaceship titanic R shiny project: part 3 of x
#Code for functions that match backbone 03 script: feature engineering

#Load packages
pacman::p_load(tidyverse,cowplot)


# Data transformation and feature extraction========================================================
## Feature Scaling--------------------
### Function to build cowplot of density and qqplots for various transforms
#### Individual ggplot functions
dens_plotter<-function(dat, var, label=""){
  dat %>%
    ggplot(aes(x={{var}})) +
    ggtitle(label) +
    geom_density() +
    theme_bw(base_size=16)
}

qq_plotter<-function(dat,var){
  dat %>%
    ggplot(aes(sample={{var}})) +
    ggtitle("") +
    geom_qq(color="darkblue") +
    geom_qq_line() +
    theme_bw(base_size=16)
}


#### Mathematical functions
min_max_scaler <- function(x){
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

standardizer <- function(x){
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
}


#### Plotting the full grid
cowplotter<-function(dat, var, label_vec=c("raw", "log-transformed", "min-max scaled", "standardized")){
  #convert var
  var<-sym(var)
  
  #create new cols and subset data
  dat %>%
    mutate(log=log(!!var),
          mm=min_max_scaler(!!var),
          stdize=standardizer(!!var), .keep="used") -> dat_new
  
  #create chr vec of col names
  cols<-names(dat_new)
  
  #create empty list
  list_plot<-vector(mode="list",length=8)
  
  #set up for loop
  for(i in 0:3){
    #use !!sym() to unquote and evaluate col names and output to objects
    dens_plotter(dat_new, !!sym(cols[i+1]), label_vec[i+1]) -> plot_x
    qq_plotter(dat_new, !!sym(cols[i+1])) -> plot_y
    
    #store plots into list_plot
    list_plot[2*i+1] <- list(plot_x)
    list_plot[2*i+2] <- list(plot_y)
  }
  #plot list of plots
  plot_grid(plotlist=list_plot,
            labels=rep(c("density plot", "qq plot"), 4), 
            label_x=rep(c(.75, .85), 4),
            label_y=.95,
            nrow=4)
}


#### Display text after confirmation hit
confirm_scaling_msg <- function(action) {
  if(action=="raw"){
    return("Scaling was not applied to any numerical variable.")
  } else if(action=="log"){
    return("log x + 1 scaling applied to all numerical variables.")
  } else if(action=="mm_scale"){
    return("Min-max scaling applied to all numerical variables.")
  } else if(action=="standize"){
    return("Standardization applied to all numerical variables.")
  }
}



## Discretization--------------------
### Function to make histograms of a numerical var filled by transported with options to adjust bins & use log x scale
histogrammer2<-function(dat, col, n.bins=30, x.log.scale=TRUE){
  dat %>%
    #convert any categorical vars to numeric
    mutate(var=as.numeric(!!sym(col))) -> dat

  dat %>%
    ggplot(aes(var)) +
    #create histogram filled by transported status; specify bin number
    geom_histogram(aes(fill=transported), bins=n.bins, color="black") +
    #apply psuedo log (b/c of 0s) if log10 scale button selected
    {if(x.log.scale) scale_x_continuous(trans=scales::pseudo_log_trans(base=10),
                                        expand=expansion(mult=c(0.05, .05)),
                                        guide=guide_axis(check.overlap=TRUE))
      else scale_x_continuous(expand=expansion(mult=c(0.05, 0.05)))} +
    scale_fill_viridis_d() +
    xlab(col) +
    ggtitle(paste("Histogram of", col)) +
    theme_bw(base_size=18) -> p
  
  return(p)
}


### Functions to bin numerical var & retain transported
#### Create user-defined bins
user_cutter <- function(dat, col, break.vals=NA){
  dat %>%
    #ensure that col is numeric
    mutate(var= as.numeric(!!sym(col)),
      #cut variable using breaks...use := to retain naming
      !!paste0(col,"_dis") := cut(var,breaks=c(min(var,na.rm=TRUE), break.vals, max(var,na.rm=TRUE)),
              include.lowest=TRUE)) %>%
    #retain id, new col, and y
    select(passenger_id,!!paste0(col,"_dis"), transported) #updated to retain RV
}


#### Create bins with equal intervals
equal_cutter <- function(dat, col, n.breaks=NA){
  dat %>%
    #ensure that col is numeric
    mutate(var= as.numeric(!!sym(col)),
      #cut variable using breaks...use := to retain naming
      !!paste0(col,"_dis") := cut_interval(var, n.breaks)) %>%
    #retain id, new col, and y
    select(passenger_id, !!paste0(col,"_dis"), transported) #updated to retain RV
}


### Function to make bar plot after creating bins
bin_plotter <- function(dat, col, type, y.log.scale=TRUE) {
  
  title_val <- if(type=="cut_int") {
    "bins with equal ranges"
  
  } else if(type=="user") {
    "bins defined by user"
  }

  #plotting code 
  dat %>%
    ggplot(aes(!!sym(paste0(col, "_dis")))) +
    geom_bar(aes(fill=transported), 
                 position="dodge") +
    {if(y.log.scale) scale_y_continuous(trans=scales::pseudo_log_trans(base=10),
                                        expand=expansion(mult=c(0, .05)),
                                        guide=guide_axis(check.overlap=TRUE))
      else scale_y_continuous(expand=expansion(mult=c(0, 0.05)))} +
    scale_fill_viridis_d(option="mako", end=0.8) +
    xlab(col) +
    ggtitle(paste(col, title_val)) +
    theme_bw(base_size=18) -> p1

  return(p1)
}


### Function to display text after confirmation hit
confirm_discretization_msg <- function(dat) {
  n_dis <- dat %>%
    names() %>%
    str_detect("_dis$") %>%
    sum()
  
  if(n_dis==0) {
    return("No variables discretized.")
    
  } else if(n_dis==1) {
    return("Discretization applied to one variable.")
    
  } else if(n_dis>1) {
    return("Discretization applied to more than one variable.")
  }
}



## Ordinal Encoding--------------------
### Function to display text after confirmation hit
confirm_ord_encoding_msg <- function(dat) {
  var_ord <- dat %>%
    names() %>%
    str_subset("_ord$") %>%
    str_remove("_ord$")
  
  if(length(var_ord)==0) {
    return("Ordinal encoding was not applied to any variable.")
    
  } else if(length(var_ord)==1) {
    return(paste0("Ordinal encoding applied to ", var_ord, "."))
    
  } else if(length(var_ord)>=2) {
    return("Ordinal encoding applied to more than one variable.")
  }
}



## Rare Label Encoding--------------------
### Function to make barplots of counts filled by transported and to combine different factor levels
barplotter2 <- function(dat, var, cats, col="viridis", title=TRUE){
  #convert quoted input to symbol
  var <- sym(var)
  
  title_suffix <- if(!missing(cats)) {
    "with rare categories combined"
  } else{""}
  
  if(!missing(cats)) { 
    dat %>%
      #combine categories into a single 'other' category (if it has values)
      mutate(var1=fct_collapse(!!var, other=cats),
        #order by frequency
        var1=fct_infreq(var1)) -> dat1
  }
  else if(missing(cats)) {
    dat %>%
      #order by frequency
      mutate(var1=fct_infreq(!!var)) -> dat1
  }
  
  dat1 %>%
    ggplot() +
      geom_bar(aes(x=var1, fill=transported), color="black", position="dodge") +
      scale_y_log10(expand=expansion(mult=c(0,0.1))) +
      scale_fill_viridis_d(option=col) +
      xlab(paste(var)) +
      {if(title) ggtitle(paste("Bar plot of", var, title_suffix))} +
      theme_bw(base_size=15) +
      theme(legend.position="bottom")
}


### Function to display text after confirmation hit
confirm_rare_encoding_msg <- function(dat) {
  var_rare <- dat %>%
    names() %>%
    str_subset("_rare$") %>%
    str_remove("_rare$")
  
  if(length(var_rare)==0) {
    return("Rare label encoding was not applied to any variables.")
    
  } else if(length(var_rare)==1) {
    return(paste0("Rare label encoding applied to ", var_rare, "."))
    
  } else if(length(var_rare)==2) {
    return(paste0("Rare label encoding applied to ",
                 paste(var_rare, collapse=" and "),
                 "."))
  }
}



# Feature Creation==================================================================================
## Function to mutate input variables to create luxury expense variable
lux_builder <- function(dat, vars){
  
  if(length(vars) <= 1) {
    
    dat1 <- tibble()
    
  } else {
    
    vars <- syms(vars)
  
    dat %>%
      rowwise() %>%
      mutate(!!paste(paste(vars, collapse="__"), "lux", sep="_") := sum(!!!vars)) %>%
      ungroup() -> dat1
  }
  
  return(dat1)
  
}


## Function to make heat map
heatmapper<-function(dat,vars){
  
dat %>%
    select(all_of(vars)) %>%
    ggcorr(geom="tile", label=TRUE, label_size=6, digits=3, high="#3B9AB2", low="#F21A00",
           size=5) +
    ggtitle("Correlation matrix of expense items") +
    theme(axis.text=element_text(size=14),
          legend.position="bottom",
          legend.key.size=unit(1.1, "cm"),
          legend.text=element_text(size=14),
          plot.title=element_text(size=16, face="bold"))
}


## Function to create boxplot with transported (x) and luxury (y; summed numeric vars)
boxplotter2<-function(dat){
  
  var <- dat %>%
    names() %>%
    .[str_detect(., "_lux$")]
  
  dat %>%
    ggplot() +
    geom_boxplot(aes(x=transported,y=!!sym(var),color=transported)) +
    scale_y_log10() +
    scale_color_viridis_d(option="mako", end=0.8, guide=NULL) +
    ylab("$") +
    ggtitle("Boxplot of summed luxury expense items") +
    theme_bw(base_size=15) +
    theme(plot.title=element_text(size=16, face="bold"))
}
  


# Feature Selection=================================================================================
## Function to extract predictors into vec with classes as names,
extract_pred_class <- function(dat){
  dat %>% 
    #remove chr vars and dep var
    select(!c(where(is.character), transported)) %>%
    #extract classes into a list
    purrr::map(class) %>%
    #combine classes into a single element if multiple (for ordered factors)
    purrr::map(paste, collapse=" ") %>%
    #convert to vector
    unlist() %>%
    set_names(names(.), .) -> tmp
  
  #grab names
  nm <- names(tmp) %>%
    str_replace("ordered", "ord")
  
  #append new names to vector of variables
  tmp %>%
    set_names(paste0(., " (", nm, ")")) -> varnames

  return(varnames)
}
  

## Function to ascertain class of selected variable
comp_var_class <- function(var_sel, vars_vec, kind) {
  #extract class of selected variable
  type <- vars_vec[vars_vec == var_sel] %>%
    #pull [variable] (class)
    names() %>%
    #extract class
    str_extract("(?<=\\().+(?=\\))")
  
  #compare to the kind arg
  type %in% kind
}


## Function to identify removed variables
id_dropped_vars <- function(dat, sel_vec) {
  nm <- names(dat) 
  
  if(is.null(sel_vec)|length(sel_vec)==0) {
    return(NULL)
    
  } else if(!is.null(sel_vec)) {
    sel_roots <- str_remove_all(sel_vec, "_scale$|_dis$|_ord$|_rare$")
    sel_roots_patt <- paste(sel_roots, collapse="|")
    
    matching_vars <- nm[str_detect(nm, sel_roots_patt)]
    dropped_vars <- matching_vars[!matching_vars %in% sel_vec]
    
    return(dropped_vars)
  }
}


## Function to reduce variable pool
# deplete_var_pool <- function(var_sel, var_pool) {
#   #create obj suffixes for regex
#   suffixes <- c("_scale$", "_dis$", "_ord$", "_rare$") %>%
#     paste(., collapse="|")
# 
#   #if at least one variable selected
#   if( {nchar(var_sel) %>% sum} > 0 ) {
#     #create obj var_drop which represents variables that will be removed
#     var_sel %>%
#       purrr::map_chr(function(var) {
#         #for each variable...
#         var %>%
#           { #if it's a luxury item then
#             if(str_detect(var, "_lux$"))
#               var %>%
#                 str_remove("_lux$") %>% #remove suffix
#                 str_replace_all("__", "|") #turn into regex
#             else var %>% #if not...
#               str_remove(suffixes) #remove suffix to retain root
#           }
#       }) %>% # b/c > 1 vars can be selected...paste together with '|'
#       paste(., collapse="|") -> var_drop
#   #otherwise, populate var_drop with NA
#   } else {var_drop <- NA}
# 
#   #if var_drop is NA...
#   var_remain <- if(is.na(var_drop)) {
#     var_pool #then var_remain is set to var_pool
#   #otherwise, extract 'novel 'variables remaining
#   } else {
#     var_pool[!str_detect(var_pool, var_drop)]
#   }
# 
# 
# 
#   return(var_remain)
# }



# deplete_var_pool <- function(var_sel, var_pool) {
#   #create obj suffixes for regex
#   suffixes <- c("_scale$", "_dis$", "_ord$", "_rare$") %>%
#     paste(., collapse="|")
#   
#   #create obj var_drop which represents variables that will be removed
#   var_drop <- if(str_detect(var_sel, "_lux$")) {
#     var_sel %>%
#       str_remove("_lux") %>%
#       str_replace_all("__", "|")
#   } else if(!str_detect(var_sel, "_lux$")) {
#     var_sel %>%
#       str_remove(suffixes)
#   }
# 
#   #extract 'novel'variables remaining
#   var_remain <- var_pool[!str_detect(var_pool, var_drop)]
# 
#   return(var_remain)
# }
























  



