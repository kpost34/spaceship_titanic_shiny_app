#Created by Keith Post on 8/5/22

#Functions for spaceship titanic R shiny project: part 3 of x
#Code for functions that match backbone 03 script: feature engineering

#Load packages
pacman::p_load(tidyverse,cowplot)

#### Data transformation and feature extraction=========================================================================
### Data transformation
## Function to build cowplot of density and qqplots for various transforms
# Individual ggplot functions
dens_plotter<-function(dat,var,label=""){
  dat %>%
    ggplot(aes(x={{var}})) +
    ggtitle(label) +
    geom_density() +
    theme_bw()
}

qq_plotter<-function(dat,var){
  dat %>%
    ggplot(aes(sample={{var}})) +
    ggtitle("") +
    geom_qq(color="darkblue") +
    geom_qq_line() +
    theme_bw()
}

# Mathematical functions
min_max_scaler<-function(x){
  (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
}

standardizer<-function(x){
  (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
}


# Plotting the full grid
cowplotter<-function(dat,var,label_vec=""){
  #create new cols and subset data
  dat %>%
    mutate(log=log({{var}}),
          mm=min_max_scaler({{var}}),
          stdize=standardizer({{var}}),.keep="used") -> dat_new
  
  #create chr vec of col names
  cols<-names(dat_new)
  
  #set up for loop
  for(i in 0:3){
    #use !!sym() to unquote and evaluate col names and output to objects
    dens_plotter(dat_new,!!sym(cols[i+1]),label_vec[i+1]) -> plot_x
    qq_plotter(dat_new,!!sym(cols[i+1])) -> plot_y
    
    #assign temp objects names with "p" prefix
    assign(paste0("p",2*i+1),plot_x)
    assign(paste0("p",2*i+2),plot_y)
  }
  #use plot_grid to display all 8 plots
  plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,nrow=4)
}
      
   
  
  
  



