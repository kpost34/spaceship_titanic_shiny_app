#Created by Keith Post on 8/5/22

#Functions for spaceship titanic R shiny project: part 3 of x
#Code for functions that match backbone 03 script: feature engineering

#Load packages
pacman::p_load(tidyverse,cowplot)

#### Data transformation and feature extraction=========================================================================
### Feature Scaling
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
cowplotter<-function(dat,var,label_vec=c("raw","log-transformed","min-max scaled","standardized")){
  #convert var
  var<-sym(var)
  
  #create new cols and subset data
  dat %>%
    mutate(log=log(!!var),
          mm=min_max_scaler(!!var),
          stdize=standardizer(!!var),.keep="used") -> dat_new
  
  #create chr vec of col names
  cols<-names(dat_new)
  
  #create empty list
  list_plot<-vector(mode="list",length=8)
  
  #set up for loop
  for(i in 0:3){
    #use !!sym() to unquote and evaluate col names and output to objects
    dens_plotter(dat_new,!!sym(cols[i+1]),label_vec[i+1]) -> plot_x
    qq_plotter(dat_new,!!sym(cols[i+1])) -> plot_y
    
    #store plots into list_plot
    list_plot[2*i+1]<-list(plot_x)
    list_plot[2*i+2]<-list(plot_y)
  }
  #plot list of plots
  plot_grid(plotlist=list_plot,nrow=4)
}

### Discretization
## Function to make histograms of a numerical var filled by transported with options to adjust bins & use log x scale
histogrammer2<-function(dat,col,n.bins=30,x.log.scale=TRUE){
  dat %>%
    #convert any categorical vars to numeric
    mutate(var=as.numeric(!!sym(col))) -> dat
  
  if(x.log.scale==TRUE){
    dat %>%
      #convert to log scale
      mutate(var=if_else(var==0,.001,var,NA_real_)) -> dat
  }

  dat %>%
    ggplot(aes(var)) +
    #create histogram filled by transported status; specify bin number
    geom_histogram(aes(fill=transported),bins=n.bins,color="black") +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    scale_fill_viridis_d() +
    xlab(col) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13)) -> p
  
  if(x.log.scale==TRUE){
    p + 
      #use log10 scale for x-axis if arg=TRUE
      scale_x_log10() +
      theme(plot.caption=element_text(hjust=0)) +
      labs(caption="0s converted to 0.001 for log10 scale") -> p
  }
  p
}

## Functions to bin and plot numerical var filled by transported 
# R decides bins
bin_plotter<-function(dat,col,num.breaks=2,y.log.scale=TRUE){
  dat %>%
    #convert any categorical vars to numeric
    mutate(var=as.numeric(!!sym(col))) %>%
    ggplot(aes(var)) +
    geom_bar(aes(fill=transported)) +         
    scale_x_binned(n.breaks=num.breaks,nice.breaks=FALSE) +
    scale_fill_viridis_d() +
    xlab(col) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13)) -> p1
  
  if(y.log.scale==FALSE){
    p1 + scale_y_continuous(expand=expansion(mult=c(0,0.05)))
  }

  else if(y.log.scale==TRUE){
    p1 + scale_y_log10(expand=expansion(mult=c(0,0.05)))
  }
}


# Create user-defined bins
cutter<-function(dat,col,break.vals=NA){
  dat %>%
    #ensure that col is numeric
    mutate(var= as.numeric(!!sym(col)),
      #cut variable using breaks...use := to retain naming
      !!paste0(col,"_dis") := cut(var,breaks=c(min(var,na.rm=TRUE),break.vals,max(var,na.rm=TRUE)),
              include.lowest=TRUE)) %>%
    #retain id, new col, and y
    select(passenger_id,!!paste0(col,"_dis"))
}

# Make bar plot after creating user-defined bins
# user_bin_plotter<-function(dat,col,y.log.scale=TRUE){
#   dat %>%
#     ggplot(aes({{col}})) +
#       geom_bar(aes(fill=transported)) +
#       scale_fill_viridis_d() +
#       theme_bw() +
#       theme(axis.text=element_text(size=12),
#             axis.title=element_text(size=13)) -> p1
#     
#     if(y.log.scale==FALSE){
#       p1 + scale_y_continuous(expand=expansion(mult=c(0,0.05)))
#     }
#     
#     else if(y.log.scale==TRUE){
#       p1 + scale_y_log10(expand=expansion(mult=c(0,0.05)))
#     }
# }


# Make bar plot after creating user-defined bins
user_bin_plotter<-function(dat,col,break.vals,y.log.scale=TRUE){
  dat %>%
    #convert any categorical vars to numeric
    mutate(var=as.numeric(!!sym(col)),
    #cut variable using breaks
      var=cut(var,breaks=c(min(var,na.rm=TRUE),break.vals,max(var,na.rm=TRUE)),
              include.lowest=TRUE)) %>%
    ggplot(aes(var)) +
    geom_bar(aes(fill=transported)) +
    scale_fill_viridis_d() +
    xlab(col) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13)) -> p1

  if(y.log.scale==FALSE){
    p1 + scale_y_continuous(expand=expansion(mult=c(0,0.05)))
  }

  else if(y.log.scale==TRUE){
    p1 + scale_y_log10(expand=expansion(mult=c(0,0.05)))
  }
}


# Join together DFs into one by passenger_id
# dis_joiner<-function(dat,j1,j2,j3,j4,j5,j6,j7){
#   df_list<-vector(mode="list",length=8)
#   
#   
#   
#   
#   dat %>%
#     {if(!missing(j1))(left_join(.,j1,by="passenger_id")) else .} %>%
#     {if(!missing(j2))(left_join(.,j2,by="passenger_id")) else .} %>%
#     {if(!missing(j3))(left_join(.,j3,by="passenger_id")) else .} %>%
#     {if(!missing(j4))(left_join(.,j4,by="passenger_id")) else .} %>%
#     {if(!missing(j5))(left_join(.,j5,by="passenger_id")) else .} %>%
#     {if(!missing(j6))(left_join(.,j6,by="passenger_id")) else .} %>%
#     {if(!missing(j7))(left_join(.,j7,by="passenger_id")) else .} -> dat2
#   
#   if(ncol(dat2)==1) {
#     dat
#   }
#   else{dat2}
# }

#{if(na.rm==TRUE)(filter(.,across(everything(),~!is.na(.x)))) else .} %>%
### Categorical Encoding



### Rare Label Encoding
## Function to make barplots of counts filled by transported and to combine different factor levels
rare_enc_barplotter<-function(dat,var,cats){
  #convert quoted input to symbol
  var<-sym(var)
  
  if(!missing(cats)) { 
    dat %>%
      #combine categories into a single 'other' category (if it has values)
      mutate(var1=fct_collapse(!!var,other=cats),
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
      geom_bar(aes(x=var1,fill=transported),color="black") +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      scale_fill_viridis_d() +
      xlab(paste(var)) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=13))
}
      

#### Feature Creation==================================================================================
### Plotting functions for luxury expense variable
## Function to mutate input variables to create luxury expense variable
lux_builder<-function(dat,vars){
  vars<-syms(vars)
  
  dat %>%
    rowwise() %>%
    mutate(luxury=sum(!!!vars)) %>%
    ungroup()
}

## Function to make heat map
heatmapper<-function(dat,vars){
  
dat %>%
    select(all_of(vars)) %>%
    ggcorr(label=TRUE,digits=3)
}


## Function to create boxplot with transported (x) and luxury (y; summed numeric vars)
boxplotter2<-function(dat){
  dat %>%
    ggplot() +
    geom_boxplot(aes(x=transported,y=luxury,color=transported)) +
    scale_y_log10() +
    scale_color_viridis_d(end=0.8,guide=NULL) +
    ylab("$") +
    theme_bw()
}
  
  
  


