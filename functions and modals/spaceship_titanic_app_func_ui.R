#Functions for UI

## Easily Create Line Breaks=====================================================
linebreaks <- function(n){HTML(strrep(br(), n))}


### Input functions========================================
selectInput01<-function(id, label, choices) {
  selectInput(inputId=id, label=label, selectize=TRUE, choices=c("Choose one"="",choices))
}

selectizeInput01<-function(id, label, choices) {
  selectizeInput(inputId=id, label=label, multiple=TRUE, 
              choices=c("Choose two"="",choices),
              options=list(maxItems=2))
}

selectizeInput02<-function(id, label, choices) {
  selectizeInput(inputId=id, label=label, multiple=TRUE, 
                 choices=c("Choose three"="",choices),
                 options=list(maxItems=3))
}

## Feature scaling/extraction: discretization
# radio buttons
#yes/no on whether to log10-transform axis
radioButtons01<-function(var){
  radioButtons(inputId=paste("rad_dis2a",var, "trnsFea04",sep="_"),
               label="Choose whether to log10-scale the y-axis",
               choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
}

#choose who selects bin boundaries
radioButtons02<-function(var){
  radioButtons(inputId=paste("rad_dis2b", var, "trnsFea04",sep="_"),
              label="Choose who selects the bin boundaries",
              choices=c("R","me"),selected=character(0),inline=TRUE)
}

# numeric input
#choose number of breaks
numericInput01<-function(var){
  numericInput(inputId=paste("num_dis2a", var, "trnsFea04",sep="_"),
              label="Select the number of breaks to create data bins (1-5)",
              value=2,min=1,max=5)
}



#### EDA Tab Structure Code=======================================================
edaTabBuilder<-function(name,tabID,varID,options,fn){
  tabPanel(title=name,id=tabID,
    titlePanel(title=paste(name,"Exploratory Data Analysis",sep=" ")),
      #inputs
    wellPanel(
      fluidRow(
        column(6,
          fn(id=paste("sel",varID[1],tabID,sep="_"),label="",choices=options)
        ),
        column(6,
          fn(id=paste("sel",varID[2],tabID,sep="_"),label="",choices=options)
        )
      )
    ),
       #outputs
      fluidRow(
        column(6,
          htmlOutput(paste("text_sel",varID[1],tabID,sep="_")),
          DTOutput(paste("tab_sel",varID[1],tabID,sep="_"))
        ),
        column(6,
          htmlOutput(paste("text_sel",varID[2],tabID,sep="_")),
          DTOutput(paste("tab_sel",varID[2],tabID,sep="_"))
        )
      ),
      fluidRow(
        column(6,
          plotOutput(paste("plot_sel",varID[1],tabID,sep="_"))
        ),
        column(6,
          plotOutput(paste("plot_sel",varID[2],tabID,sep="_"))
        )
      )
    )
}


#### tabsetPanel Builders==================================================================
### Function for sidebar panel
tabPaneler01<-function(var){
  tabPanelBody(var,
    #uiOutput("ui_sel_dis1_trnsFea04"),
    radioButtons(inputId=paste("rad_dis1",var,"trnsFea04",sep="_"),
                 label="Choose whether to log10-scale the x-axis",
                 choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE),
    numericInput(inputId=paste("num_dis1",var,"trnsFea04",sep="_"),
                 label="Select the number of bins for the histogram (2-100)",
                 value=30,min=2,max=100),
    br(),
    actionButton(inputId=paste("btn_dis2",var,"trnsFea04",sep="_"),
                 label="Visualize binned data?"),
    uiOutput(paste("ui_rad_dis2a",var,"trnsFea04",sep="_")),
    uiOutput(paste("ui_num_dis2a",var,"trnsFea04",sep="_")),
    uiOutput(paste("ui_rad_dis2b",var,"trnsFea04",sep="_")),
    uiOutput(paste("ui_num_dis2b",var,"trnsFea04",sep="_")),
    htmlOutput(paste("text_dis3",var,"trnsFea04",sep="_")),
    fluidRow(
      column(6,
             uiOutput(paste("ui_btn_dis3a",var,"trnsFea04",sep="_"))
      ),
      column(6,
             uiOutput(paste("ui_btn_dis3b",var,"trnsFea04",sep="_"))
      )
    )
  )
}

#create a vector
num_vars<-trainDF %>%
  select(where(is.numeric),num) %>% names()

#run vector through function
ui_tabPanelBody<-map(num_vars,tabPaneler01)


### Function for main panel
tabPaneler02<-function(var){
  tabPanelBody(var,
    plotOutput(paste("plot_sel_dis1",var,"trnsFea04",sep="_")),
    plotOutput(paste("plot_sel_dis2",var,"trnsFea04",sep="_"))
  )
}


#### Batch output====================================================================
### Many uiOutput
split_chk_sel_builder<-function(lab){
  splitLayout(cellWidths=c("30%","70%"),cellArgs=list(style="vertical-align: middle"),
    uiOutput(paste0("ui_chk_ordEnc",lab,"_trnsFea04")),
    uiOutput(paste0("ui_sel_ordEnc",lab,"_trnsFea04")),
    #SO: parent div of dropdown menu has an overflow style, blocking dropdown menu; 
    #this changes it to visible
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
  )
}

#creates a vector
labs<-paste0(rep(2,5),letters[1:5])

#run vector through function
ui_splits<-map(labs,split_chk_sel_builder)







  
  
  
  
  
  







