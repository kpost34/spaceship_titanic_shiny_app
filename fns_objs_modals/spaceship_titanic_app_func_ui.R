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







  
  
  
  
  
  






