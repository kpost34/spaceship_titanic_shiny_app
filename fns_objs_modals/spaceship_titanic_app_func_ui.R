#Functions for UI

# Helper UI Functionss=====================================================
## Easily Create Line Break
linebreaks <- function(n){HTML(strrep(br(), n))}


## Extract name of selected item
extract_nm <- function(vec, val) { #val is in quotes
  names(vec)[vec==val] %>% 
    str_to_title()
}



# Input functions========================================
selectInput01<-function(ID, label, choices) {
  selectInput(inputId=ID, label=label, selectize=TRUE, 
              choices=c("Choose one"="", choices))
}

selectizeInput01<-function(ID, label, choices) {
  selectizeInput(inputId=ID, label=label, multiple=TRUE, 
              choices=c("Choose two"="", choices),
              options=list(maxItems=2))
}

selectizeInput02<-function(ID, label, choices) {
  selectizeInput(inputId=ID, label=label, multiple=TRUE, 
                 choices=c("Choose three"="", choices),
                 options=list(maxItems=3))
}


# EDA Tab Structure Code=======================================================
edaTabBuilder<-function(id, name, varID, options, fn){
  ns <- NS(id)
  
  height_val <- if(name=="Multivariate") {
    "600px"
  } else{
    "400px"
  }
  
  tagList(
    #title and well panels
    titlePanel(title=paste(name,"Exploratory Data Analysis",sep=" ")),
    #inputs
    wellPanel(
      fluidRow(
        column(6,
          fn(ID=ns(paste("sel",varID[1],sep="_")),label="",choices=options)
        ),
        column(6,
          fn(ID=ns(paste("sel",varID[2],sep="_")),label="",choices=options)
        )
      )
    ),
    #outputs
    splitLayout(cellWidths=c("49.5%", "1%", "49.5%"),
      #first var output
      tagList(
        column(12,
          htmlOutput(ns(paste("text_sel",varID[1],sep="_"))),
          DTOutput(ns(paste("tab_sel",varID[1],sep="_"))),
          linebreaks(2),
          plotOutput(ns(paste("plot_sel",varID[1],sep="_")), height=height_val)
        )
      ),
      #space in middle
      tagList(),
      #second var output
      tagList(
        column(12,
          htmlOutput(ns(paste("text_sel",varID[2],sep="_"))),
          DTOutput(ns(paste("tab_sel",varID[2],sep="_"))),
          linebreaks(2),
          plotOutput(ns(paste("plot_sel",varID[2],sep="_")), height=height_val)
        )
      )
    )
  )
}
      


# Batch output====================================================================
## Many uiOutput
split_chk_sel_builder<-function(lab, fn){
  splitLayout(cellWidths=c("30%","70%"),cellArgs=list(style="vertical-align: middle"),
    uiOutput(fn(paste0("ui_chk_ordEnc",lab))),
    uiOutput(fn(paste0("ui_sel_ordEnc",lab))),
    #SO: parent div of dropdown menu has an overflow style, blocking dropdown menu; 
    #this changes it to visible
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
  )
}

#creates a vector
labs<-paste0(rep(2,5),letters[1:5])







  
  
  
  
  
  







