#Functions for UI

## Easily create line breaks====================================
linebreaks <- function(n){HTML(strrep(br(), n))}


### selectInput function========================================
selectInput01<-function(id, label, choices) {
  selectInput(inputId=id, label=label, selectize=TRUE, choices=c("Choose one"="",choices))
}

selectizeInput01<-function(id, label, choices) {
  selectizeInput(inputId=id, label=label, multiple=TRUE, 
              choices=c("Choose two"="",choices),
              options=list(maxItems=2))
}


