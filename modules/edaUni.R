# EDA-Univariate Module


# UI================================================================================================
edaUniUI <- function(id) {
    tabPanel(title="Univariate",
      edaTabBuilder(id,
                    name="Univariate",
                    varID=c("var1","var2"),
                    options=df_train_nchrVars,
                    fn=selectInput01)
    )
}



# Server============================================================================================
edaUniServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Tabular outputs----------------------
    ### reactives of output tables
    dat1<-reactive({
      if(input$sel_var1 %in% df_train_numVars){
        summaryize(df_train,input$sel_var1)
      }
      else if(input$sel_var1 %in% df_train_catVars){
        df_train %>%
          #bin num into six equally spaced groups
          {if(input$sel_var1=="num") 
            mutate(., 
                   num=as.numeric(num), 
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          tabylize(input$sel_var1)
      }
    })
  
    dat2<-reactive({
      if(input$sel_var2 %in% df_train_numVars){
        summaryize(df_train,input$sel_var2)
      }
      else if(input$sel_var2 %in% df_train_catVars){
        df_train %>%
          {if(input$sel_var2=="num") 
            mutate(., 
                   num=as.numeric(num), 
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          tabylize(input$sel_var2)
      }
    }) 
    
    ### Output tables
    output$tab_sel_var1<-renderDT(
      dat1(),
      rownames=FALSE,
      options=list(dom="t",
                   pageLength=10,
                   scrollX=TRUE,
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:175% ;",
        input$sel_var1)
    )
  
    output$tab_sel_var2<-renderDT(
      dat2(),
      rownames=FALSE, 
      options=list(dom="t",
                   pageLength=10,
                   scrollX=TRUE,
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:175% ;",
        input$sel_var2)
    )
  
  
    ## Plot outputs----------------------
    output$plot_sel_var1<-renderPlot({
      if(input$sel_var1 %in% df_train_numVars){
        histogrammer(df_train,input$sel_var1)
      }
      else if(input$sel_var1 %in% df_train_catVars){
        df_train %>%
          #bin num into six equally spaced groups
          {if(input$sel_var1=="num")
              mutate(., 
                     num=as.numeric(num), 
                     num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          barplotter(input$sel_var1)
      }
    })
  
    output$plot_sel_var2<-renderPlot({
      if(input$sel_var2 %in% df_train_numVars){
        histogrammer(df_train,input$sel_var2)
      }
      else if(input$sel_var2 %in% df_train_catVars){
        df_train %>%
          {if(input$sel_var2=="num")
              mutate(., 
                     num=as.numeric(num), 
                     num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          barplotter(input$sel_var2)
      }
    })
  })
}