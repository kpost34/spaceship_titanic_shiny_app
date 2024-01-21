# EDA-Univariate Module


# UI================================================================================================
edaUniUI <- function(id) {
  tabPanel(title="Univariate",
    edaTabBuilder(id,
                  name="Univariate",
                  varID=c("var1","var2"),
                  options=ncharVarClass,
                  fn=selectInput01)
  )
}



# Server============================================================================================
edaUniServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Tabular outputs----------------------
    ### reactives of output tables
    #first reactive DF
    dat1 <- reactive({
      if(input$sel_var1 %in% numVars){
        summaryize(df_train, input$sel_var1)
      }
      else if(input$sel_var1 %in% catVars){
        df_train %>%
          tabylize(input$sel_var1)
      }
    })
  
    #second reactive DF
    dat2 <- reactive({
      if(input$sel_var2 %in% numVars){
        summaryize(df_train,input$sel_var2)
      }
      else if(input$sel_var2 %in% catVars){
        df_train %>%
          tabylize(input$sel_var2)
      }
    }) 
    
    
    ### Output tables
    #first DT
    output$tab_sel_var1 <- renderDT(
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
  
    #second DT
    output$tab_sel_var2 <- renderDT(
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
    #first plot
    output$plot_sel_var1 <- renderPlot({
      if(input$sel_var1 %in% numVars){
        histogrammer(df_train,input$sel_var1)
        
      } else if(input$sel_var1 %in% catVars){
        df_train %>%
          barplotter(input$sel_var1)
      }
    })
  
    #second plot
    output$plot_sel_var2 <- renderPlot({
      if(input$sel_var2 %in% numVars){
        histogrammer(df_train,input$sel_var2)
        
      } else if(input$sel_var2 %in% catVars){
        df_train %>%
          barplotter(input$sel_var2)
      }
    })
  })
}