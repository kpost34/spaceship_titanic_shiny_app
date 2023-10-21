# EDA-Univariate Module


#UI=================================================================================================
edaUniUI <- function(id) {
    tabPanel(title="Univariate",
      edaTabBuilder(id,
                    name="Univariate",
                    tabID="uniEDA02",
                    varID=c("var1","var2"),
                    options=trainDF_nchrVars,
                    fn=selectInput01)
    )
}



# Server============================================================================================
edaUniServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Text outputs----------------------
    output$text_sel_var1_uniEDA02<-renderUI({
      h3(paste(input$sel_var1_uniEDA02))
    })
    
    output$text_sel_var2_uniEDA02<-renderUI({
      h3(paste(input$sel_var2_uniEDA02))
    })
    
    ## Table outputs----------------------
    ### reactives of output tables
    dat1_uniEDA02<-reactive({
      if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
        summaryize(trainDF,input$sel_var1_uniEDA02)
      }
      else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
        tabylize(trainDF,input$sel_var1_uniEDA02)
      }
    })
  
    dat2_uniEDA02<-reactive({
      if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
        summaryize(trainDF,input$sel_var2_uniEDA02)
      }
      else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
        tabylize(trainDF,input$sel_var2_uniEDA02)
      }
    })
    
    ### Output tables
    output$tab_sel_var1_uniEDA02<-renderDT(
      dat1_uniEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
    )
  
    output$tab_sel_var2_uniEDA02<-renderDT(
      dat2_uniEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
    )
  
  
    ## Plot outputs----------------------
    output$plot_sel_var1_uniEDA02<-renderPlot({
      if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
        histogrammer(trainDF,input$sel_var1_uniEDA02)
      }
      else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
        barplotter(trainDF,input$sel_var1_uniEDA02)
      }
    })
  
    output$plot_sel_var2_uniEDA02<-renderPlot({
      if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
        histogrammer(trainDF,input$sel_var2_uniEDA02)
      }
      else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
        barplotter(trainDF,input$sel_var2_uniEDA02)
      }
    })
    
  })
}


