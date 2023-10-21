# EDA-Bivariate Module


#UI=================================================================================================
edaBiUI <- function(id) {
  tabPanel(title="Bivariate",
    edaTabBuilder(id,
                  name="Bivariate",
                  tabID="biEDA02",
                  varID=c("var12","var34"),
                  options=trainDF_nchrVars,
                  fn=selectizeInput01)
  )
}




# Server============================================================================================
edaBiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    ## Text outputs----------------------
    output$text_sel_var12_biEDA02<-renderUI({
      h3(paste(input$sel_var12_biEDA02,collapse="-"))
    })
    
    output$text_sel_var34_biEDA02<-renderUI({
      h3(paste(input$sel_var34_biEDA02,collapse="-"))
    })
    
    
    ## Table outputs----------------------
    ### Create reactives of output tables
    dat1_biEDA02<-reactive({
      req(length(input$sel_var12_biEDA02)==2)
      #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
      if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
        tabylize(trainDF,input$sel_var12_biEDA02)
      }
      else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
        summaryize(trainDF,input$sel_var12_biEDA02,input$sel_var12_biEDA02[input$sel_var12_biEDA02 %in% trainDF_catVars])
      }
      else if(sum(input$sel_var12_biEDA02 %in% trainDF_numVars)==2) {
        corrtester(trainDF,input$sel_var12_biEDA02)
      }
    })
  
  
    dat2_biEDA02<-reactive({
      req(length(input$sel_var34_biEDA02)==2)
      #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
      if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
        tabylize(trainDF,input$sel_var34_biEDA02)
      }
      else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
        summaryize(trainDF,input$sel_var34_biEDA02,input$sel_var34_biEDA02[input$sel_var34_biEDA02 %in% trainDF_catVars])
      }
      else if(sum(input$sel_var34_biEDA02 %in% trainDF_numVars)==2) {
        corrtester(trainDF,input$sel_var34_biEDA02)
      }
    })
    
    
    ### Output tables
    output$tab_sel_var12_biEDA02<-renderDT(
      dat1_biEDA02(),options=list(scrollX="400px",
                                    pageLength=5)
    )
    
    output$tab_sel_var34_biEDA02<-renderDT(
      dat2_biEDA02(),options=list(scrollX="400px",
                                    pageLength=5)
    )
  
    
    ## Plot outputs----------------------
    output$plot_sel_var12_biEDA02<-renderPlot({
      #must select two inputs first
      req(length(input$sel_var12_biEDA02)==2)
      #if both categorical, then bar plot
      if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
        barplotter(trainDF,input$sel_var12_biEDA02)
      }
      #if 1 cat & 1 num then boxplot
      else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
        boxplotter(trainDF,input$sel_var12_biEDA02)
      }
      #if two num then scatterplot
      else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==0) {
        scatterplotter(trainDF,input$sel_var12_biEDA02)
      }
    })
    
    output$plot_sel_var34_biEDA02<-renderPlot({
      req(length(input$sel_var34_biEDA02)==2)
      if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
        barplotter(trainDF,input$sel_var34_biEDA02)
      }
      else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
        boxplotter(trainDF,input$sel_var34_biEDA02)
      }
      else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==0) {
        scatterplotter(trainDF,input$sel_var34_biEDA02)
      }
    })
    
  })
}
