# EDA-Multivariate Module


# UI================================================================================================
edaMultUI <- function(id) {
  tabPanel(title="Multivariate",
    edaTabBuilder(id,
                  name="Multivariate",
                  # tabID="mulEDA02",
                  varID=c("var1abc", "var2abc"),
                  # varID=c("var123", "var456"),
                  options=df_train_nchrVars,
                  fn=selectizeInput02)
  )
}




# Server============================================================================================
edaMultServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Text outputs--------------------
    output$text_sel_var1abc <-renderUI({
      h3(paste(input$sel_var1abc, collapse="-"))
    })

    output$text_sel_var2abc <- renderUI({
      h3(paste(input$sel_var2abc, collapse="-"))
    })
    
    # ## Text outputs--------------------
    # output$text_sel_var123_mulEDA02<-renderUI({
    #   h3(paste(input$sel_var123_mulEDA02,collapse="-"))
    # })
    # 
    # output$text_sel_var456_mulEDA02<-renderUI({
    #   h3(paste(input$sel_var456_mulEDA02,collapse="-"))
    # })
    
    
    ## Plot outputs--------------------
    output$plot_sel_var1abc <- renderPlot({
      #must select three inputs first
      req(length(input$sel_var1abc)==3)
      #if all categorical, then bar plot
      if(sum(input$sel_var1abc %in% df_train_catVars)==3) {
        barplotter(df_train, input$sel_var1abc)
      }
      #if 2 cat & 1 num then boxplot
      else if(sum(input$sel_var1abc %in% df_train_catVars)==2) {
        boxplotter(df_train, input$sel_var1abc)
      }
      #if 2-3 num then scatterplot
      else if(sum(input$sel_var1abc %in% df_train_catVars) < 2) {
        scatterplotter(df_train, input$sel_var1abc)
      }
    })
    
    output$plot_sel_var2abc <- renderPlot({
      req(length(input$sel_var2abc)==3)
      if(sum(input$sel_var2abc %in% df_train_catVars)==3) {
        barplotter(df_train, input$sel_var2abc)
      }
      else if(sum(input$sel_var2abc %in% df_train_catVars)==2) {
        boxplotter(df_train, input$sel_var2abc)
      }
      else if(sum(input$sel_var2abc %in% df_train_catVars) < 2) {
        scatterplotter(df_train, input$sel_var2abc)
      }
    })
    
    
    # ## Plot outputs--------------------
    # output$plot_sel_var123_mulEDA02<-renderPlot({
    #   #must select three inputs first
    #   req(length(input$sel_var123_mulEDA02)==3)
    #   #if all categorical, then bar plot
    #   if(sum(input$sel_var123_mulEDA02 %in% df_train_catVars)==3) {
    #     barplotter(df_train,input$sel_var123_mulEDA02)
    #   }
    #   #if 2 cat & 1 num then boxplot
    #   else if(sum(input$sel_var123_mulEDA02 %in% df_train_catVars)==2) {
    #     boxplotter(df_train,input$sel_var123_mulEDA02)
    #   }
    #   #if 2-3 num then scatterplot
    #   else if(sum(input$sel_var123_mulEDA02 %in% df_train_catVars) < 2) {
    #     scatterplotter(df_train,input$sel_var123_mulEDA02)
    #   }
    # })
    # 
    # output$plot_sel_var456_mulEDA02<-renderPlot({
    #   req(length(input$sel_var456_mulEDA02)==3)
    #   if(sum(input$sel_var456_mulEDA02 %in% df_train_catVars)==3) {
    #     barplotter(df_train,input$sel_var456_mulEDA02)
    #   }
    #   else if(sum(input$sel_var456_mulEDA02 %in% df_train_catVars)==2) {
    #     boxplotter(df_train,input$sel_var456_mulEDA02)
    #   }
    #   else if(sum(input$sel_var456_mulEDA02 %in% df_train_catVars) < 2) {
    #     scatterplotter(df_train,input$sel_var456_mulEDA02)
    #   }
    # })
    
  })
}

  
  
  
  
  
  
  