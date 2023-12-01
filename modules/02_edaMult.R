# EDA-Multivariate Module


# UI================================================================================================
edaMultUI <- function(id) {
  tabPanel(title="Multivariate",
    edaTabBuilder(id,
                  name="Multivariate",
                  # tabID="mulEDA02",
                  varID=c("var1abc", "var2abc"),
                  # varID=c("var123", "var456"),
                  options=ncharVarClass,
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
    
    
    ## Plot outputs--------------------
    output$plot_sel_var1abc <- renderPlot({
      #must select three inputs first
      req(length(input$sel_var1abc)==3)
      #if all categorical, then bar plot
      if(sum(input$sel_var1abc %in% catVars)==3) {
        df_train %>%
          {if(sum(input$sel_var1abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          barplotter(input$sel_var1abc)
      }
      #if 2 cat & 1 num then boxplot
      else if(sum(input$sel_var1abc %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var1abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          boxplotter(input$sel_var1abc)
      }
      #if 2-3 num then scatterplot
      else if(sum(input$sel_var1abc %in% catVars) < 2) {
        df_train %>%
          {if(sum(input$sel_var1abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          scatterplotter(input$sel_var1abc)
      }
    })
    
    output$plot_sel_var2abc <- renderPlot({
      req(length(input$sel_var2abc)==3)
      if(sum(input$sel_var2abc %in% catVars)==3) {
        df_train %>%
          {if(sum(input$sel_var2abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          barplotter(input$sel_var2abc)
      }
      else if(sum(input$sel_var2abc %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var2abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          boxplotter(input$sel_var2abc)
      }
      else if(sum(input$sel_var2abc %in% catVars) < 2) {
        df_train %>%
          {if(sum(input$sel_var2abc=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          scatterplotter(input$sel_var2abc)
      }
    })
    
  })
}

  
  
  
  
  
  
  