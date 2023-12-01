# EDA-Bivariate Module


# UI================================================================================================
edaBiUI <- function(id) {
  tabPanel(title="Bivariate",
    edaTabBuilder(id,
                  name="Bivariate",
                  # tabID="biEDA02",
                  varID=c("var1ab", "var2ab"),
                  # varID=c("var12","var34"),
                  options=ncharVarClass,
                  fn=selectizeInput01)
  )
}




# Server============================================================================================
edaBiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    ## Table outputs----------------------
    ### Create reactives of output tables
    dat1<-reactive({
      req(length(input$sel_var1ab)==2)
      #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
      if(sum(input$sel_var1ab %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var1ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          tabylize(input$sel_var1ab)
      }
      else if(sum(input$sel_var1ab %in% catVars)==1) {
        df_train %>%
          {if(sum(input$sel_var1ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          summaryize(input$sel_var1ab,input$sel_var1ab[input$sel_var1ab %in% catVars])
      }
      else if(sum(input$sel_var1ab %in% numVars)==2) {
        corrtester(df_train,input$sel_var1ab)
      }
    })
  
  
    dat2<-reactive({
      req(length(input$sel_var2ab)==2)
      #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
      if(sum(input$sel_var2ab %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var2ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          tabylize(input$sel_var2ab)
      }
      else if(sum(input$sel_var2ab %in% catVars)==1) {
        df_train %>%
          {if(sum(input$sel_var2ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          summaryize(input$sel_var2ab,input$sel_var2ab[input$sel_var2ab %in% catVars])
      }
      else if(sum(input$sel_var2ab %in% numVars)==2) {
        corrtester(df_train,input$sel_var2ab)
      }
    })
    
    
    ### Output tables
    output$tab_sel_var1ab<-renderDT(
      dat1(),
      rownames=FALSE,
      options=list(dom="t",
                   pageLength=10,
                   scrollX=TRUE,
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:175% ;",
        paste(input$sel_var1ab[1], input$sel_var1ab[2], sep="-"))
    )
    
    output$tab_sel_var2ab<-renderDT(
      dat2(),
      rownames=FALSE,
      options=list(dom="t",
                   pageLength=10,
                   scrollX=TRUE,
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:175% ;",
        paste(input$sel_var2ab[1], input$sel_var2ab[2], sep="-"))
    )
  
    
    ## Plot outputs----------------------
    output$plot_sel_var1ab<-renderPlot({
      #must select two inputs first
      req(length(input$sel_var1ab)==2)
      #if both categorical, then bar plot
      if(sum(input$sel_var1ab %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var1ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
        barplotter(input$sel_var1ab)
      }
      #if 1 cat & 1 num then boxplot
      else if(sum(input$sel_var1ab %in% catVars)==1) {
        df_train %>%
          {if(sum(input$sel_var1ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          boxplotter(input$sel_var1ab)
      }
      #if two num then scatterplot
      else if(sum(input$sel_var1ab %in% catVars)==0) {
        scatterplotter(df_train,input$sel_var1ab)
      }
    })
    
    output$plot_sel_var2ab<-renderPlot({
      req(length(input$sel_var2ab)==2)
      if(sum(input$sel_var2ab %in% catVars)==2) {
        df_train %>%
          {if(sum(input$sel_var2ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
        barplotter(input$sel_var2ab)
      }
      else if(sum(input$sel_var2ab %in% catVars)==1) {
        df_train %>%
          {if(sum(input$sel_var2ab=="num") ==1)
            mutate(.,
                   num=as.numeric(num),
                   num=cut_width(num, width=303, boundary=0, dig.lab=4)) else .} %>%
          boxplotter(input$sel_var2ab)
      }
      else if(sum(input$sel_var2ab %in% catVars)==0) {
        scatterplotter(df_train,input$sel_var2ab)
      }
    })
    
  })
}

