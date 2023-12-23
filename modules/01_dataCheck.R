# Data Check Module

# UI================================================================================================
dataCheckUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    ## Input selectors for two tables
    sidebarPanel(width=2,
      selectInput01(ID=ns("sel_quick"), label="Quick data check", choices=ch_quick_dataCheck),
      linebreaks(2),
      selectInput01(ID=ns("sel_summ"), label="Data summaries", choices=ch_summ_dataCheck),
    ),
    ## Tabular outputs
    mainPanel(width=10,
      DTOutput(ns("tab_quick")),
      linebreaks(2),
      DTOutput(ns("tab_summ"))
    )
  )
}




# Server============================================================================================
dataCheckServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Display data checks dependent upon user selection
    ### Create reactive obj df_check()
    df_check <- reactive({
      switch(input$sel_quick,
             dim=dim_tbl(df_train),
             dat_samp=slice_sample(df_train, n=5),
             miss=n_miss_tbl(df_train)
      )
    })
    
    ### Generate DT
    output$tab_quick <- renderDT(
      df_check(), 
      rownames=FALSE, 
      options=list(dom="tip",
                   autoWidth=TRUE,
                   pageLength=7,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all")),
                   #conditionally display scroll bar
                   scrollX=if(input$sel_quick=="dat_samp") {TRUE} else {FALSE}),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        extract_nm(ch_quick_dataCheck, input$sel_quick))
    )

    ## Display data summary by col type
    ### Create reactive df_summ
    df_summ <- reactive({
      switch(input$sel_summ,
        chr=skim_tbl(df_train, type="character"),
        fct=skim_tbl(df_train, type="factor"),
        lgl=skim_tbl(df_train, type="logical"),
        num=skim_tbl(df_train, type="numeric"))
    })
  
    ### Generate DT
    output$tab_summ <- renderDT(
      df_summ(), 
      rownames=FALSE,
      options=list(dom="t",
                   autoWidth=TRUE,
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        paste(extract_nm(ch_summ_dataCheck, input$sel_summ), "Variables"))
    )
    
  })
}




