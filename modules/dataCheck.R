# Data Check Module

# UI================================================================================================
dataCheckUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width=2,
      selectInput01(ID=ns("sel_quick"), label="Quick data check", choices=vec_quick_chk),
      linebreaks(2),
      selectInput01(ID=ns("sel_summ"), label="Data summaries", choices=vec_summ_chk),
    ),
    mainPanel(width=10,
      DTOutput(ns("tab_quick")),
      br(),
      DTOutput(ns("tab_summ"))
    )
  )
  # )
}




# Server============================================================================================
dataCheckServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    df_check <- reactive({
      switch(input$sel_quick,
             dim=dim_tbl(df_train),
             dat_samp=slice_sample(df_train, n=5),
             miss=n_miss_tbl(df_train)
      )
    })

    output$tab_quick <- renderDT(
      df_check(), rownames=FALSE, 
      options=list(dom="tip",
                   pageLength=12,
                   autoWidth=TRUE)
    )

    ### Display data summary by col type
    df_summ <- reactive({
      switch(input$sel_summ,
        chr=skim_tbl(df_train, type="character"),
        fct=skim_tbl(df_train, type="factor"),
        lgl=skim_tbl(df_train, type="logical"),
        num=skim_tbl(df_train, type="numeric"))
    })
  
    output$tab_summ <- renderDT(
      df_summ(), rownames=FALSE,
      options=list(dom="tip",
                   autoWidth=TRUE)
    )
    
  })
}




