# Data Check Module

# UI================================================================================================
dataCheckUI <- function(id) {
  ns <- NS(id)
  
  # tabPanel(title="Data Check",
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput01(ID=ns("sel_quick_Chk01"),label="Quick data check",choices=Chk01_quickVec),
      linebreaks(2),
      selectInput01(ID=ns("sel_summ_Chk01"),label="Data summaries",choices=Chk01_summVec),
    ),
    mainPanel(width=9,
      DTOutput(ns("tab_sel_quick_Chk01")),
      br(),
      DTOutput(ns("tab_sel_summ_Chk01"))
    )
  )
  # )
}




# Server============================================================================================
dataCheckServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    dat_check<-reactive({
      switch(input$sel_quick_Chk01,
             dim=dim_tbl(trainDF),
             dat_samp=slice_sample(trainDF,n=5),
             miss=n_miss_tbl(trainDF)
      )
    })

    output$tab_sel_quick_Chk01<-renderDT(
      dat_check(),options=list(scrollX="400px",
                               pageLength=5)
    )

    ### Display data summary by col type
    dat_sum<-reactive({
      switch(input$sel_summ_Chk01,
        chr=skim_tbl(trainDF,type="character"),
        fct=skim_tbl(trainDF,type="factor"),
        lgl=skim_tbl(trainDF,type="logical"),
        num=skim_tbl(trainDF,type="numeric"))
    })
  
    output$tab_sel_summ_Chk01<-renderDT(
      dat_sum(),options=list(scrollX="400px")
    )
    
  })
}




