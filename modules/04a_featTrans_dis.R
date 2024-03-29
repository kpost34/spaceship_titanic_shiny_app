#Feature Engineering-Transformation Module: Discretization Submodule

# UI================================================================================================
featTrans_disUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width=3,
      #ui for histogram
      h4("Histogram of raw data"),
      selectInput01(ID=ns("sel_var_hist"), label=varViz_feat, choices=disVars),
      radioButtons(inputId=ns("rad_log_hist"),
                   label="Choose whether to log10-scale the x-axis",
                   choices=c("Yes"=TRUE, "No"=FALSE), selected=character(0), inline=TRUE),
      numericInput(inputId=ns("num_bin_hist"),
                   label="Select the number of bins for the histogram (2-50)",
                   value=10,min=2,max=50),
      br(),
      fluidRow(
        column(9,
          h5(strong(textOutput(ns("text_not_dis"))))
        ),
        column(3, 
          div(style="margin-bottom: 15px;",
              uiOutput(ns("ui_btn_not_dis"))
          )
        )
      ),
      
      br(),
      
      #ui for barplot (by discretizing numerical variable)
      h4("Visualization of binned data"),
      radioButtons(inputId=ns("rad_log_bar"),
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes"=TRUE, "No"=FALSE), selected=character(0), inline=TRUE),
      numericInput(inputId=ns("num_brk_bar"),
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2, min=1, max=5),
      radioButtons(inputId=ns("rad_bdry_bar"),
                   label="How should data be binned?",
                   choices=ch_bin_opt_featTrans,
                   selected=character(0), inline=TRUE),
      uiOutput(ns("ui_num_bdry_bar")),
      fluidRow(
        column(9,
          h5(strong(textOutput(ns("text_dis"))))
        ),
        column(3,
          uiOutput(ns("ui_btn_dis")),
        )
      ),
      br(),
      hr(style = "border-top: 1px solid #000000;"),
      
      #summary table that shows discretization employed
      DTOutput(ns("table_dis_summ")),
      br(),
      
      #complete discretization actionButton
      fluidRow(
        column(12, align="center",
          actionButton(ns("btn_dis_complete"), 
                       label="Complete discretization", 
                       class="btn-success"),
          strong(textOutput(ns("text_btn_dis_complete")))
        )
      )
    ),
    
    mainPanel(width=9,
      #histogram
      plotOutput(ns("plot_sel_var_hist")),
      linebreaks(2),
      
      #barplot
      plotOutput(ns("plot_sel_bar")),
      
      #temporary outputs (to be removed)
      tableOutput(ns("temp_table_hist")),
      verbatimTextOutput(ns("check_rv"))
    )
  )
}



# Server============================================================================================
featTrans_disServer <- function(id, df_train_nd_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    ## Inputs--------------------
    ### Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
    output$ui_num_bdry_bar<-renderUI({
      req(input$rad_bdry_bar=="user")
      
      tags_num<-tagList()
      
      for(i in seq_len(input$num_brk_bar)){
        tags_num[[i]] <- numericInput(ns(paste0("n",i)), paste0("Break",i), min=0, value=NULL)
      }
      
      tags_num
    })
  
  
    ### Dynamically displays action buttons (and associated text) to discretize/not discretize variable 
    #### Display not-discretize txt
    output$text_not_dis <- renderText({
      req(input$sel_var_hist %in% disVars)

      paste0("Do not discretize ", input$sel_var_hist,".")
    })
    
    
    #### Display discretize txt
    output$text_dis <- renderText({
      #either "Equal Intervals" is selected or "User specifications" is selected and the number and 
        #every break point input is populated
      req(input$sel_var_hist %in% disVars,
            (!is.na(input$rad_log_bar) & input$rad_bdry_bar=="cut_int")|
              (input$rad_bdry_bar=="user" &  sum(!is.na(user_cuts()))==input$num_brk_bar)
      )
      
      paste("Discretize",input$sel_var_hist, "using these settings.")
    })
    
    
    #### Display buttons
    #not discretize
    output$ui_btn_not_dis<-renderUI({
      req(input$sel_var_hist %in% disVars)
      
      actionButton(ns(paste("btn_not_dis",
                            input$sel_var_hist,
                            sep="_")),
                   label="Confirm",
                   class="btn-primary")
    })
    
    #discretize
    output$ui_btn_dis<-renderUI({
      req(input$sel_var_hist %in% disVars,
          (!is.na(input$rad_log_bar) & input$rad_bdry_bar=="cut_int")|
            (input$rad_bdry_bar=="user" &  sum(!is.na(user_cuts()))==input$num_brk_bar)
      )
      
      actionButton(ns(paste("btn_dis" ,input$sel_var_hist, sep="_")), label="Confirm",
                   class="btn-primary")
    })
    
    
    
    ## Outputs--------------------
    ### Plot raw data with fill=transported as histogram
    output$plot_sel_var_hist<-renderPlot({
      req(input$sel_var_hist %in% disVars,
          input$rad_log_hist)
      
      #simplify code
      histogrammer2(dat=df_train_nd_nvI(),col=input$sel_var_hist,
          n.bins=input$num_bin_hist,x.log.scale=input$rad_log_hist)
    })
    
    
    ### Create reactive using cut options
    #### Define cuts 
    user_cuts<-reactive({
      c(input$n1,input$n2,input$n3,input$n4,input$n5)
    })

    
    ### Generate DF
    df_cut <- reactive({
      req(input$sel_var_hist %in% disVars,
          input$rad_bdry_bar, input$num_brk_bar, input$rad_log_bar)
      
      if(input$rad_bdry_bar=="cut_int") {
        equal_cutter(dat=df_train_nd_nvI(), col=input$sel_var_hist, n.breaks=input$num_brk_bar)
      
      } else if(input$rad_bdry_bar=="user") {
        user_cutter(dat=df_train_nd_nvI(), col=input$sel_var_hist, break.vals=user_cuts())
        
      }
    })
    
    
    ### Generate plot
    output$plot_sel_bar <- renderPlot({
      req(df_cut())
      
      bin_plotter(dat=df_cut(), 
                  col=input$sel_var_hist, 
                  type=input$rad_bdry_bar, 
                  y.log.scale=input$rad_log_bar)
    })
    
    
    ### Discretization summary table
    #### Create temp reactive DF that holds variable name and discretization type
    df_dis_type <- reactive({
      req(input$sel_var_hist)
      req(input$rad_bdry_bar)
      req(input$num_brk_bar)
      
      tibble(predictor=input$sel_var_hist,
             type=input$rad_bdry_bar,
             bins=input$num_brk_bar) %>%
        mutate(type=str_replace(type, "cut_int", "equal"),
               bins=as.integer(bins) + 1)
    })
    
    
    ### Create reactiveVal var & populate with variable selected in histogram
    var <- reactiveVal(NA_character_)
    
    observeEvent(input$sel_var_hist, {
      var(input$sel_var_hist)
    })
    
    
    ### Create reactiveValues rv_dis_type
    rv_dis_type <- reactiveValues()
    
    
    ### Update reactiveValues rv_dis_type
    #### If discretization selected
    observeEvent(input[[paste("btn_dis", var(), sep="_")]], {
      rv_dis_type[[var()]] <- df_dis_type()
    })
    
    
    #### If no discretization selected
    observeEvent(input[[paste("btn_not_dis", var(), sep="_")]], {
      rv_dis_type[[var()]] <- NULL
    })
    
    
    ### Update full reactive DF that holds variable name and discretization type
    df_dis_summ <- reactive({
      rv_dis_type %>%
        reactiveValuesToList() %>%
        reduce(bind_rows, .init=NULL)
    })
    
    
    ### Create table
    output$table_dis_summ <- renderDT(
      df_dis_summ(),
      rownames=FALSE,
      options=list(dom="t"),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        "Summary of discretization by variable")
    )
    
    
    
    ## Export-------------------- 
    ### Create reactiveValues() rv_dis & populate it with every confirmed discretized variable
    rv_dis <- reactiveValues()
    
    #if discretized selected, then populate rv element with df_cut() & update summary table
    observeEvent(input[[paste("btn_dis", var(), sep="_")]], {
      rv_dis[[var()]] <- df_cut() 
    })
    
    #if no discretization selected, then populate with pass id & update summary table
    observeEvent(input[[paste("btn_not_dis", var(), sep="_")]], {
      rv_dis[[var()]] <- df_train %>% select(passenger_id)
    })
    
    
    ### Convert rv_dis to df_dis by running an inner_join
    df_train_nd_nvI_d <- eventReactive(input$btn_dis_complete, {
        
      print("Button Clicked - Inside eventReactive")
        
      rv_dis %>%
        reactiveValuesToList() %>%
        reduce(inner_join, .init=df_train %>% select(passenger_id)) %>%
        {if(sum(names(.)=="transported") > 0)
          relocate(., transported, .after=last_col()) else .}
      
    })
    
    
    ### User feedback: display text of discretization completed
    output$text_btn_dis_complete <- renderText({
      req(df_train_nd_nvI_d())
      confirm_discretization_msg(df_train_nd_nvI_d())
    })
    
    
    ### Checks
    #### Check creation of rv_dis
    output$check_rv <- renderPrint({
      rv_dis %>%
        reactiveValuesToList()
    })
    
    
    #### Check creation of reactiveValues & joining within elements
    output$temp_table_hist <- renderTable({
      
      df_train_nd_nvI_d() %>%
        head()
      
      })
    
    
    
    ## Return DF--------------------
    return(df_train_nd_nvI_d)
  })
}
  
    
    
    
    
    
    
    
