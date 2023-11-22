#Feature Engineering-Transformation Module: Discretization Submodule

# UI================================================================================================
featTrans_disUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      #ui for histogram
      h4("Histogram of raw data"),
      selectInput01(ID=ns("sel_var_hist"), label=varViz_feat, choices=disVars),
      # uiOutput(ns("ui_sel_var_hist")),
      radioButtons(inputId=ns("rad_log_hist"),
                   label="Choose whether to log10-scale the x-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE),
      # uiOutput(ns("ui_rad_log_hist")),
      numericInput(inputId=ns("num_bin_hist"),
                   label="Select the number of bins for the histogram (2-50)",
                   value=10,min=2,max=50),
      # uiOutput(ns("ui_num_bin_hist")),
      br(),
      fluidRow(
        column(9,
          # h5(strong("Do not discretize this variable"))
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
      # h4(textOutput(ns("text_bar"))),
      radioButtons(inputId=ns("rad_log_bar"),
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes", "No"), selected=character(0), inline=TRUE),
      # uiOutput(ns("ui_rad_log_bar")),
      numericInput(inputId=ns("num_brk_bar"),
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2, min=1, max=5),
      # uiOutput(ns("ui_num_brk_bar")),
      radioButtons(inputId=ns("rad_bdry_bar"),
                   label="How should data be binned?",
                   choices=ch_bin_opt_featTrans,
                   selected=character(0), inline=TRUE),
      # uiOutput(ns("ui_rad_bdry_bar")),
      uiOutput(ns("ui_num_bdry_bar")),
      fluidRow(
        column(9,
          h5(strong(textOutput(ns("text_dis"))))
        ),
        column(3,
          # div(style="margin-bottom: 15px;",
              uiOutput(ns("ui_btn_dis"))
          # )
        )
      )
    ),
    
    mainPanel(
      #histogram
      plotOutput(ns("plot_sel_var_hist")),
      linebreaks(2),
      
      #barplot
      plotOutput(ns("plot_sel_dis2")),
      
      #temporary tables (to be removed)
      tableOutput(ns("temp_table_hist")),
      tableOutput(ns("temp_table_dis2")),
      tableOutput(ns("temp_table_dis3")),
      tableOutput(ns("temp_table_dis4")),
      tableOutput(ns("temp_table_dis5")),
      tableOutput(ns("temp_table_dis6")),
      tableOutput(ns("temp_table_dis7")),
      tableOutput(ns("temp_table_dis8"))
    )
  )
}



# Server============================================================================================
featTrans_disServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    ## Inputs--------------------
    ### Input to select var to visualize as histogram (for discretization)
    # output$ui_sel_var_hist<-renderUI({
    #   selectInput01(ID=ns("sel_var_hist"),label=varViz_feat,
    #                 #dynamically select numerical variables and num 
    #                 choices=disVars)
    # })
    
    ### Input to choose whether to log10-transform x-axis
    # output$ui_rad_log_hist<-renderUI({
    #   req(input$sel_var_hist)
    #   
    #   radioButtons(inputId=ns("rad_log_hist"),
    #                label="Choose whether to log10-scale the x-axis",
    #                choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    # })
    
    ### Input to choose number of bins for histogram
    # output$ui_num_bin_hist<-renderUI({
    #   req(input$sel_var_hist)
    #   
    #   # numericInput(inputId=ns("num_bin_hist"),
    #   #              label="Select the number of bins for the histogram (2-50)",
    #   #              value=10,min=2,max=50)
    # })
    
    ### Output to display text for next set of inputs
    # output$text_bar<-renderText({
    #   req(input$sel_var_hist,input$rad_log_hist)
    #   
    #   paste("Visualization of binned data")
    # })
    
    
    ### Input to select a log10-transformed y-axis
    # output$ui_rad_log_bar<-renderUI({
    #   req(input$sel_var_hist,input$rad_log_hist)
    #   
    #   radioButtons(inputId=ns("rad_log_bar"),
    #                label="Choose whether to log10-scale the y-axis",
    #                choices=c("Yes", "No"), selected=character(0),inline=TRUE)
    # })
    
    ### Input to choose number of breaks
    # output$ui_num_brk_bar<-renderUI({
    #   req(input$sel_var_hist,input$rad_log_hist)
    #   
    #   numericInput(inputId=ns("num_brk_bar"),
    #                label="Select the number of breaks to create data bins (1-5)",
    #                value=2,min=1,max=5)
    # })
    
    ### Input to choose whether to have R or user-selected bin boundaries
    # output$ui_rad_bdry_bar<-renderUI({
    #   req(input$sel_var_hist,input$rad_log_hist)
    #   
    #   radioButtons(inputId=ns("rad_bdry_bar"),
    #                label="How should data be binned?",
    #                choices=ch_bin_opt_featTrans,
    #                selected=character(0),inline=TRUE)
    # })
    
    
  
    ### Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
    output$ui_num_bdry_bar<-renderUI({
      req(input$rad_bdry_bar=="user")
      # req(input$rad_log_bar, input$rad_bdry_bar=="user")
      
      tags_num<-tagList()
      
      for(i in seq_len(input$num_brk_bar)){
        tags_num[[i]] <- numericInput(ns(paste0("n",i)), paste0("Break",i), min=0, value=NULL)
      }
      
      tags_num
    })
    
  
  
    ### Dynamically displays action buttons (and associated text) to discretize/not discretize variable 
    #display text for action buttons
    output$text_not_dis <- renderText({
      req(input$sel_var_hist %in% disVars)
      # req(!is.na(input$rad_log_hist))

      paste0("Do not discretize ", input$sel_var_hist,".")
    })
    
    output$text_dis <- renderText({
      #either "Equal Intervals" is selected or "User specifications" is selected and the number and 
        #every break point input is populated
      req(input$sel_var_hist %in% disVars,
            (!is.na(input$rad_log_bar) & input$rad_bdry_bar=="cut_int")|
              (input$rad_bdry_bar=="user" &  sum(!is.na(user_cuts()))==input$num_brk_bar)
      )
      
      paste("Discretize",input$sel_var_hist, "using these settings.")
    })
    
    #display buttons
    output$ui_btn_not_dis<-renderUI({
      req(input$sel_var_hist %in% disVars)
      # req(!is.na(input$sel_var_hist))
      # req(!is.na(input$rad_log_hist))
      
      #simplify button-generating code
      actionButton(ns(paste("btn_not_dis",
                            input$sel_var_hist,
                            sep="_")),
                   label="Confirm")
    })
    
    output$ui_btn_dis<-renderUI({
      req(input$sel_var_hist %in% disVars,
          (!is.na(input$rad_log_bar) & input$rad_bdry_bar=="cut_int")|
            (input$rad_bdry_bar=="user" &  sum(!is.na(user_cuts()))==input$num_brk_bar)
      )
      
      #simplify button-generating code
      actionButton(ns(paste("btn_dis",input$sel_var_hist,sep="_")),label="Confirm")
    })
    
    
    ## Outputs--------------------
    ### Plot raw data with fill=transported as histogram
    output$plot_sel_var_hist<-renderPlot({
      req(input$rad_log_hist)
      
      #simplify code
      histogrammer2(dat=df_train_nvI(),col=input$sel_var_hist,
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
        equal_cutter(dat=df_train_nvI(), col=input$sel_var_hist, n.breaks=input$num_brk_bar)
      
      } else if(input$rad_bdry_bar=="user") {
        user_cutter(dat=df_train_nvI(), col=input$sel_var_hist, break.vals=user_cuts())
        
      }
    })
    
    
    ### Generate plot
    output$plot_sel_dis2 <- renderPlot({
      req(df_cut())
      
      bin_plotter(dat=df_cut(), 
                  col=input$sel_var_hist, 
                  type=input$rad_bdry_bar, 
                  log_val=input$rad_log_bar)
    })
      
    
    
    ## Export-------------------- [will want to uncomment this code]
    ## Create a single df from one var discretized
    # df_train_nvI_d <- eventReactive(input[[paste("btn_dis", input$sel_var_hist, sep="_")]], {
    #   df_train_nvI %>%
    #     
    # }
    #   
    # })
  
    
    
    ### Create reactiveValues object and initialize with NULL values
    #df_train_dis_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,vr_deck=NULL,num=NULL)
    # df_train_dis_list<-reactiveValues()
    # df_train_dis_list<-vector(mode="list")
  
   
    
    # Update reactiveValues object with passenger_id values as a tibble when selector input is used
    # observeEvent(input$rad_trans, {
    #   
    # }
    # })
    
   # df_train_dis_list<-reactiveValues(age=NULL)
   #  
   #  df_train_dis_list$age<-reactive({df_train_nvI() %>% select(passenger_id)})
  
    # 
    # df_train_dis_list$room_service<-reactive({
    #   df_train_nvI() %>% select(passenger_id) 
    # })
    # 
    # df_train_dis_list$food_court<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$shopping_mall<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$spa<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$vr_deck<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
    # 
    # df_train_dis_list$num<-eventReactive(input$rad_trans, {
    #   df_train["passenger_id"]
    # })
  
    
  
    
    # df_train_dis_list<-reactiveValues(age=NULL)
    
    #df_train_dis_list$room_service<-tibble(x="test")
    
   # df_train_dis_list<-reactiveValues(age=df_train["passenger_id"],
   #                         room_service=df_train["passenger_id"])
   
                           # food_court=df_train["passenger_id"],
                           # shopping_mall=df_train["passenger_id"],
                           # spa=df_train["passenger_id"],
                           # vr_deck=df_train["passenger_id"],
                           # num=df_train["passenger_id"])
    
    # df_train_dis_list<-reactiveValues(age=NA,room_service=NA,food_court=NA,shopping_mall=NA,spa=NA,
    #                                  vr_deck=NA,num=NA)
    
    # Confirm discretization (for each variable)
    #age
    # df_train_dis_list$age<-eventReactive(input$btn_not_dis_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis_age, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    
  
    # observeEvent(input$sel_var_histe, {
    #   df_train_dis_list$age<-df_train %>% select(passenger_id)
    # })
    # 
    # observeEvent(input$btn_dis_age, {
    #   req(input$rad_bdry_bar)
    #   if(input$rad_bdry_bar=="R"){
    #     df_train_dis_list$age<-user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts())
    #   }
    #   else if(input$rad_bdry_bar=="me"){
    #     df_train_dis_list$age<-user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   }
    # })
    # 
    # 
    # observeEvent(input$sel_var_hist, {
    #   df_train_dis_list$room_service<-df_train %>% select(passenger_id)
    # })
    # 
    # observeEvent(input$btn_dis_room_service, {
    #   req(input$rad_bdry_bar)
    #   if(input$rad_bdry_bar=="R"){
    #     df_train_dis_list$room_service<-user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts())
    #   }
    #   else if(input$rad_bdry_bar=="me"){
    #     df_train_dis_list$room_service<-user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   }
    # })
    
    # df_train_dis_list$age_pi<-eventReactive(input$btn_not_dis_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis_age, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$age<-reactive({
    #   if(input$rad_trans=="Discretization" & !exists("rad_bdry_bar_trnsFea")){
    #     df_train_nvI() %>% select(passenger_id)}
    #   if(input$sel_var_hist=="age" & exists("rad_bdry_bar_trns_Fea04") && input$rad_bdry_bar=="R"){
    #     user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts())
    #     }
    #   if(input$sel_var_hist=="age" & exists("rad_bdry_bar_trns_Fea04") && input$rad_bdry_bar=="me"){
    #     user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #     }
    # })
  
    
  
    #room_service
    # df_train_dis_list$room_service<-eventReactive(list(input$rad_trans,input$btn_room_service), {
    #   rs1<-df_train["passenger_id"]
    #   rs2<- switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$room_service<-eventReactive(input$btn_room_service, {
      # switch(input$rad_bdry_bar,
      #        R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
      #        me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
      # )
    # })
    
    # reax_room_service<-eventReactive(input$btn_room_service, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    
    
    # #food_court
    # df_train_dis_list$food_court<-eventReactive(input$btn_food_court, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    # 
    # #shopping_mall
    # df_train_dis_list$shopping_mall<-eventReactive(input$btn_shopping_mall, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    # 
    # #spa
    # df_train_dis_list$spa<-eventReactive(input$btn_spa, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    # 
    # #vr_deck
    # df_train_dis_list$vr_deck<-eventReactive(input$btn_vr_deck, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    # 
    # #num
    # df_train_dis_list$num<-eventReactive(input$btn_num, {
    #   switch(input$rad_bdry_bar,
    #          R=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=R_cuts()),
    #          me=user_cutter(dat=df_train_nvI(),col=input$sel_var_hist,break.vals=user_cuts())
    #   )
    # })
    
    #train_list<-reactiveValuesToList(df_train_dis_list)
    
    
    
    
  # Combine all list elements into a new reactive DF
  
    
    # df_train_nvI_d<-eventReactive(input$btn_age,{
    #   df_train_nvI() %>%
    #     select(passenger_id) %>%
    #     {if(ncol(df_train_dis_list$age())==2)(left_join(.,df_train_dis_list$age(),by="passenger_id")) else .} #%>%
    #     {if(ncol(df_train_dis_list$room_service())==2)(left_join(.,df_train_dis_list$room_service(),by="passenger_id")) else .} %>%
    #     {if(ncol(df_train_dis_list$food_court())==2)(left_join(.,df_train_dis_list$food_court(),by="passenger_id")) else .} #%>%
    # #     # {if(input$btn_shopping_mall==1)(left_join(.,df_train_dis_list$shopping_mall(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_spa==1)(left_join(.,df_train_dis_list$spa(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_vr_deck==1)(left_join(.,df_train_dis_list$vr_deck(),by="passenger_id")) else .} %>%
    # #     # {if(input$btn_num==1)(left_join(.,df_train_dis_list$num(),by="passenger_id")) else .}
    # })
    
  
    
  
    
    
    # df_train_nvI_d<-reactive({
    #   reduce(list(df_train_dis_list$age,
    #               df_train_dis_list$room_service),
    # # # #               # df_train_dis_list$food_court(),
    # # # #               # df_train_dis_list$shopping_mall(),
    # # # #               # df_train_dis_list$spa(),
    # # # #               # df_train_dis_list$vr_deck(),
    # # #               # df_train_dis_list$num()),
    #          left_join,by="passenger_id")
    # })
    
    
  
    
    # df_train_nvI_d<-reactive({
    #   reduce(df_train_dis_list,left_join,by="passenger_id")
    # })
      
    # df_train_nvI_d<-reactive({
    #     df_train_dis_list$age %>%
    #     left_join(df_train_dis_list$room_service,by="passenger_id") #%>%
    #     # left_join(df_train_dis_list$food_court,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$shopping_mall,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$spa,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$vr_deck,by="passenger_id") %>%
    #     # left_join(df_train_dis_list$num,by="passenger_id")
    # })
      
        #purrr::when(sum(!is.na(df_train_dis_list$age()))>0 ~left_join(.,df_train_dis_list$age(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(df_train_dis_list$room_service()))>0 ~left_join(.,df_train_dis_list$room_service(),by="passenger_id"), ~.) %>%
        #purrr::when(sum(!is.na(df_train_dis_list$food_court()))>0 ~left_join(.,df_train_dis_list$food_court(),by="passenger_id"), ~.) #%>%
        #{if(!is.na(df_train_list$shopping_mall))(left_join(.,df_train_dis_list$shopping_mall(),by="passenger_id")) else .} #%>%
      # {if(!is.null(df_train_dis_list$spa()))(left_join(.,df_train_dis_list$spa(),by="passenger_id")) else .} %>%
      # {if(!is.null(df_train_dis_list$vr_deck()))(left_join(.,df_train_dis_list$vr_deck(),by="passenger_id")) else .} %>%
      # {if(!is.null(df_train_dis_list$num()))(left_join(.,df_train_dis_list$num(),by="passenger_id")) else .}
    #})
    
    
    # toy_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,
    #                   c               vr_deck=NULL,num=NULL)
    
    # 
    # df_train %>%
    #   select(passenger_id) %>%
    #   #left_join(df_train[c("passenger_id","age")],by="passenger_id")
    #   {if(is.null(samp))(left_join(.,df_train[c("passenger_id","age")],by="passenger_id")) else .} %>%
    #   {if(is.null(dog))(left_join(.,df_train[c("passenger_id","spa")],by="passenger_id")) else .}
    # 
    # df_train %>%
    #   select(passenger_id) %>%
    #   #left_join(df_train[c("passenger_id","age")],by="passenger_id")
    #   purrr::when(is.null(samp) ~left_join(.,df_train[c("passenger_id","age")],by="passenger_id"), ~.)
    
    #Convert reactiveValues to list
    #df_train_dis_list<-reactiveValuesToList(df_train_dis_list)
    
    
    
    # Check whether results working (by variable)
    #age
    # output$temp_table_hist<-renderTable({
    #   df_train_dis_list$age %>% head()
    # })
    # 
    # # # #room_service
    # output$temp_table_dis2<-renderTable({
    #   df_train_dis_list$room_service %>% head()
    # })
    # 
    # 
    # # #food_court
    # output$temp_table_dis3<-renderTable({
    #   df_train_dis_list$food_court() %>% head()
    # })
    # 
    # # #shopping_mall
    # output$temp_table_dis4<-renderTable({
    #   df_train_dis_list$shopping_mall() %>% head()
    # })
    # 
    # #spa
    # output$temp_table_dis5<-renderTable({
    #   df_train_dis_list$spa() %>% head()
    # })
    # 
    # #vr_deck
    # output$temp_table_dis6<-renderTable({
    #   df_train_dis_list$vr_deck() %>% head()
    # })
    # 
    # #num
    # output$temp_table_dis7<-renderTable({
    #   df_train_dis_list$num() %>% head()
    # })
    
    # output$temp_table_dis8<-renderTable({
    #   df_train_nvI_d() %>% head()
    # })
  
    
    # Connect reactive DFs together
    #list_dis<-vector(mode="list",length=7)
    # list_dis<-reactiveValuesToList(c(df_train_nvI_da(),df_train_nvI_dr(),df_train_nvI_df(),df_train_nvI_dm(),
    #                                          df_train_nvI_ds(),df_train_nvI_dv(),df_train_nvI_dn()))
    # 
    
   # df_train_nvI_d<-reactive({
   #   df_train_nvI_da() %>%
   #   left_join(df_train_nvI_dr(),by=passenger_id) %>%
   #     left_join(df_train_nvI_df(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dm(),by=passenger_id) %>%
   #     left_join(df_train_nvI_ds(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dv(),by=passenger_id) %>%
   #     left_join(df_train_nvI_dv(),by=passenger_id) %>%
   #     select(passenger_id,ends_with("dis"))
   # })
    
  
    
    #age - a
    #room_service - r
    #food_court - f
    #shopping_mall - m
    #spa - s
    #vr_deck - v
    #num - n

    
    
  })
}



