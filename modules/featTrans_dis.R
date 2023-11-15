#Feature Engineering-Transformation Module: Discretization Submodule

# UI================================================================================================
featTrans_disUI <- function(id) {
  ns <- NS(id)
  
  # tabPanelBody("Discretization",
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("ui_sel_dis1")),
        uiOutput(ns("ui_rad_dis1")),
        uiOutput(ns("ui_num_dis1")),
        br(),
        fluidRow(
          column(9,
            htmlOutput(ns("text_dis3a"))
          ),
          column(3,
            uiOutput(ns("ui_btn_dis3a"))
          )
        ),
        tags$style(type="text/css", "#ui_btn_dis3a {width: 100%; margin-top: 25px;}"),
        br(),
        htmlOutput(ns("text_dis2")),
        uiOutput(ns("ui_rad_dis2a")),
        uiOutput(ns("ui_num_dis2a")),
        uiOutput(ns("ui_rad_dis2b")),
        uiOutput(ns("ui_num_dis2b")),
        fluidRow(
          column(9,
            htmlOutput(ns("text_dis3b"))
          ),
          column(3,
            uiOutput(ns("ui_btn_dis3b"))
          )
        ),
        tags$style(type="text/css", "#ui_btn_dis3b {width: 100%; margin-top: 25px;}")
      ),
      mainPanel(
        plotOutput(ns("plot_sel_dis1")),
        plotOutput(ns("plot_sel_dis2")),
        tableOutput(ns("temp_table_dis1")),
        tableOutput(ns("temp_table_dis2")),
        tableOutput(ns("temp_table_dis3")),
        tableOutput(ns("temp_table_dis4")),
        tableOutput(ns("temp_table_dis5")),
        tableOutput(ns("temp_table_dis6")),
        tableOutput(ns("temp_table_dis7")),
        tableOutput(ns("temp_table_dis8"))
      )
    )
  # )
}



# Server============================================================================================
featTrans_disServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    ## Inputs
    ### Input to select var to visualize as histogram (for discretization)
    output$ui_sel_dis1<-renderUI({
      # req(input$rad_trans=="Discretization")
      selectInput01(ID=ns("sel_dis1"),label=varViz_feat,
                    #dynamically select numerical variables and num 
                    choices=disVars)
    })
    
    ### Input to choose whether to log10-transform x-axis
    output$ui_rad_dis1<-renderUI({
      req(input$sel_dis1)
      radioButtons(inputId=ns("rad_dis1"),
                   label="Choose whether to log10-scale the x-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    ### Input to choose number of bins for histogram
    output$ui_num_dis1<-renderUI({
      req(input$sel_dis1)
      numericInput(inputId=ns("num_dis1"),
                   label="Select the number of bins for the histogram (2-100)",
                   value=30,min=2,max=100)
    })
    
    ### Output to display text for next set of inputs
    output$text_dis2<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      h4("Visualization of binned data")
    })
    
    
    ### Input to select a log10-transformed y-axis
    output$ui_rad_dis2a<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      radioButtons(inputId=ns("rad_dis2a"),
                   label="Choose whether to log10-scale the y-axis",
                   choices=c("Yes"=TRUE,"No"=FALSE),selected=character(0),inline=TRUE)
    })
    
    ### Input to choose number of breaks
    output$ui_num_dis2a<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      numericInput(inputId=ns("num_dis2a"),
                   label="Select the number of breaks to create data bins (1-5)",
                   value=2,min=1,max=5)
    })
    
    ### Input to choose whether to have R or user-selected bin boundaries
    output$ui_rad_dis2b<-renderUI({
      req(input$sel_dis1,input$rad_dis1)
      radioButtons(inputId=ns("rad_dis2b"),
                   label="Choose who selects the bin boundaries",
                   choices=c("R","me"),selected=character(0),inline=TRUE)
    })
    
    
  
    
    
    ### Dynamically create numericInput UIs based on n.breaks entry and if bin boundaries set to "me"
    output$ui_num_dis2b<-renderUI({
      req(input$rad_dis2a,input$rad_dis2b=="me")
      tags_num<-tagList()
      for(i in seq_len(input$num_dis2a)){
        tags_num[[i]]<-numericInput(ns(paste0("n",i)),paste0("Break",i),min=0,value=NULL)
      }
      tags_num
    })
    
  
  
    ### Dynamically display action button (and associated text) to discretize variable 
    #display text for action buttons
    output$text_dis3a<-renderUI({
      req(!is.na(input$rad_dis1))
      h4(paste0("I am not interested in discretizing ",input$sel_dis1,"."))
    })
    
    output$text_dis3b<-renderUI({
      #either "R" is selected or "me" is selected and the number and every break point input is populated
      req((!is.na(input$rad_dis2a) & input$rad_dis2b=="R")|
         (input$rad_dis2b=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a)
      )
      h4(paste("Click confirm to discretize",input$sel_dis1, "using these settings."))
    })
    
    #display buttons
    output$ui_btn_dis3a<-renderUI({
      req(!is.na(input$rad_dis1))
      switch(input$sel_dis1,
        age=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        room_service=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        food_court=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        shopping_mall=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        spa=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        vr_deck=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm"),
        num=actionButton(ns(paste("btn_dis3a",input$sel_dis1,sep="_")),label="Confirm")
      )
    })
    
    output$ui_btn_dis3b<-renderUI({
      req((!is.na(input$rad_dis2a) & input$rad_dis2b=="R")|
            (input$rad_dis2b=="me" &  sum(!is.na(user_cuts()))==input$num_dis2a)
      )
      switch(input$sel_dis1,
        age=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        room_service=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        food_court=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        shopping_mall=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        spa=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        vr_deck=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm"),
        num=actionButton(ns(paste("btn_dis3b",input$sel_dis1,sep="_")),label="Confirm")
      )
    })
    
    
    ## Ouptuts
    ### Plot raw data with fill=transported as histogram
    output$plot_sel_dis1<-renderPlot({
      req(input$rad_dis1)
      switch(input$sel_dis1,
        age=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        room_service=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        food_court=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        shopping_mall=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        spa=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        vr_deck=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1),
        num=histogrammer2(dat=df_train_nvI(),col=input$sel_dis1,
          n.bins=input$num_dis1,x.log.scale=input$rad_dis1)
      )
    })
    
    
    ### Plot numerical var in bins filled by transported either using R or user specified break values
    #create reactive vectors of cut locations
    user_cuts<-reactive({
      c(input$n1,input$n2,input$n3,input$n4,input$n5)
    })
    
    R_cuts<-reactive({
      layer_scales(plot_dis())$x$breaks
    })
    
  
    
    #use switch to create plot object
    plot_dis<-reactive({
      req(input$rad_dis2a, input$rad_dis2b)
      switch(input$rad_dis2b,
             R=bin_plotter(dat=df_train_nvI(),col=input$sel_dis1,num.breaks=input$num_dis2a,
                           y.log.scale=input$rad_dis2a),
             me=user_bin_plotter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts(),
                                 y.log.scale=input$rad_dis2a)
      )
    })
    
    #plot the plot object
    output$plot_sel_dis2<-renderPlot({
      plot_dis()
    })
    
    
    
    ### Create reactiveValues object and initialize with NULL values
    #df_train_dis_list<-reactiveValues(age=NULL,room_service=NULL,food_court=NULL,shopping_mall=NULL,spa=NULL,vr_deck=NULL,num=NULL)
    # df_train_dis_list<-reactiveValues()
    df_train_dis_list<-vector(mode="list")
  
   
    
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
    # df_train_dis_list$age<-eventReactive(input$btn_dis3a_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
  
    observeEvent(input$sel_dis1e, {
      df_train_dis_list$age<-df_train %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_age, {
      req(input$rad_dis2b)
      if(input$rad_dis2b=="R"){
        df_train_dis_list$age<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
      }
      else if(input$rad_dis2b=="me"){
        df_train_dis_list$age<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      }
    })
    
    
    observeEvent(input$sel_dis1, {
      df_train_dis_list$room_service<-df_train %>% select(passenger_id)
    })
    
    observeEvent(input$btn_dis3b_room_service, {
      req(input$rad_dis2b)
      if(input$rad_dis2b=="R"){
        df_train_dis_list$room_service<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
      }
      else if(input$rad_dis2b=="me"){
        df_train_dis_list$room_service<-cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      }
    })
    
    # df_train_dis_list$age_pi<-eventReactive(input$btn_dis3a_age, {
    #   df_train_nvI() %>% select(passenger_id)
    # })
    # 
    # 
    # df_train_dis_list$age<-eventReactive(input$btn_dis3b_age, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$age<-reactive({
    #   if(input$rad_trans=="Discretization" & !exists("rad_dis2b_trnsFea")){
    #     df_train_nvI() %>% select(passenger_id)}
    #   if(input$sel_dis1=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b=="R"){
    #     cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts())
    #     }
    #   if(input$sel_dis1=="age" & exists("rad_dis2b_trns_Fea04") && input$rad_dis2b=="me"){
    #     cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #     }
    # })
  
    
  
    #room_service
    # df_train_dis_list$room_service<-eventReactive(list(input$rad_trans,input$btn_room_service), {
    #   rs1<-df_train["passenger_id"]
    #   rs2<- switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    # df_train_dis_list$room_service<-eventReactive(input$btn_room_service, {
      # switch(input$rad_dis2b,
      #        R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
      #        me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
      # )
    # })
    
    # reax_room_service<-eventReactive(input$btn_room_service, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    
    
    # #food_court
    # df_train_dis_list$food_court<-eventReactive(input$btn_food_court, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #shopping_mall
    # df_train_dis_list$shopping_mall<-eventReactive(input$btn_shopping_mall, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #spa
    # df_train_dis_list$spa<-eventReactive(input$btn_spa, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #vr_deck
    # df_train_dis_list$vr_deck<-eventReactive(input$btn_vr_deck, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
    #   )
    # })
    # 
    # #num
    # df_train_dis_list$num<-eventReactive(input$btn_num, {
    #   switch(input$rad_dis2b,
    #          R=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=R_cuts()),
    #          me=cutter(dat=df_train_nvI(),col=input$sel_dis1,break.vals=user_cuts())
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
    output$temp_table_dis1<-renderTable({
      df_train_dis_list$age %>% head()
    })
  
    # # #room_service
    output$temp_table_dis2<-renderTable({
      df_train_dis_list$room_service %>% head()
    })
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



