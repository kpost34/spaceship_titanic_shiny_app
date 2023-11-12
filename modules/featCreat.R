#Feature Engineering-Feature Creation Module

# UI================================================================================================
featCreatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Feature Creation",id="creFea04",
    titlePanel(title="Feature Creation"),
    h4("Now you have the opportunity to create new features for your model using the existing variables. Let's look
       at some possible options"),
    #inputs
    wellPanel(
      fluidRow(
        column(5,
          #select input for group size
          selectInput01(id="sel_exp1_creFea04",label="Create a group size variable that uses...",
                        choices=ch_grp_size_featCreat)
        ),
        column(2,
          linebreaks(5),    
          uiOutput("ui_btn_cfirm_creFea04")
        ),
        column(3,
          #select input for luxury expenses
          selectizeInput(inputId="sel_exp2_creFea04",label="Create a luxury expense variable that uses the sum of",
                        multiple=TRUE,choices=c("Choose at least two variables"="",ch_lux_featCreat)),
        ),
        column(2,
          actionButton(inputId="btn_exp2_creFea04",label="Visualize results")
        )
      ),
      #vertically aligns button with selectizeInput
      tags$style(type="text/css", "#btn_exp2_creFea04 {width: 100%; margin-top: 25px;}")
    ),
    #outputs
    fluidRow(
      column(6,
        plotOutput("plot_sel_exp1a_creFea04"),
        plotOutput("plot_sel_exp1b_creFea04")
      ),
      column(6,
        plotOutput("plot_sel_exp2a_creFea04"),
        br(),
        plotOutput("plot_sel_exp2b_creFea04"),
        #temporary table--previews data after confirming selections
        tableOutput("plot_temp_table_creFea04")
      )
    )
  )
}

# Server============================================================================================
featCreatServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Plot outputs
    ### Group size variable
    #### Create a reactive data frame based on input
    dat1_creFea04<-reactive({
      req(input$sel_exp1_creFea04)
      switch(input$sel_exp1_creFea04,
             ticket_group_size=trainDF_nvI() %>%
              group_by(passenger_group) %>%
              mutate(ticket_group_size=n(),
                    ticket_group_size=as.factor(ticket_group_size)) %>%
               ungroup(),
             family_size=trainDF_nvI() %>%
               group_by(passenger_group,l_name) %>%
               mutate(family_size=n(),
                      family_size=as.factor(family_size)) %>%
               ungroup(),
             travel_party_size=trainDF_nvI() %>%
               group_by(cabin) %>% 
               mutate(travel_party_size=n(),
                      travel_party_size=as.factor(travel_party_size)) %>%
               ungroup()
        )
    })
    
    #### Make bar plots using new df
    output$plot_sel_exp1a_creFea04<-renderPlot({
      #require that user does not select "none" to get plots
      req(input$sel_exp1_creFea04 %in% ch_grp_size_featCreat[ch_grp_size_featCreat!="none"])
      dat1_creFea04() %>%
        barplotter(input$sel_exp1_creFea04)
    })
  
    output$plot_sel_exp1b_creFea04<-renderPlot({
      #require that user does not select "none" to get plots
      req(input$sel_exp1_creFea04 %in% ch_grp_size_featCreat[ch_grp_size_featCreat!="none"])
      dat1_creFea04() %>%
        barplotter(c(input$sel_exp1_creFea04,"transported"))
    })
    
    
    ### Luxury expense variable
    #### Create reactive df for plotting and feature creation
    dat2_creFea04<-reactive({
      req(input$sel_exp2_creFea04 %in% ch_lux_featCreat[ch_lux_featCreat!="none"])
      lux_builder(trainDF_nvI(),input$sel_exp2_creFea04)
    })
    
    
    #### Display plots after action button depressed
    observeEvent(input$btn_exp2_creFea04, {
      output$plot_sel_exp2a_creFea04<-renderPlot({
        heatmapper(dat2_creFea04(),input$sel_exp2_creFea04) 
      })
      output$plot_sel_exp2b_creFea04<-renderPlot({
        boxplotter2(dat2_creFea04())
      })
    })
    
    
    ## Feature creation
    ### Display action button to confirm selections dynamically
    output$ui_btn_cfirm_creFea04<-renderUI({
      req(input$sel_exp1_creFea04)
      req(input$sel_exp2_creFea04=="none"|length(input$sel_exp2_creFea04)>=2)
      req(input$btn_exp2_creFea04)
      actionButton(inputId="btn_cfirm_creFea04","Confirm feature creation selections")
    })
    
    ### Create features
    trainDF_nvI_ecF<-eventReactive(input$btn_cfirm_creFea04,{
      dat1_creFea04() %>%
        select(passenger_id,ends_with("size")) %>%
        left_join(dat2_creFea04() %>%
                    select(passenger_id,luxury),by="passenger_id")
    }
    )
    
    output$plot_temp_table_creFea04<-renderTable({
      head(trainDF_nvI_ecF())
    })
    
    
  })
}



















