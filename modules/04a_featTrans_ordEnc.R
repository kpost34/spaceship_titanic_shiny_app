#Feature Engineering-Transformation Module: Ordinal Encoding Submodule


# UI================================================================================================
featTrans_ordEncUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel( 
      selectInput01(ID=ns("sel_var_viz"),label=varViz_feat,
                    choices=fct_nonumVars),
      linebreaks(2),
      radioButtons(inputId=ns("rad_ordEnc"),label="Would you like to perform ordinal encoding on any of
                   the variables?",choices=c("Yes","No"),selected=character(0)),
      linebreaks(2),
      h4(textOutput(ns("text_ordEnc"))),
      #produces a list of checkboxes and selectors
      map(labs, split_chk_sel_builder, fn=ns),
      # ui_splits,
      uiOutput(ns("ui_btn_ordEnc_complete"))
    ),
    mainPanel(
      plotOutput(ns("plot_sel_var_viz")),
      linebreaks(2),
      htmlOutput(ns("text_sel_var_viz")),
      tableOutput(ns("temp_table"))
    )
  )
}



# Server============================================================================================
featTrans_ordEncServer <- function(id, df_train_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Inputs--------------------
    ### Dynamically display text above checkboxes below
    output$text_ordEnc<-renderText({
      req(input$rad_ordEnc=="Yes") 
      
      paste("Check each variable for ordinal encoding and rank the categories from least to most important")
    })
    
    
    ### Dynamically create checkboxes to choose variables for ordinal encoding
    output$ui_chk_ordEnc2a<-renderUI({
      req(input$rad_ordEnc=="Yes")
      checkboxInput(inputId=ns("chk_ordEnc2a"),label="ticket",value=FALSE)
    })
    output$ui_chk_ordEnc2b<-renderUI({
      req(input$rad_ordEnc=="Yes")
      checkboxInput(inputId=ns("chk_ordEnc2b"),label="home_planet",value=FALSE)
    })
    output$ui_chk_ordEnc2c<-renderUI({
      req(input$rad_ordEnc=="Yes")
      checkboxInput(inputId=ns("chk_ordEnc2c"),label="deck",value=FALSE)
    })
    output$ui_chk_ordEnc2d<-renderUI({
      req(input$rad_ordEnc=="Yes")
      checkboxInput(inputId=ns("chk_ordEnc2d"),label="side",value=FALSE)
    })
    output$ui_chk_ordEnc2e<-renderUI({
      req(input$rad_ordEnc=="Yes")
      checkboxInput(inputId=ns("chk_ordEnc2e"),label="destination",value=FALSE)
    })
  
      
    ### Dynamically create selectors for ordinal encoding
    output$ui_sel_ordEnc2a<-renderUI({
      req(input$chk_ordEnc2a)
      selectizeInput(inputId=ns("sel_ordEnc2a"),label="", multiple=TRUE,
                    choices=c(varSelOrd_feat,
                            df_train_nvI()[["ticket"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2b<-renderUI({
      req(input$chk_ordEnc2b)
      selectizeInput(inputId=ns("sel_ordEnc2b"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["home_planet"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2c<-renderUI({
      req(input$chk_ordEnc2c)
      selectizeInput(inputId=ns("sel_ordEnc2c"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["deck"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2d<-renderUI({
      req(input$chk_ordEnc2d)
      selectizeInput(inputId=ns("sel_ordEnc2d"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["side"]] %>% levels()))
    })
    
    output$ui_sel_ordEnc2e<-renderUI({
      req(input$chk_ordEnc2e)
      selectizeInput(inputId=ns("sel_ordEnc2e"),label="", multiple=TRUE,
                     choices=c(varSelOrd_feat,
                               df_train_nvI()[["destination"]] %>% levels()))
    })
    
    
    ### Dynamically display button
    output$ui_btn_ordEnc_complete<-renderUI({
      n_ticket<-nlevels(df_train_nvI()[["ticket"]])
      n_home_planet<-nlevels(df_train_nvI()[["home_planet"]])
      n_deck<-nlevels(df_train_nvI()[["deck"]])
      n_side<-nlevels(df_train_nvI()[["side"]])
      n_destination<-nlevels(df_train_nvI()[["destination"]])
      
      #button displays if 1) "No" selected in radio button; 2) at least one box is checked AND for each var either
        #1) box unchecked or all categories selected 
      req(input$rad_ordEnc=="No"|(
        sum(length(input$sel_ordEnc2a)==n_ticket,
            length(input$sel_ordEnc2b)==n_home_planet, 
            length(input$sel_ordEnc2c)==n_deck,
            length(input$sel_ordEnc2d)==n_side,
            length(input$sel_ordEnc2e)==n_destination) > 0 & (
        (length(input$sel_ordEnc2a)==n_ticket|input$chk_ordEnc2a==FALSE) &
        (length(input$sel_ordEnc2b)==n_home_planet|input$chk_ordEnc2b==FALSE) &
        (length(input$sel_ordEnc2c)==n_deck|input$chk_ordEnc2c==FALSE) &
        (length(input$sel_ordEnc2d)==n_side|input$chk_ordEnc2d==FALSE) &
        (length(input$sel_ordEnc2e)==n_destination|input$chk_ordEnc2e==FALSE)
        )
        )
      )
      actionButton(inputId=ns("btn_ordEnc_complete"),
                   label="Confirm all ordinal encoding selections",
                   class="btn-success")
    })
    
    
    ## Outputs--------------------
    ### Display plot
    output$plot_sel_var_viz<-renderPlot({
      req(input$sel_var_viz)
      barplotter(df_train_nvI(), input$sel_var_viz)
    })
    
    
    ### Display text associated with each variable 
    output$text_sel_var_viz<-renderUI({
      req(input$sel_var_viz)
      switch(input$sel_var_viz,
        ticket=HTML("<i>passenger_id</i> is broken into two parts: <i>passenger_group</i> (the first four digits) and 
          <i>ticket</i> (the last two digits). The 'ticket' component indicates the number/position within a passenger group."),
        home_planet=HTML("<i>home_planet</i> represents the planet that the passenger left, which is often where they live. 
          There are three home planets in this data set: <br>
          <b>Earth</b>: third planet from the Sun <br>
          <b>Mars</b>: fourth planet fromt the Sun <br>
          <b>Europa</b>: smallest of the four Galilean moons orbiting Jupiter"),
        deck=HTML("<i>deck</i> is one of three components of the variable <i>cabin</i> along with <i>num</i> and <i>side</i>.
          There are eight different decks: <b>A</b>-<b>G</b> and <b>T</b>"),
        side=HTML("<i>side</i> is one of three components of the varible <i>cabin</i> along with <i>deck</i> and <i>num</i>.
          <i>side</i> can take on one of two values: <b>P</b> for port and <b>S</b> for starboard."),
        destination=HTML("<i>destination</i> represents the planet to which the passenger is traveling. There are three
          possible destinations: <br>
          <b>TRAPPIST-1e</b>: a rocky, near-Earth-sized exoplanet that researchers consider as potentially habitable by
            humans <br>
          <b>55 Cancri e</b>: an exoplanet nearly 9x the mass of the Earth with an atmosphere composed of at least hydrogen
            and helium <br>
          <b>PSO J318.5-22</b>: a rogue planet with estimated temperatures of its clouds exceed 800 <sup>o</sup> C")
      )
    })
    
    
    ## Export--------------------
    ### Create reactive data frame
    df_train_nvI_o <- eventReactive(input$btn_ordEnc_complete, {
      df_train_nvI() %>%
        #choose all factors except num
        mutate(across(.cols=all_of(fct_nonumVars), ~as.ordered(.x))) %>%
          #if...else statements for whether to change factor level order based on if checkbox checked
          {if(input$chk_ordEnc2a==TRUE)
            mutate(.,ticket_ord=fct_relevel(ticket,input$sel_ordEnc2a))
            else .} %>%
          {if(input$chk_ordEnc2b==TRUE)
            mutate(.,home_planet_ord=fct_relevel(home_planet,input$sel_ordEnc2b))
            else .} %>%
          {if(input$chk_ordEnc2c==TRUE)
            mutate(.,deck_ord=fct_relevel(deck,input$sel_ordEnc2c))
            else .} %>%
          {if(input$chk_ordEnc2d==TRUE)
            mutate(.,side_ord=fct_relevel(side,input$sel_ordEnc2d))
            else .} %>%
           {if(input$chk_ordEnc2e==TRUE)
            mutate(.,destination_ord=fct_relevel(destination,input$sel_ordEnc2e))
            else .} %>%
        #retain passenger_id and mutated cols
        select(passenger_id,ends_with("_ord"))
    })
    
    
    ### Print temp table as a check
    output$temp_table<-renderTable({
      df_train_nvI_o() %>% head()
    })
    
    
    ### Return obj
    return(df_train_nvI_o)


    
  })
}