#Feature Engineering-Transformation Module: Ordinal Encoding Submodule


# UI================================================================================================
featTrans_ordEncUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel( 
      selectInput01(ID=ns("sel_var_viz"), label=varViz_feat,
                    choices=fct_nonumVars),
      linebreaks(2),
      radioButtons(inputId=ns("rad_ordEnc"), label="Would you like to perform ordinal encoding on any of
                   the variables?", choices=c("Yes","No"), selected=character(0)),
      linebreaks(2),
      h4(textOutput(ns("text_ordEnc"))),
      #produces a list of checkboxes and selectors
      map(labs, split_chk_sel_builder, fn=ns),
      # ui_splits,
      uiOutput(ns("ui_btn_ordEnc_complete")),
      strong(textOutput(ns("text_btn_ordEnc_complete")))
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
featTrans_ordEncServer <- function(id, df_train_nd_nvI) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Inputs--------------------
    ### Dynamically display text above checkboxes below
    output$text_ordEnc<-renderText({
      req(input$rad_ordEnc=="Yes") 
      
      paste("Check each variable for ordinal encoding and rank the categories from least to most important")
    })
    
    
    ### Dynamically create checkboxes to choose variables for ordinal encoding
    purrr::map2(.x=fct_nonumVars, .y=letters[1:5], function(x, y){
      
      output[[paste0("ui_chk_ordEnc2", y)]] <- renderUI({
        req(input$rad_ordEnc=="Yes")
        
        checkboxInput(inputId=ns(paste0("chk_ordEnc2", y)),
                      label=x, value=FALSE)
      })
    })
    
  
    ### Dynamically create selectors for ordinal encoding
    purrr::map2(.x=fct_nonumVars, .y=letters[1:5], function(x, y) {
      
      output[[paste0("ui_sel_ordEnc2", y)]] <- renderUI({
        req(input[[paste0("chk_ordEnc2", y)]],
            input$rad_ordEnc=="Yes")
      
        selectizeInput(inputId=ns(paste0("sel_ordEnc2", y)), label="", multiple=TRUE,
                       choices=c(varSelOrd_feat, df_train_nd_nvI()[[x]] %>% levels()))
      })
    })
    
    
    ### Dynamically display button
    output$ui_btn_ordEnc_complete<-renderUI({
      req(input$rad_ordEnc)
      
      actionButton(inputId=ns("btn_ordEnc_complete"),
                   label="Confirm all ordinal encoding selections",
                   class="btn-success")
    })
    
    
    
    ## Outputs--------------------
    ### Display plot
    output$plot_sel_var_viz<-renderPlot({
      req(input$sel_var_viz)
      barplotter(df_train_nd_nvI(), input$sel_var_viz)
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
    df_train_nd_nvI_o <- eventReactive(input$btn_ordEnc_complete, ignoreInit=TRUE, {

      n_ticket <- nlevels(df_train_nd_nvI()[["ticket"]])
      n_home_planet <- nlevels(df_train_nd_nvI()[["home_planet"]])
      n_deck <- nlevels(df_train_nd_nvI()[["deck"]])
      n_side <- nlevels(df_train_nd_nvI()[["side"]])
      n_destination <- nlevels(df_train_nd_nvI()[["destination"]])
      
      if(input$rad_ordEnc=="No") {
        df_train_nd_nvI() %>%
          select(passenger_id) -> tmp 
        
      } else if(input$rad_ordEnc=="Yes") {
        df_train_nd_nvI() %>%
          #choose all factors except num
          mutate(across(.cols=all_of(fct_nonumVars), ~as.ordered(.x))) %>%
            #if...else statements for whether to change factor level order based on if checkbox checked
            {if(input$chk_ordEnc2a==TRUE & length(input$sel_ordEnc2a)==n_ticket)
              mutate(.,ticket_ord=fct_relevel(ticket,input$sel_ordEnc2a))
              else .} %>%
            {if(input$chk_ordEnc2b==TRUE & length(input$sel_ordEnc2b)==n_home_planet)
              mutate(.,home_planet_ord=fct_relevel(home_planet,input$sel_ordEnc2b))
              else .} %>%
            {if(input$chk_ordEnc2c==TRUE & length(input$sel_ordEnc2c)==n_deck)
              mutate(.,deck_ord=fct_relevel(deck,input$sel_ordEnc2c))
              else .} %>%
            {if(input$chk_ordEnc2d==TRUE & length(input$sel_ordEnc2d)==n_side)
              mutate(.,side_ord=fct_relevel(side,input$sel_ordEnc2d))
              else .} %>%
             {if(input$chk_ordEnc2e==TRUE & length(input$sel_ordEnc2e)==n_destination)
              mutate(.,destination_ord=fct_relevel(destination,input$sel_ordEnc2e))
              else .} %>%
          #retain passenger_id and mutated cols
          select(passenger_id, ends_with("_ord")) -> tmp
      }
      
      return(tmp)
    })
    
    
    ### User feedback: display text of discretization completed
    output$text_btn_ordEnc_complete <- renderText({
      req(df_train_nd_nvI_o())
      confirm_ord_encoding_msg(df_train_nd_nvI_o())
    })
    
    ### Print temp table as a check
    output$temp_table<-renderTable({
      df_train_nd_nvI_o() %>% head()
    })
    
    
    
    ## Return DF--------------------
    return(df_train_nd_nvI_o)
    
  })
}