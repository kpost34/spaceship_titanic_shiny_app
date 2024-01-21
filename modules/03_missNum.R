# Missingness-num Module


# UI================================================================================================
missNumUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title="Cabin Number",
    titlePanel("num (Door Number) and Missingness"),
    sidebarLayout(
      sidebarPanel(
        h3("Convert door number to floor or floor ranges"),
        #visualize binning options
        accordion(id=ns("accordion1"),
          accordionItem(
            title="Click for more details",
            h4(chr_1_missNum)
          )
        ),
        sliderInput(ns("slid_num_bin"), label="Select number of floors per bin", 
                    min=1, max=4, value=1),
        
        #submit option
        actionButton(ns("btn_num_bin"), "Confirm selection", class="btn-primary")
      ),
      
      mainPanel(
        DTOutput(ns("tab_num_bin")),
        linebreaks(2),
        plotOutput(ns("plot_num_bin")),
      )
    )
  )
}





# Server============================================================================================
missNumServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Exploring floor ranges-------------------
    ### Create reactive object (for tabular output)
    dat1 <- reactive({
      req(input$slid_num_bin)
      #bin num into floor_num
      group_floors(df_train, input$slid_num_bin)
    })
    
  
    ### Output table/plot
    #### Table output
    output$tab_num_bin <- renderDT(
      dat1() %>%
        tabyl(floor_num, transported),
      rownames=FALSE,
      options=list(dom="tp",
                   autoWidth=TRUE,
                   pageLength=5,
                   #center-justifies column header and text
                   columnDefs=list(list(className='dt-center', targets="_all"))),
      #creates a caption above table in large, black text
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;  font-size:150% ;",
        paste0("transported by floor_num (", input$slid_num_bin, "-floor groups)"))
    )
    
    
    #### Plot output
    output$plot_num_bin <- renderPlot({
      dat1() %>%
        barplotter2(var="floor_num", title=FALSE)
    })
    
    
    
    ## Confirm selection-------------------
    ### Trigger toast notifications
    observeEvent(input$btn_num_bin, {
      if(input$slid_num_bin %in% 1:4) {
        show_toast(
          title="Floor variable",
          type="success",
          text=create_floor_num_msg(input$slid_num_bin),
          position="center",
          timer=3500
      )} 
    })
    
    
    ### Create new reactive DF to be exported
    df_train_nd <- eventReactive(input$btn_num_bin, {
      dat1()
    })
    
    
    
    ## Return DF--------------------
    return(df_train_nd)
    
  })
}





