#Data Partitioning Module

# UI-===============================================================================================
dataPartUI <- function(id) {
  ns <- NS(id)
  
sidebarLayout(
    ## Input sliders for v-fold cross-validation with confirm button
    sidebarPanel(width=2,
      sliderInput(ns("slid_v"), label="Number of partitions", min=5, max=10, value=5),
      linebreaks(2),
      sliderInput(ns("slid_repeats"), label="Number of repeats", min=1, max=10, value=5),
      
    ),
    
    ## Tabular outputs
    mainPanel(width=10,
      DTOutput(ns("tab_analysis")),
      br(),
      DTOutput(ns("tab_assessment"))
    )
  )
}


# Server============================================================================================
featSelServer <- function(id, df_train_select) {
  moduleServer(id, function(input, output, session) {
    
    ## Apply data partitioning to DF--------------------
    df_vfold <- reactive({df_train_select() %>%
      vfold_cv(v=input$slid_v, repeats=input$slid_repeats, strat=transported)
    })
    
    
    
    ## Generate 
    
    
    ## Return DF--------------------
    return(df_vfold)
    
    
    #here's sample code for displaying in tables
    split1 <- get_rsplit(df_vfold, index=1)
    tidy(split1)
    analysis(split1)
    assessment(split1)
    
    
  })
}
