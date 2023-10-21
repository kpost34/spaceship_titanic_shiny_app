# Missingness Module

# UI
missingUI <- function(id) {
  navbarMenu(title="Missingness",menuName="Mis03",
    #tab 1: names-----------------------------------------------------------------------------------------------------------------
    tabPanel(title="Names", id="namMis03",
      titlePanel("Names and Missingness"),
      sidebarLayout(
        sidebarPanel(
          #exploring missing names
          h4("Did you notice that some passengers did not have names? If not, take a closer look"),
          selectInput01(id="sel_exp_namMis03",label="",choices=namMis03_expVec),
          br(),
          #go deeper with some possibilities
          h4("Two hundred out of 8693 passengers (in the training data) lack names. That's 2.3%. Although first names, and thus
             full names will be impossible to impute from the other variables. Last names may be populated with confidence if we
             assume passengers traveled together as families. Two ways to conclude that the traveling party is a family is
             1) purchasing tickets together (same passenger group) or 2) saying in the same room (cabin). Here's how the patterns break down."),
          radioButtons(inputId="rad_grpVar_namMis03",label="",choices=c("passenger_group"="passenger_group",
                                                                        "cabin occupancy"="cabin"),
                       selected=character(0)),
          h4("Note that each group, regardless of group size or grouping variable, has one unnamed passenger."),
          br(),
          h4("Given all this information, how would you like to handle passengers with missing names?"),
          selectInput01(id="sel_impOpt_namMis03",label="",choices=namMis03_impOptVec),
          br(),
          uiOutput("ui_slid_impOpt_namMis03")
        ),
        mainPanel(
          htmlOutput("text_sel_exp_namMis03"),
          DTOutput("tab_sel_exp_namMis03"),
          plotOutput("plot_sel_exp_namMis03"),
          br(),
          htmlOutput("text_rad_grpVar_namMis03"),
          plotOutput("plot_rad_grpVar_namMis03"),
          tableOutput("test_table"),
          tableOutput("test_table2")
        )
      )
    ),
    #tab 2: exploring non-character missingness-----------------------------------------------------------------------------------
    tabPanel(title="Explore Missingness",id="nchrMis03",
      titlePanel("Exploring Other Missing Data"),
      sidebarLayout(
        sidebarPanel(
          h4("Let's visualize missingness in all non-character variables."),
          selectInput01(id="sel_exp_nchrMis03",label="",choices=nchrMis03_expVec),
          br(),
          h4("Which variable pairs exhibit missingness at random (MAR)?. Compare each variable with missing data to the
          remaining set of variables."),
          selectInput01(id="sel_compare_nchrMis03",label="",choices=trainDF_nchrPreds),
          br(),
          #selectInput01(id="sel_imp_nchrMis03",label="",choices=)
        ),
        mainPanel(
          htmlOutput("text_sel_exp_nchrMis03"),
          plotOutput("plot_sel_exp_nchrMis03"),
          br(),
          htmlOutput("text_sel_compare_nchrMis03"),
          DTOutput("tab_sel_compare_nchrMis03")
        )
      )
    )
  )
}


# Server
# missingServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
# 
#   
#   
#   
#   
#   
#   
# }




