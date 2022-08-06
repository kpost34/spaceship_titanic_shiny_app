#### Load packages
pacman::p_load(shiny,here,tidyverse,janitor,DT,visdat,finalfit,skimr,GGally,rstatix,conflicted)

#address conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("chisq.test","stats")


#### Load functions
source(here("functions and modals","spaceship_titanic_app_func_ui.R"))
source(here("functions and modals","spaceship_titanic_app_func_01.R"))


#### Read in and clean data
read_csv(here("data","train.csv")) %>%
  clean_names() %>%
  ### passenger_id
  separate(passenger_id,into=c("passenger_group","ticket"),sep="_",remove=FALSE) %>%
  ### cabin
  separate(cabin,into=c("deck","num","side"),sep="/",remove=FALSE) %>%
  ### name
  separate(name,into=c("f_name","l_name"),sep=" ",remove=FALSE) %>%
  ### reclassify vars
  mutate(across(c(home_planet,deck:destination),~as.factor(.x))) -> trainDF


#### Create vectors
### Col names
## All cols but character
trainDF %>% select(!where(is.character)) %>% names() -> trainDF_nchrVars

## All logical and factor cols
trainDF %>% select(where(is.logical)|where(is.factor)) %>% names() -> trainDF_catVars

## All numeric cols
trainDF %>% select(where(is.numeric)|where(is.integer)) %>% names() -> trainDF_numVars

## Dependent variable
depVar<-"transported"

## choices vectors
Chk01_quickVec<-c("dimensions"="dim","data sample"="dat_samp","missingness"="miss")
Chk01_summVec<-c("character"="chr","factor"="fct","logical"="lgl","numeric"="num")
namMis03_expVec<-c("missing example"="miss_samp","non-missing example"="nmiss_samp","summary table"="sum_tab","bar plot"="plot")
namMis03_impOptVec<-c("drop name columns"="drop_cols",
                      "remove with rows with missing names"="remove_rows",
                      "populate using passenger group"="imp_pass_group",
                      "populate using cabin info"="imp_cabin")
expMis03_expVec<-c("missing values occurence plot"="miss_occur",
                   "missing data pattern plot"="miss_patt")

#add modal if someone tries to select this--e.g., could be useful in feature engineering
#add modal if someone tries to select this--e.g., could be impacting rest of vars


#NAMING FORMULAS
#inputs: [abbrvInput]_[abbrvTask]_[Tabname]
#e.g., sel_quick_Chk01 (from Chk011 tab): sel = selectInput, quick = "Quick data check", chk01
#outputs: [abbrOutput]_[abbrvInput]_[# if 2+]_[tabName]
#e.g., tab_sel_Chk01 (from Chk01 tab): tab = table, sel = selectInput, chk01 (if 2 tables then append 1, 2, etc to before tabName)
#i.e., tab_[inputNameFormula]

ui<-navbarPage(title="Spaceship Titanic Shiny App", id="mainTab",position="static-top",
  #### 0: Title page and intro====================================================================================================
  # tabPanel(title="Intro",id="intro_00",
  #          numericInput(inputId = "numInput",label="pick a number",value=2),
  #          textOutput("textout1")
  #          )
    ##option to skip intro-> advances to tab/menu 1
    ## input name ... Hello, x. Your mission is....
    ##preview
  #### 1: Menu-Data Checking======================================================================================================
  tabPanel(title="Data Check",id="Chk01",
    sidebarLayout(
      sidebarPanel(width=3,
        selectInput01(id="sel_quick_Chk01",label="Quick data check",choices=Chk01_quickVec),
        linebreaks(2),
        selectInput01(id="sel_summ_Chk01",label="Data summaries",choices=Chk01_summVec)
      ),
      mainPanel(width=9,
        DTOutput("tab_sel_quick_Chk01"),
        br(),
        DTOutput("tab_sel_summ_Chk01")
      )
    )
  ),

  #### 2: Menu-EDA================================================================================================================
  navbarMenu(title="EDA",menuName="EDA02",
    #tab 1: univariate EDA--------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Univariate",tabID="uniEDA02",varID=c("var1","var2"),options=trainDF_nchrVars,fn=selectInput01),
    
    #tab 2: bivariate EDA---------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Bivariate",tabID="biEDA02",varID=c("var12","var34"),options=trainDF_nchrVars,fn=selectizeInput01),
    
    #tab 3: multivariate EDA-------------------------------------------------------------------------------------------------------
    edaTabBuilder(name="Multivariate",tabID="mulEDA02",varID=c("var123","var456"),options=trainDF_nchrVars,fn=selectizeInput02)
  ),


  #### 3: Menu-Missing Data=======================================================================================================
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
          tableOutput("test_table")
        )
      )
    ),
    #tab 2: exploring non-character missingness-----------------------------------------------------------------------------------
    tabPanel(title="Explore Missingness",id="expMis03",
      titlePanel("Exploring Other Missing Data"),
      sidebarLayout(
        sidebarPanel(
          h4("Let's visualize missingness in all non-character variables"),
          selectInput01(id="sel_exp_expMis03",label="",choices=expMis03_expVec)
        ),
        mainPanel(
          htmlOutput("text_sel_exp_expMis03"),
          plotOutput("plot_sel_exp_expMis03")
        )
      )
    )
  )

  
  #see sec 10.1.2 in Mastering Shiny to generate hierarchical select boxes
  
  # h4("Names may be populated"),
  # #options for handling missing names
  # selectInput01(id="sel_opt_namMis03",label="How would you like to handle missing names?",
  #               choices=misName03_optVec),

    
    
    
    
    
    # tabPanel(title="Overall missingness", id="03b_miss_miss",
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(inputId="03b_viz_miss_sel",
    #                   label="How would you like to visualize missingness?",
    #                   choices=c("overall"="overall",
    #                             "missing patterns"="patt")),
    #       selectInput(inputId="03b_stats_miss_sel",
    #                   label="Which variable would you like to test for MAR?",
    #                   choices="x" #SEE BOOK#)
    #       )
    #     ),
    #     mainPanel(
    #       plotOutput("03b_"),
    #       plotOutput("03b_"),
    #       tableOutput("03b_")
    #     )
    #   )
    # ),
    # tabPanel(title="Handle missingness",id="03c_miss_imp",
    #   sidebarLayout(
    #     sidebarPanel(),
    #     mainPanel()
    #   )
    # )


  # 
  # #### 4: Menu-Features=========================================================================================================
  # navbarMenu(title="Features",menuName="feat_04",
  #   tabPanel(title="Feature assessment", id="feat_assess_04a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Feature engineering",id="feat_eng_04b",
  #            sidebarLayout(
  #              sidebarPanel(),
  #              mainPanel()
  #            )
  #   )
  # ),
  # 
  # 
  # 
  # #### 5: Tab-Data Partitioning=================================================================================================
  # tabPanel(title="Data Partitioning",id="part_05",
  #   sidebarLayout(
  #     sidebarPanel(),
  #     mainPanel()
  #   )
  # ),
  # 
  # #### 6: Menu-Modeling=================================================================================================
  # navbarMenu(title="Modeling",menuName="mod_06",
  #   tabPanel(title="Model specification", id="mod_spec_06a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Fit models", id="mod_fit_06b",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model assessment", id="mod_assess_06c",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   )
  # ),
  # 
  # #### 7: Menu-Validation & Tuning===========================================================================================
  # navbarMenu(title="Validation & Tuning", menuName="valTune_07",
  #   tabPanel(title="Cross-validate",id="valTune_07a",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model tuning", id="valTune_mod_07b",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),  
  #   tabPanel(title="Model selection", id="valTune_sel_07c",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   ),
  #   tabPanel(title="Model assessment", id="valTune_assess_07d",
  #     sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel()
  #     )
  #   )
  # ),
  #   
  # #### 8: Tab-Model Testing============================================================================================
  # tabPanel(title="Testing",id="mod_test_08",
  #   sidebarLayout(
  #     sidebarPanel(),
  #     mainPanel()
  #   )
  # )
)



server<-function(input,output,session){
  
  #### Server 0: Intro================================================================================================================
  
  #### Server 1: Data Checking========================================================================================================
  ### Display dims, data sample, or missingness
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

  
  #### Server 2: EDA============================================================================================
  ### Univariate-------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var1_uniEDA02<-renderUI({
    h3(paste(input$sel_var1_uniEDA02))
  })
  
  output$text_sel_var2_uniEDA02<-renderUI({
    h3(paste(input$sel_var2_uniEDA02))
  })
  
  ## Table outputs
  # reactives of output tables
  dat1_uniEDA02<-reactive({
    if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
      summaryize(trainDF,input$sel_var1_uniEDA02)
    }
    else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
      tabylize(trainDF,input$sel_var1_uniEDA02)
    }
  })

  dat2_uniEDA02<-reactive({
    if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
      summaryize(trainDF,input$sel_var2_uniEDA02)
    }
    else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
      tabylize(trainDF,input$sel_var2_uniEDA02)
    }
  })
  
  # Output tables
  output$tab_sel_var1_uniEDA02<-renderDT(
    dat1_uniEDA02(),options=list(scrollX="400px",
                                pageLength=5)
  )

  output$tab_sel_var2_uniEDA02<-renderDT(
    dat2_uniEDA02(),options=list(scrollX="400px",
                                pageLength=5)
  )
  
  
  ## Plot outputs
  output$plot_sel_var1_uniEDA02<-renderPlot({
    if(input$sel_var1_uniEDA02 %in% trainDF_numVars){
      histogramer(trainDF,input$sel_var1_uniEDA02)
    }
    else if(input$sel_var1_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var1_uniEDA02)
    }
  })

  output$plot_sel_var2_uniEDA02<-renderPlot({
    if(input$sel_var2_uniEDA02 %in% trainDF_numVars){
      histogramer(trainDF,input$sel_var2_uniEDA02)
    }
    else if(input$sel_var2_uniEDA02 %in% trainDF_catVars){
      barplotter(trainDF,input$sel_var2_uniEDA02)
    }
  })
  
  
  ### Bivariate-------------------------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var12_biEDA02<-renderUI({
    h3(paste(input$sel_var12_biEDA02,collapse="-"))
  })
  
  output$text_sel_var34_biEDA02<-renderUI({
    h3(paste(input$sel_var34_biEDA02,collapse="-"))
  })
  
  ## Table outputs
  # Create reactives of output tables
  dat1_biEDA02<-reactive({
    req(length(input$sel_var12_biEDA02)==2)
    #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
    if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
      tabylize(trainDF,input$sel_var12_biEDA02)
    }
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
      summaryize(trainDF,input$sel_var12_biEDA02,input$sel_var12_biEDA02[input$sel_var12_biEDA02 %in% trainDF_catVars])
    }
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_numVars)==2) {
      corrtester(trainDF,input$sel_var12_biEDA02)
    }
  })
  
  
  dat2_biEDA02<-reactive({
    req(length(input$sel_var34_biEDA02)==2)
    #reactive (table) depends on type of input (i.e., cat-num, cat-cat, or num-num)
    if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
      tabylize(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
      summaryize(trainDF,input$sel_var34_biEDA02,input$sel_var34_biEDA02[input$sel_var34_biEDA02 %in% trainDF_catVars])
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_numVars)==2) {
      corrtester(trainDF,input$sel_var34_biEDA02)
    }
  })
  
  
  # Output tables
  output$tab_sel_var12_biEDA02<-renderDT(
    dat1_biEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
  )
  
  output$tab_sel_var34_biEDA02<-renderDT(
    dat2_biEDA02(),options=list(scrollX="400px",
                                  pageLength=5)
  )

  ## Plot outputs
  output$plot_sel_var12_biEDA02<-renderPlot({
    #must select two inputs first
    req(length(input$sel_var12_biEDA02)==2)
    #if both categorical, then bar plot
    if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==2) {
      barplotter(trainDF,input$sel_var12_biEDA02)
    }
    #if 1 cat & 1 num then boxplot
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==1) {
      boxplotter(trainDF,input$sel_var12_biEDA02)
    }
    #if two num then scatterplot
    else if(sum(input$sel_var12_biEDA02 %in% trainDF_catVars)==0) {
      scatterplotter(trainDF,input$sel_var12_biEDA02)
    }
  })
  
  output$plot_sel_var34_biEDA02<-renderPlot({
    req(length(input$sel_var34_biEDA02)==2)
    if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==2) {
      barplotter(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==1) {
      boxplotter(trainDF,input$sel_var34_biEDA02)
    }
    else if(sum(input$sel_var34_biEDA02 %in% trainDF_catVars)==0) {
      scatterplotter(trainDF,input$sel_var34_biEDA02)
    }
  })

  
  ### Multivariate-------------------------------------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_var123_mulEDA02<-renderUI({
    h3(paste(input$sel_var123_mulEDA02,collapse="-"))
  })
  
  output$text_sel_var456_mulEDA02<-renderUI({
    h3(paste(input$sel_var456_mulEDA02,collapse="-"))
  })
  
  
  ## Plot outputs
  output$plot_sel_var123_mulEDA02<-renderPlot({
    #must select three inputs first
    req(length(input$sel_var123_mulEDA02)==3)
    #if all categorical, then bar plot
    if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars)==3) {
      barplotter(trainDF,input$sel_var123_mulEDA02)
    }
    #if 2 cat & 1 num then boxplot
    else if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars)==2) {
      boxplotter(trainDF,input$sel_var123_mulEDA02)
    }
    #if 2-3 num then scatterplot
    else if(sum(input$sel_var123_mulEDA02 %in% trainDF_catVars) < 2) {
      scatterplotter(trainDF,input$sel_var123_mulEDA02)
    }
  })
  
  output$plot_sel_var456_mulEDA02<-renderPlot({
    req(length(input$sel_var456_mulEDA02)==3)
    if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars)==3) {
      barplotter(trainDF,input$sel_var456_mulEDA02)
    }
    else if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars)==2) {
      boxplotter(trainDF,input$sel_var456_mulEDA02)
    }
    else if(sum(input$sel_var456_mulEDA02 %in% trainDF_catVars) < 2) {
      scatterplotter(trainDF,input$sel_var456_mulEDA02)
    }
  })
  
  

  ##### Server 3: Missing Data====================================================================================================
  #### Names----------------------------------------------------------------------------------------------------------------------
  ### Exploring missing names
  ## Text outputs
  output$text_sel_exp_namMis03<-renderUI({
    #here switch() can be used with all four choices to display the appropriate name
    switch(input$sel_exp_namMis03,
      miss_samp=h3(paste("Sample of Passengers with Missing Names")),
      nmiss_samp=h3(paste("Sample of Passengers with Names")),
      sum_tab=h3(paste("Summary of Missing Names")),
      plot=h3(paste("Plot of Missing Names"))
    )
  })
  
  ## Create reactive object (for tabular output)
  dat1_namMis03<-reactive({
    #reactive is used to build reactive table objects
    switch(input$sel_exp_namMis03,
      miss_samp=trainDF %>% filter(is.na(name)) %>% slice_sample(n=5),
      nmiss_samp=trainDF %>% filter(!is.na(name)) %>% slice_sample(n=5),
      sum_tab=chr_miss_tabler(trainDF)
    )
  })
  

  ## Output table/plot
  # Table output
  output$tab_sel_exp_namMis03<-renderDT(
    dat1_namMis03(),options=list(scrollX="400px")
  )
  
  # Plot output
  output$plot_sel_exp_namMis03<-renderPlot({
    #plot outputs only when selected (note that this is always the same plot)
    req(input$sel_exp_namMis03=="plot")
    chr_miss_boxplotter(trainDF)
  })
  
  
  ### Understanding name missingness conditioned on other variables
  ## Text outputs
  output$text_rad_grpVar_namMis03<-renderUI({
    req(input$rad_grpVar_namMis03)
    switch(input$rad_grpVar_namMis03,
      passenger_group=h3(paste("Summary of Missing Names by Size of Passenger Groups")),
      cabin=h3(paste("Summary of Missing Names by Cabin Occupancy"))
    )
  })
  
  ## Create reactive object (for tabular and plot outputs)
  dat2_namMis03<-reactive({
    switch(input$rad_grpVar_namMis03,
      passenger_group=mis_name_tabler(trainDF,l_name,passenger_group),
      cabin=mis_name_tabler(trainDF,l_name,cabin)
    )
  })
  
  
  ## Output plots
  output$plot_rad_grpVar_namMis03<-renderPlot({
    req(input$rad_grpVar_namMis03)
    col_plotter(dat2_namMis03(),num_name,n)
  })
  
  
  ## Create dynamic UI to display sliders
  output$ui_slid_impOpt_namMis03<-renderUI({
    req(input$sel_impOpt_namMis03 %in% c("imp_pass_group","imp_cabin"))
    switch(input$sel_impOpt_namMis03,
      imp_pass_group=sliderInput("slid1_impOpt_namMis03",
                      "Select a range of named passengers per passenger_group to use for name imputation",
                      value=c(3,3),min=1,max=7),
      imp_cabin=sliderInput("slid2_impOpt_namMis03","Select a range of named passengers per cabin to use for name imputation",
                  value=c(3,3),min=1,max=6)
    )
  })
  
  
  ## Create reactive object (for creating a new DF)
  dat3_namMis03<-reactive({
    req(input$sel_impOpt_namMis03 %in% c("imp_pass_group","imp_cabin"))
    switch(input$sel_impOpt_namMis03,
      imp_pass_group=mis_name_tabler(trainDF,l_name,passenger_group),
      imp_cabin=mis_name_tabler(trainDF,l_name,cabin)
    )
  })
  
  ## Create new data frame object after name imputation or col/row removal
  trainDF_nI<-reactive({
    #requires selection from drop-down menu
    req(input$sel_impOpt_namMis03)
    #dplyr code if drop_cols selected
    if(input$sel_impOpt_namMis03=="drop_cols"){
      trainDF %>% select(-contains("name"))
    }
    #same for remove_rows
    else if(input$sel_impOpt_namMis03=="remove_rows"){
      trainDF %>% filter(!is.na("name"))
    }
    #if imp_pass_groups chosen and slider input values chosen then name_imputer() runs
    else if(input$sel_impOpt_namMis03=="imp_pass_group" & length(input$slid1_impOpt_namMis03)>0){
      name_imputer(dat3_namMis03(),num_name,input$slid1_impOpt_namMis03,trainDF,passenger_group)
    }
    else if(input$sel_impOpt_namMis03=="imp_cabin" & length(input$slid2_impOpt_namMis03)>0){
      name_imputer(dat3_namMis03(),num_name,input$slid2_impOpt_namMis03,trainDF,cabin)
    }
  })
  
  ## Test whether code above is working
  output$test_table<-renderTable({
    head(trainDF_nI()) 
  })
  
  
  #### Exploring Non-Character Missingness----------------------------------------------------------------------------------------
  ## Text outputs
  output$text_sel_exp_expMis03<-renderUI({
    switch(input$sel_exp_expMis03,
           miss_occur=
           miss_patt=
             
           miss_samp=h3(paste("Sample of Passengers with Missing Names")),
           nmiss_samp=h3(paste("Sample of Passengers with Names")),
           sum_tab=h3(paste("Summary of Missing Names")),
           plot=h3(paste("Plot of Missing Names"))
    )
  })
  
  
  sidebarPanel(
    h4("Let's visualize missingness in all non-character variables"),
    selectInput01(id="sel_exp_expMis03",label="",choices=expMis03_expVec)
  ),
  mainPanel(
    htmlOutput("text_sel_exp_expMis03"),
    plotOutput("plot_sel_exp_expMis03")
  
  
  #Server 4: Features========================================================================================================
  
  #Server 5: Data Partitioning==========================================================================================

  
  #Server 6: Modeling===================================================================================================
  
  #Server 7: Validation & Tuning========================================================================================

  
  #Server 8: Model Testing==============================================================================================
  
  
}

shinyApp(ui,server)



#------------------------------------------------
## NEED TO...
# split up backbone code into 01 and 02
# create another function script with a server suffix (for more 'structural' functions) & create functions

#--------------------

## DONE
# re-organized the third main tab into a missing data tab and have name as the first 'subtab'

## IN PROGRESS
# working on missingness of non-chr var exploration


#---------------------

## TO DO
#spacing between tables and plots
#text size on plots (e.g., axes)
#add table titles--perhaps to correlation table
#deal with all the variable num categories
#update edaTabBuilder code to make dt outputs optional (to adjust for mult)
#ability to bin choices? (vars into factor, logical, etc)
#convert larger server 'patterns' to functions
#output of missing names submenu/tab is a new DF object...thus a user can skip to, but not past, this section
#for missing name tab--need to have the first output (plot or DT) output in the same area


#------------------------------------------------
#OUTLINE

### 2. Data imputation
## Assessed data for missingness and understand pattern of missingness
## imputate missing data
## Data check: data checked (e.g., ranges) to see if data make sense following imputation--consider figures here too (differentiating given)


### 3. Feature engineering
## Assess data for possible features
## Feature engineering: extracting "data" from cols to generate variables (e.g., extracting alpha prefixes from ticket #s)
## (Feature selection: vars of interest selected for epa)
## Another option to re-order cols

### 4. Data normalization/standardization

### 5. Data Paritioning: Divide training data into four subsamples for v-fold cross-validation


### 6. Modeling
## Create recipe (identify col as id var, predictor, or outcome)
## Specify models
## Construct workflow
## Fit models 
## Assess model accuracy


### 7. Validation and tuning
## Fit models to cross-validation folds
## Tune models
## Select best model and finalize workflow
## Assess model characteristics


### 8. Testing
## Preprocess test data using same methods for training data
## Run trained and tuned model onto test data

