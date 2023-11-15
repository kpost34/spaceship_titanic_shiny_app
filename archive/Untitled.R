output$ui_sel_ordEnc2_trnsFea04<-renderUI({
  req(input$chkgrp_ordEnc_trnsFea04)
  tags_sel<-tagList()
  for(i in input$chkgrp_ordEnc_trnsFea04){
    tags_sel[[i]]<-selectizeInput(inputId=paste("sel",i,"ordEnc_trnsFea04",sep="_"),
                                  label=i,multiple=TRUE,
                                  choices=c(varSelOrd_feat,
                                            trainDF_nvI()[[i]] %>% levels())
    )
  }
  tags_sel
})



uiOutput("ui_chkGrp_ordEnc2_trnsFea04"),
uiOutput("ui_sel_ordEnc2_trnsFea04"),
uiOutput("ui_sel_ordEnc3_trnsFea04"),
uiOutput("ui_sel_ordEnc4_trnsFea04"),
uiOutput("ui_sel_ordEnc5_trnsFea04"),
uiOutput("ui_sel_ordEnc6_trnsFea04")
         
         
         #vertically aligns button with selectizeInput
         tags$style(type="text/css", "#btn_exp2_creFea04 {width: 100%; margin-top: 25px;}")