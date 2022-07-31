#### Name Imputation Options Modals================================================================================================
modal_colDeletion_misNam03<-modalDialog(
  "Deleting name columns prevents use of this variable in feature engineering. Would you still like to continue?",
  title="Deleting columns",
  footer=tagList(
    actionButton("cancel","Cancel"),
    actionButton("ok","Delete",class="btn btn-danger")
  )
)

modal_rowDeletion_misNam03<-modalDialog(
  "Deleting rows containing missing names may disproportionately affect distribution of values of other columns and thus impact
  variable imputation and model accuracy. Would you still like to continue?",
  title="Deleting rows",
  footer=tagList(
    actionButton("cancel","Cancel"),
    actionButton("ok","Delete",class="btn btn-danger")
  )
)

