output$downloadDI <- downloadHandler(
  filename = function() {
    paste0("reqAP_DI_export_du_",format(Sys.time(), "%A_%d_%B_%Y"),".xlsx")
  },
  content = function(file) {
    write_xlsx(list(DI = dataDI()[input[["dataReqDI_rows_all"]], ]), path = file)
  }
)