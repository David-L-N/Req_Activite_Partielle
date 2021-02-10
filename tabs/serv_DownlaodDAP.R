output$downloadDAP <- downloadHandler(
  filename = function() {
    paste0("reqAP_DAP_export_du_",format(Sys.time(), "%A_%d_%B_%Y"),".xlsx")
  },
  content = function(file) {
    write_xlsx(list(DAP = DAPActu()[input[["dataReqDAP_rows_all"]], ]), path = file)
  }
)