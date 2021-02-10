tabdi <- reactive(if(is.null(input$menu1RDI)){tab_reqDI
}else if(input$menu0RDI=="A17" & str_sub(input$menu1RDI,1,2) %in% tab_reqDI$Code_NAF_A17){
  tab_reqDI[tab_reqDI$Code_NAF_A17 %in% str_sub(input$menu1RDI,1,2),]
}else if(input$menu0RDI=="A38" & str_sub(input$menu1RDI,1,2) %in% tab_reqDI$Code_NAF_A38){
  tab_reqDI[tab_reqDI$Code_NAF_A38 %in% str_sub(input$menu1RDI,1,2),]
}else if(input$menu0RDI=="A88" & str_sub(input$menu1RDI,1,2) %in% tab_reqDI$Code_NAF_A88){
  tab_reqDI[tab_reqDI$Code_NAF_A88 %in% str_sub(input$menu1RDI,1,2),]
}else{tab_reqDI
})

dataDI <- reactive(if(input$menu2RDI=="APE"){aggregate(cbind(Nombre_de_DI, Effectif_en_DI, Heures_en_DI, Montant_demandé_en_DI) ~ 
                                                         Code_NAF_APE + Libellé_APE_du_secteur + mois_statistique, data = tabdi(), sum)
}else if(input$menu2RDI=="A17"){aggregate(cbind(Nombre_de_DI, Effectif_en_DI, Heures_en_DI, Montant_demandé_en_DI) ~ 
                                            Code_NAF_A17 + Libellé_A17_du_secteur + mois_statistique, data = tabdi(), sum)
}else if(input$menu2RDI=="A38"){aggregate(cbind(Nombre_de_DI, Effectif_en_DI, Heures_en_DI, Montant_demandé_en_DI) ~ 
                                            Code_NAF_A38 + Libellé_A38_du_secteur + mois_statistique, data = tabdi(), sum)
}else if(input$menu2RDI=="A88"){aggregate(cbind(Nombre_de_DI, Effectif_en_DI, Heures_en_DI, Montant_demandé_en_DI) ~ 
                                            Code_NAF_A88 + Libellé_A88_du_secteur + mois_statistique, data = tabdi(), sum)
})

DIAct <- reactive(if(is.null(input$menu3RDI) & is.null(input$menu4RDI)){dataDI()
}else{dataDI()[dataDI()[,1] %in% input$menu3RDI |
                 dataDI()[,2] %in% input$menu4RDI,]
})

DIActu <- reactive(if(is.null(input$menu5RDI)){DIAct()
}else{DIAct()[DIAct()[,3] %in% input$menu5RDI,]
})

output$dataReqDI = DT::renderDataTable({
  if(!is.null(input$menu2RDI)){
    EtatUI$rdi <- 1
    datatable(DIActu(), 
              filter = 'none', rownames = FALSE, 
              options = list(order = list(list(0, 'asc')))
    )%>%
      formatRound(c('Nombre_de_DI','Effectif_en_DI','Heures_en_DI','Montant_demandé_en_DI'), digits = 0, interval = 3, mark = " ")
  }
})

output$TotReqDI = DT::renderDataTable({
  if(EtatUI$rdi == 1){
    datatable(as.data.frame(lapply(DIActu()[input[["dataReqDI_rows_all"]], -c(1:3)],sum), row.names = c("Total de la selection")), 
              filter = 'none', rownames = TRUE, options = list(dom = 't'))%>%
      formatRound(c('Nombre_de_DI','Effectif_en_DI','Heures_en_DI','Montant_demandé_en_DI'), digits = 0, interval = 3, mark = " ")
  }
})
