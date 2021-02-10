tabdap <- reactive(if(is.null(input$menu1RDAP)){tab_reqDAP
}else if(input$menu0RDAP=="A17" & str_sub(input$menu1RDAP,1,2) %in% tab_reqDAP$Code_NAF_A17){
  tab_reqDAP[tab_reqDAP$Code_NAF_A17 %in% str_sub(input$menu1RDAP,1,2),]
}else if(input$menu0RDAP=="A38" & str_sub(input$menu1RDAP,1,2) %in% tab_reqDAP$Code_NAF_A38){
  tab_reqDAP[tab_reqDAP$Code_NAF_A38 %in% str_sub(input$menu1RDAP,1,2),]
}else if(input$menu0RDAP=="A88" & str_sub(input$menu1RDAP,1,2) %in% tab_reqDAP$Code_NAF_A88){
  tab_reqDAP[tab_reqDAP$Code_NAF_A88 %in% str_sub(input$menu1RDAP,1,2),]
}else{tab_reqDAP
})

dataDAP <- reactive(if(input$menu2RDAP=="APE"){aggregate(cbind(Nombre_de_DAP, Effectif_demandé, Heures_demandées, siret_unique, siren_unique) ~ 
                                                           Code_NAF_APE + Libellé_APE_du_secteur, data = tabdap(), sum)
}else if(input$menu2RDAP=="A17"){aggregate(cbind(Nombre_de_DAP, Effectif_demandé, Heures_demandées, siret_unique, siren_unique) ~ 
                                             Code_NAF_A17 + Libellé_A17_du_secteur, data = tabdap(), sum)
}else if(input$menu2RDAP=="A38"){aggregate(cbind(Nombre_de_DAP, Effectif_demandé, Heures_demandées, siret_unique, siren_unique) ~ 
                                             Code_NAF_A38 + Libellé_A38_du_secteur, data = tabdap(), sum)
}else if(input$menu2RDAP=="A88"){aggregate(cbind(Nombre_de_DAP, Effectif_demandé, Heures_demandées, siret_unique, siren_unique) ~ 
                                             Code_NAF_A88 + Libellé_A88_du_secteur, data = tabdap(), sum)
})

DAPActu <- reactive (if(is.null(input$menu3RDAP) & is.null(input$menu4RDAP)){dataDAP()
}else{dataDAP()[dataDAP()[,1] %in% input$menu3RDAP | 
                  dataDAP()[,2] %in% input$menu4RDAP,]
})

output$dataReqDAP = DT::renderDataTable({
  if(!is.null(input$menu2RDAP)){
    EtatUI$rdap <- 1
    datatable(DAPActu(), 
              filter = 'none', rownames = FALSE, 
              options = list(order = list(list(0, 'asc')))
    )%>%
      formatRound(c('Nombre_de_DAP','Effectif_demandé','Heures_demandées','siret_unique', 'siren_unique'), 
                  digits = 0, interval = 3, mark = " ")
  }
})


output$TotReqDAP = DT::renderDataTable({
  if(EtatUI$rdap == 1){
    datatable(as.data.frame(lapply(DAPActu()[input[["dataReqDAP_rows_all"]], -c(1:2)],sum), row.names = c("Total de la selection")), 
              filter = 'none', rownames = TRUE, options = list(dom = 't'))%>%
      formatRound(c('Nombre_de_DAP','Effectif_demandé','Heures_demandées','siret_unique', 'siren_unique'), digits = 0, interval = 3, mark = " ")
  }
})
