observeEvent(input$DIviz,{
  ds <- DIActu()
  
  if(length(unique(ds[, 2])) < 20){
    
    EtatUI$mdiviz <- 1
    
    m <- list(l = 50,
              r = 50,
              b = 100,
              t = 100,
              pad = 4)
    
    palcol <- c('deepskyblue','deepskyblue1','deepskyblue2', 'deepskyblue3', 'dodgerblue', 'dodgerblue1', 'dodgerblue2', 'dodgerblue3')
    
    if(input$menu6RDI=="Les demandes"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,4])])
      output$plotDI <- renderPlotly(
        plot_ly(ds, x = ds[,4], y = ds[, 2], type= "bar",
                name = ~mois_statistique, color = ~mois_statistique, colors = palcol, height = "600")%>%
          layout(title="Nombre de demandes d'indemnisation",
                 barmode = EtatUI$bartype,
                 autosize = T, margin = m)
      )
    }else if(input$menu6RDI=="Les effectifs"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,5])])
      output$plotDI <- renderPlotly(
        plot_ly(ds, x = ds[,5], y = ds[, 2], type= "bar",
                name = ~mois_statistique, color = ~mois_statistique, colors = palcol, height = "600")%>%
          layout(title="Effectifs indemnisés",
                 barmode = EtatUI$bartype,
                 autosize = T, margin = m)
      )
    }else if(input$menu6RDI=="Les heures"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,6])])
      output$plotDI <- renderPlotly(
        plot_ly(ds, x = ds[,6], y = ds[, 2], type= "bar",
                name = ~mois_statistique, color = ~mois_statistique, colors = palcol, height = "600")%>%
          layout(title="Nombre d'heures indemnisés",
                 barmode = EtatUI$bartype,
                 autosize = T, margin = m)
      )
    }else if(input$menu6RDI=="Les montants"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,7])])
      output$plotDI <- renderPlotly(
        plot_ly(ds, x = ds[,7], y = ds[, 2], type= "bar",
                name = ~mois_statistique, color = ~mois_statistique, colors = palcol, height = "600")%>%
          layout(title="Montants indemnisés",
                 barmode = EtatUI$bartype,
                 autosize = T, margin = m)
      )
    }
    
  }else{
    
    EtatUI$mdiviz <- 0
    
    m <- list(l = 50,
              r = 50,
              b = 100,
              t = 100,
              pad = 4)
    
    if(input$menu6RDI=="Les demandes"){
      ds <- ds %>% 
        mutate(grp_DI = case_when(
          Nombre_de_DI < 10  ~ "moins de 10",
          Nombre_de_DI >= 10 & Nombre_de_DI < 100 ~ "entre 10 et 99",
          Nombre_de_DI >= 100 & Nombre_de_DI < 500 ~ "entre 100 et 499",
          Nombre_de_DI >= 500 & Nombre_de_DI < 1000 ~ "entre 500 et 999",
          Nombre_de_DI >= 1000 & Nombre_de_DI < 2000 ~ "entre 1 000 et 1 999",
          Nombre_de_DI >= 2000 & Nombre_de_DI < 5000 ~ "entre 2 000 et 4 999",
          Nombre_de_DI >= 5000 & Nombre_de_DI < 10000 ~ "entre 5 000 et 9 999",
          Nombre_de_DI >= 10000 ~ "10 000 ou plus"))
      
      ds$grp_DI <- factor(ds$grp_DI, levels = c("moins de 10", "entre 10 et 99", "entre 100 et 499", "entre 500 et 999",
                                                "entre 1 000 et 1 999", "entre 2 000 et 4 999", "entre 5 000 et 9 999", "10 000 ou plus"))
      output$plotDI <- renderPlotly(
        plot_ly(ds, x=ds$grp_DI, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre des demandes",
                 xaxis = list(title = "Nombre de demandes"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu6RDI=="Les effectifs"){
      ds <- ds %>% 
        mutate(grp_EFF = case_when(
          Effectif_en_DI < 100  ~ "moins de 100",
          Effectif_en_DI >= 100 & Effectif_en_DI < 1000 ~ "entre 100 et 999",
          Effectif_en_DI >= 1000 & Effectif_en_DI < 5000 ~ "entre 1 000 et 4 999",
          Effectif_en_DI >= 5000 & Effectif_en_DI < 10000 ~ "entre 5 000 et 9 999",
          Effectif_en_DI >= 10000 & Effectif_en_DI < 50000 ~ "entre 10 000 et 49 999",
          Effectif_en_DI >= 50000 & Effectif_en_DI < 100000 ~ "entre 50 000 et 99 999",
          Effectif_en_DI >= 100000 & Effectif_en_DI < 500000 ~ "entre 100 000 et 499 999",
          Effectif_en_DI >= 500000 ~ "500 000 ou plus"))
      
      ds$grp_EFF <- factor(ds$grp_EFF, levels = c("moins de 10", "entre 100 et 999", "entre 1 000 et 4 999", "entre 5 000 et 9 999",
                                                  "entre 10 000 et 49 999", "entre 50 000 et 99 999", "entre 100 000 et 499 999", 
                                                  "500 000 ou plus"))
      output$plotDI <- renderPlotly(
        plot_ly(ds, x=ds$grp_EFF, color = ~mois_statistique, colors = palcol, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre de salariés indemnisés",
                 xaxis = list(title = "Nombre de salariés indemnisés"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu6RDI=="Les heures"){
      ds <- ds %>% 
        mutate(grp_HEUR = case_when(
          Heures_en_DI < 1000  ~ "Moins de mille",
          Heures_en_DI >= 1000 & Heures_en_DI < 10000 ~ "De mille à 10 milles",
          Heures_en_DI >= 10000 & Heures_en_DI < 100000 ~ "De 10 milles à 100 milles",
          Heures_en_DI >= 100000 & Heures_en_DI < 500000 ~ "De 100 milles à 500 milles",
          Heures_en_DI >= 500000 & Heures_en_DI < 1000000 ~ "De 500 milles à 1 million",
          Heures_en_DI >= 1000000 & Heures_en_DI < 5000000 ~ "De 1 million à 5 millions",
          Heures_en_DI >= 5000000 & Heures_en_DI < 10000000 ~ "De 5 millions à 10 millions",
          Heures_en_DI >= 10000000 & Heures_en_DI < 50000000 ~ "De 10 millions à 50 millions",
          Heures_en_DI >= 50000000 & Heures_en_DI < 100000000 ~ "De 50 millions à 100 millions",
          Heures_en_DI >= 100000000 & Heures_en_DI < 500000000 ~ "De 100 millions à 500 millions",
          Heures_en_DI >= 500000000 & Heures_en_DI < 1000000000 ~ "De 500 millions à 1 milliard",
          Heures_en_DI >= 1000000000 ~ "1 milliard ou plus"))
      
      ds$grp_HEUR <- factor(ds$grp_HEUR, levels = c("Moins de mille", "De mille à 10 milles", "De 10 milles à 100 milles",
                                                    "De 100 milles à 500 milles", "De 500 milles à 1 million", "De 1 million à 5 millions",
                                                    "De 5 millions à 10 millions", "De 10 millions à 50 millions", "De 50 millions à 100 millions",
                                                    "De 100 millions à 500 millions", "De 500 millions à 1 milliard", "1 milliard ou plus"))
      output$plotDI <- renderPlotly(
        plot_ly(ds, x=ds$grp_HEUR, color = ~mois_statistique, colors = palcol, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre d'heures indemnisées",
                 xaxis = list(title = "Nombre d'heures indemnisées"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu6RDI=="Les montants"){
      ds <- ds %>% 
        mutate(grp_MONTAN = case_when(
          Montant_demandé_en_DI < 10000  ~ "Moins de 10 milles",
          Montant_demandé_en_DI >= 10000 & Montant_demandé_en_DI < 100000 ~ "De 10 milles à 100 milles",
          Montant_demandé_en_DI >= 100000 & Montant_demandé_en_DI < 500000 ~ "De 100 milles à 500 milles",
          Montant_demandé_en_DI >= 500000 & Montant_demandé_en_DI < 1000000 ~ "De 500 milles à 1 million",
          Montant_demandé_en_DI >= 1000000 & Montant_demandé_en_DI < 5000000 ~ "De 1 million à 5 millions",
          Montant_demandé_en_DI >= 5000000 & Montant_demandé_en_DI < 10000000 ~ "De 5 millions à 10 millions",
          Montant_demandé_en_DI >= 10000000 & Montant_demandé_en_DI < 50000000 ~ "De 10 millions à 50 millions",
          Montant_demandé_en_DI >= 50000000 & Montant_demandé_en_DI < 100000000 ~ "De 50 millions à 100 millions",
          Montant_demandé_en_DI >= 100000000 & Montant_demandé_en_DI < 500000000 ~ "De 100 millions à 500 millions",
          Montant_demandé_en_DI >= 500000000 & Montant_demandé_en_DI < 1000000000 ~ "De 500 millions à 1 milliard",
          Montant_demandé_en_DI >= 1000000000 ~ "1 milliard ou plus"))
      
      ds$grp_MONTAN <- factor(ds$grp_MONTAN, levels = c("Moins de 10 mille", "De 10 milles à 100 milles",
                                                        "De 100 milles à 500 milles", "De 500 milles à 1 million", "De 1 million à 5 millions",
                                                        "De 5 millions à 10 millions", "De 10 millions à 50 millions", "De 50 millions à 100 millions",
                                                        "De 100 millions à 500 millions", "De 500 millions à 1 milliard", "1 milliard ou plus"))
      output$plotDI <- renderPlotly(
        plot_ly(ds, x=ds$grp_MONTAN, color = ~mois_statistique, colors = palcol, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon les montants indemnisées",
                 xaxis = list(title = "Montants indemnisées"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }
  }
})