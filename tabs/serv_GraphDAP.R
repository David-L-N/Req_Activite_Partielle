observeEvent(input$DAPviz,{
  ds <- DAPActu()
  if(length(ds[, 2]) < 20){
    
    m <- list(l = 50,
              r = 50,
              b = 100,
              t = 100,
              pad = 4)
    
    
    if(input$menu5RDAP=="Les demandes"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,3])])
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x = ds[,3], y = ds[, 2], type= "bar", height = "600")%>%
          layout(title="Nombre de demandes d'activité partielle", margin = m)
      )
    }else if(input$menu5RDAP=="Les effectifs"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,4])])
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x = ds[,4], y = ds[, 2], type= "bar", height = "600")%>%
          layout(title="Effectifs couverts par une demande", margin = m)
      )
    }else if(input$menu5RDAP=="Les heures"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,5])])
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x = ds[,5], y = ds[, 2], type= "bar", height = "600")%>%
          layout(title="Nombre d'heures demandées", margin = m)
      )
    }else if(input$menu5RDAP=="Les entreprises"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,7])])
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x = ds[,7], y = ds[, 2], type= "bar", height = "600")%>%
          layout(title="Nombre d'entreprises", margin = m)
      )
    }else if(input$menu5RDAP=="Les établissements"){
      ds[,2] <- factor(ds[,2], levels = unique(ds[,2])[order(ds[,6])])
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x = ds[,6], y = ds[, 2], type= "bar", height = "600")%>%
          layout(title="Nombre d'établissements", margin = m)
      )
    }
    
  }else{
    
    m <- list(l = 50,
              r = 50,
              b = 100,
              t = 100,
              pad = 4)
    
    if(input$menu5RDAP=="Les demandes"){
      ds <- ds %>% 
        mutate(grp_DAP = case_when(
          Nombre_de_DAP < 10  ~ "moins de 10",
          Nombre_de_DAP >= 10 & Nombre_de_DAP < 100 ~ "entre 10 et 99",
          Nombre_de_DAP >= 100 & Nombre_de_DAP < 500 ~ "entre 100 et 499",
          Nombre_de_DAP >= 500 & Nombre_de_DAP < 1000 ~ "entre 500 et 999",
          Nombre_de_DAP >= 1000 & Nombre_de_DAP < 2000 ~ "entre 1 000 et 1 999",
          Nombre_de_DAP >= 2000 & Nombre_de_DAP < 5000 ~ "entre 2 000 et 4 999",
          Nombre_de_DAP >= 5000 & Nombre_de_DAP < 10000 ~ "entre 5 000 et 9 999",
          Nombre_de_DAP >= 10000 ~ "10 000 ou plus"))
      
      ds$grp_DAP <- factor(ds$grp_DAP, levels = c("moins de 10", "entre 10 et 99", "entre 100 et 499", "entre 500 et 999",
                                                  "entre 1 000 et 1 999", "entre 2 000 et 4 999", "entre 5 000 et 9 999", "10 000 ou plus"))
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x=ds$grp_DAP, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre des demandes",
                 xaxis = list(title = "Nombre de demandes"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu5RDAP=="Les effectifs"){
      ds <- ds %>% 
        mutate(grp_EFF = case_when(
          Effectif_demandé < 100  ~ "moins de 100",
          Effectif_demandé >= 100 & Effectif_demandé < 1000 ~ "entre 100 et 999",
          Effectif_demandé >= 1000 & Effectif_demandé < 5000 ~ "entre 1 000 et 4 999",
          Effectif_demandé >= 5000 & Effectif_demandé < 10000 ~ "entre 5 000 et 9 999",
          Effectif_demandé >= 10000 & Effectif_demandé < 50000 ~ "entre 10 000 et 49 999",
          Effectif_demandé >= 50000 & Effectif_demandé < 100000 ~ "entre 50 000 et 99 999",
          Effectif_demandé >= 100000 & Effectif_demandé < 500000 ~ "entre 100 000 et 499 999",
          Effectif_demandé >= 500000 ~ "500 000 ou plus"))
      
      ds$grp_EFF <- factor(ds$grp_EFF, levels = c("moins de 10", "entre 100 et 999", "entre 1 000 et 4 999", "entre 5 000 et 9 999",
                                                  "entre 10 000 et 49 999", "entre 50 000 et 99 999", "entre 100 000 et 499 999", 
                                                  "500 000 ou plus"))
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x=ds$grp_EFF, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre de salariés converts",
                 xaxis = list(title = "Nombre de salariés converts"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu5RDAP=="Les heures"){
      ds <- ds %>% 
        mutate(grp_HEUR = case_when(
          Heures_demandées < 1000  ~ "Moins de mille",
          Heures_demandées >= 1000 & Heures_demandées < 10000 ~ "De mille à 10 milles",
          Heures_demandées >= 10000 & Heures_demandées < 100000 ~ "De 10 milles à 100 milles",
          Heures_demandées >= 100000 & Heures_demandées < 500000 ~ "De 100 milles à 500 milles",
          Heures_demandées >= 500000 & Heures_demandées < 1000000 ~ "De 500 milles à 1 million",
          Heures_demandées >= 1000000 & Heures_demandées < 5000000 ~ "De 1 million à 5 millions",
          Heures_demandées >= 5000000 & Heures_demandées < 10000000 ~ "De 5 millions à 10 millions",
          Heures_demandées >= 10000000 & Heures_demandées < 50000000 ~ "De 10 millions à 50 millions",
          Heures_demandées >= 50000000 & Heures_demandées < 100000000 ~ "De 50 millions à 100 millions",
          Heures_demandées >= 100000000 & Heures_demandées < 500000000 ~ "De 100 millions à 500 millions",
          Heures_demandées >= 500000000 & Heures_demandées < 1000000000 ~ "De 500 millions à 1 milliard",
          Heures_demandées >= 1000000000 ~ "1 milliard ou plus"))
      
      ds$grp_HEUR <- factor(ds$grp_HEUR, levels = c("Moins de mille", "De mille à 10 milles", "De 10 milles à 100 milles",
                                                    "De 100 milles à 500 milles", "De 500 milles à 1 million", "De 1 million à 5 millions",
                                                    "De 5 millions à 10 millions", "De 10 millions à 50 millions", "De 50 millions à 100 millions",
                                                    "De 100 millions à 500 millions", "De 500 millions à 1 milliard", "1 milliard ou plus"))
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x=ds$grp_HEUR, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre d'heures convertes",
                 xaxis = list(title = "Nombre d'heures convertes"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }else if(input$menu5RDAP=="Les entreprises"){
      ds <- ds %>% 
        mutate(grp_ENTR = case_when(
          siren_unique < 10  ~ "moins de 10",
          siren_unique >= 10 & siren_unique < 100 ~ "entre 10 et 99",
          siren_unique >= 100 & siren_unique < 500 ~ "entre 100 et 499",
          siren_unique >= 500 & siren_unique < 1000 ~ "entre 500 et 999",
          siren_unique >= 1000 & siren_unique < 2000 ~ "entre 1 000 et 1 999",
          siren_unique >= 2000 & siren_unique < 5000 ~ "entre 2 000 et 4 999",
          siren_unique >= 5000 & siren_unique < 10000 ~ "entre 5 000 et 9 999",
          siren_unique >= 10000 ~ "10 000 ou plus"))
      
      ds$grp_ENTR <- factor(ds$grp_ENTR, levels = c("moins de 10", "entre 10 et 99", "entre 100 et 499", "entre 500 et 999",
                                                    "entre 1 000 et 1 999", "entre 2 000 et 4 999", "entre 5 000 et 9 999", "10 000 ou plus"))
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x=ds$grp_ENTR, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre d'entreprises ayant fait une demande",
                 xaxis = list(title = "Nombre d'entreprises"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
      
    }else if(input$menu5RDAP=="Les établissements"){
      ds <- ds %>% 
        mutate(grp_ETAB = case_when(
          siren_unique < 10  ~ "moins de 10",
          siren_unique >= 10 & siren_unique < 100 ~ "entre 10 et 99",
          siren_unique >= 100 & siren_unique < 500 ~ "entre 100 et 499",
          siren_unique >= 500 & siren_unique < 1000 ~ "entre 500 et 999",
          siren_unique >= 1000 & siren_unique < 2000 ~ "entre 1 000 et 1 999",
          siren_unique >= 2000 & siren_unique < 5000 ~ "entre 2 000 et 4 999",
          siren_unique >= 5000 & siren_unique < 10000 ~ "entre 5 000 et 9 999",
          siren_unique >= 10000 ~ "10 000 ou plus"))
      
      ds$grp_ETAB <- factor(ds$grp_ETAB, levels = c("moins de 10", "entre 10 et 99", "entre 100 et 499", "entre 500 et 999",
                                                    "entre 1 000 et 1 999", "entre 2 000 et 4 999", "entre 5 000 et 9 999", "10 000 ou plus"))
      output$plotDAP <- renderPlotly(
        plot_ly(ds, x=ds$grp_ETAB, type= "histogram")%>%
          layout(bargap=0.1,
                 title="Nombre de secteurs selon le nombre d'établissements ayant fait une demande",
                 xaxis = list(title = "Nombre d'établissements"),
                 yaxis = list(title = "Nombre de secteurs"),
                 margin = m
          )
      )
    }
    
  }
})