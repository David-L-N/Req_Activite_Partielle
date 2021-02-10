
##-- Choix de la division ----
output$m0rdap = renderUI(selectInput("menu0RDAP","Division", c("A17","A38","A88"),"A17"))

##-- Choix du secteur ----
output$m1rdap = renderUI(if(is.null(input$menu0RDAP)){return()
  }else if(input$menu0RDAP=="A17"){
    selectInput("menu1RDAP","Choix du secteur d'activité",
                sort(paste(unique(tab_reqDAP$Code_NAF_A17),unique(tab_reqDAP$Libellé_A17_du_secteur),sep=" - ")), multiple = TRUE)
  }else if(input$menu0RDAP=="A38"){
    selectInput("menu1RDAP","Choix du secteur d'activité",
                sort(paste(unique(tab_reqDAP$Code_NAF_A38),unique(tab_reqDAP$Libellé_A38_du_secteur),sep=" - ")), multiple = TRUE)
  }else if(input$menu0RDAP=="A88"){
    selectInput("menu1RDAP","Choix du secteur d'activité",
                sort(paste(unique(tab_reqDAP$Code_NAF_A88),unique(tab_reqDAP$Libellé_A88_du_secteur),sep=" - ")), multiple = TRUE)
  }
)

##-- Choix de la sous-division ----
output$m2rdap = renderUI(if(is.null(input$menu0RDAP)){return()
  }else if(input$menu0RDAP=="A17"){
    selectInput("menu2RDAP","Affichage", c("A17","A38","A88","APE"),"A17")
  }else if(input$menu0RDAP=="A38"){
    selectInput("menu2RDAP","Affichage", c("A38","A88","APE"),"A38")
  }else if(input$menu0RDAP=="A88"){
    selectInput("menu2RDAP","Affichage", c("A88","APE"),"A88")
  }
)

##-- filtre par code ----
output$m3rdap = renderUI(if(EtatUI$rdap == 1){selectInput("menu3RDAP","Filtrer par code", 
                                                          sort(dataDAP()[,1]),
                                                          multiple = TRUE)})

##-- filtre par libellé ----
output$m4rdap = renderUI(if(EtatUI$rdap == 1){selectInput("menu4RDAP","Filtrer par libellé", 
                                                          sort(dataDAP()[,2]),
                                                          multiple = TRUE)})

##-- Choix de la variable pour le graphique ----
output$m5rdap = renderUI(selectInput("menu5RDAP","Choix de la variable", c("Les demandes","Les effectifs","Les heures",
                                                                           "Les entreprises", "Les établissements" ),"Les demandes"))

