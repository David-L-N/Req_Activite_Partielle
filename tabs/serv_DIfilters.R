##-- Choix de la division ----
output$m0rdi <- renderUI(selectInput("menu0RDI","Division", c("A17","A38","A88"),"A17"))

##-- Choix du secteur ----
output$m1rdi <- renderUI(if(is.null(input$menu0RDI)){return()
}else if(input$menu0RDI=="A17"){
  selectInput("menu1RDI","Choix du secteur d'activité",
              sort(paste(unique(tab_reqDI$Code_NAF_A17),unique(tab_reqDI$Libellé_A17_du_secteur),sep=" - ")), multiple = TRUE)
}else if(input$menu0RDI=="A38"){
  selectInput("menu1RDI","Choix du secteur d'activité",
              sort(paste(unique(tab_reqDI$Code_NAF_A38),unique(tab_reqDI$Libellé_A38_du_secteur),sep=" - ")), multiple = TRUE)
}else if(input$menu0RDI=="A88"){
  selectInput("menu1RDI","Choix du secteur d'activité",
              sort(paste(unique(tab_reqDI$Code_NAF_A88),unique(tab_reqDI$Libellé_A88_du_secteur),sep=" - ")), multiple = TRUE)
}
)

##-- Choix de la sous-division ----
output$m2rdi <- renderUI(if(is.null(input$menu0RDI)){return()
}else if(input$menu0RDI=="A17"){
  selectInput("menu2RDI","Affichage", c("A17","A38","A88","APE"),"A17")
}else if(input$menu0RDI=="A38"){
  selectInput("menu2RDI","Affichage", c("A38","A88","APE"),"A38")
}else if(input$menu0RDI=="A88"){
  selectInput("menu2RDI","Affichage", c("A88","APE"),"A88")
}
)

##-- filtre des codes ----
output$m3rdi <- renderUI(if(EtatUI$rdi == 1){selectInput("menu3RDI","Filtrer par code", 
                                                         sort(dataDI()[,1]),
                                                         multiple = TRUE)})

##-- filtre des libellés ----
output$m4rdi <- renderUI(if(EtatUI$rdi == 1){selectInput("menu4RDI","Filtrer par libellé", 
                                                         sort(dataDI()[,2]),
                                                         multiple = TRUE)})

##-- filtre des mois ----
output$m5rdi <- renderUI(if(EtatUI$rdi == 1){selectInput("menu5RDI","Filtrer par date", 
                                                         sort(dataDI()[,3]),
                                                         multiple = TRUE)})


##-- Choix de la variable pour le graphique ----
output$m6rdi <- renderUI(selectInput("menu6RDI","Choix de la variable", c("Les demandes","Les effectifs","Les heures", "Les montants"),"Les demandes"))

##-- Choix de des mois statistiques pour le graphique ----
output$m7rdi <- renderUI(if(EtatUI$mdiviz == 1){selectInput("menu7RDI","Mois statistiques", c("Empilées","Groupées"),"Empilées")})

##-- Choix du type de graphique ----
observe({if(!is.null(input$menu7RDI)){
  if(input$menu7RDI=="Empilées"){EtatUI$bartype <- "stack"
  }else if(input$menu7RDI=="Groupées"){EtatUI$bartype <- "group"}
}
})