################################################################################
# Server
################################################################################

shinyServer(function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  ################################################################################
  # Menus déroulants
  ################################################################################ 
  
  ##-- Pour le Réquêteur DAP ---
  source("tabs/serv_DAPfilters.R", encoding = "UTF-8", local = TRUE)

  ##-- Pour le Réquêteur DI ----
  source("tabs/serv_DIfilters.R", encoding = "UTF-8", local = TRUE)

  
  #=============================================================================
  # Tableaux
  #=============================================================================
  
  ##-- Pour le Réquêteur DAP ---
  source("tabs/serv_TabDAP.R", encoding = "UTF-8", local = TRUE)
  
  ##-- Pour le Réquêteur DI ----
  source("tabs/serv_TabDI.R", encoding = "UTF-8", local = TRUE)
  
  
  #=============================================================================
  # Téléchargement
  #=============================================================================
  
  ##-- Pour le Réquêteur DAP ---
  source("tabs/serv_DownlaodDAP.R", encoding = "UTF-8", local = TRUE)
  
  ##-- Pour le Réquêteur DI ---
  source("tabs/serv_DownlaodDI.R", encoding = "UTF-8", local = TRUE)
  
  
  #=============================================================================
  # Graphiques
  #=============================================================================
  
  ##-- Pour le Réquêteur DAP ---
  source("tabs/serv_GraphDAP.R", encoding = "UTF-8", local = TRUE)
  
  ##-- Pour le Réquêteur DI ---
  source("tabs/serv_GraphDI.R", encoding = "UTF-8", local = TRUE)
  

})