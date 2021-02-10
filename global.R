################################################################################
# Library
################################################################################
library(shinymanager)
library(DT)
library(shiny)
library(plotly)
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(crosstalk)
library(shinydashboard)
library(shinyBS)


################################################################################
# Authentification
################################################################################
credentials <- data.frame(
  user = c("user", "admin"),
  password = c("1234", "4321"),
  stringsAsFactors = FALSE
)

################################################################################
# Données Rdata sur l'activité partielle
################################################################################
load("data/tab_reqDAP.Rda")
load("data/tab_reqDI.Rda")

EtatUI <- reactiveValues()
EtatUI$rdap <- 0
EtatUI$rdi <- 0
EtatUI$mdiviz <- 0
EtatUI$bartype <- "stack"