# Title: Shiny Portfolio Management 
# Author: Adriel Martins
# Date: 21/19/2020

# EXECUTING THE APP
## Global
source('global.R')
## UI
source('ui.R')
## Server
source('server.R')

shiny::shinyApp(ui, server)

