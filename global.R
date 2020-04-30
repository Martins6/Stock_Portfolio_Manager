# Libraries
## App
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
## Data wrangling
library(tidyverse)
library(tidyselect)
library(lubridate)
library(PerformanceAnalytics)
## Plotting
library(plotly)
## Retrieving Data
library(quantmod)

# Source functions
source('functions_app.R')

######################################### INPUT ###################################
# Start date
start_date <- today() - 366*2

