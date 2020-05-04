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
## Plotting
library(plotly)
library(ggthemes)
library(ggpubr)
library(hrbrthemes)

# Source functions
source('functions_app.R')

######################################### INPUT ###################################
# Start date
start_date <- today() - 366*2

