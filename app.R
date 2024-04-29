library("shiny")
library("tidyverse")
library("scales")
library("DT")
library("bslib")
library("shinyjs")
library("plotly")

source("ui.R")
source("server.R")
source("plots.R")
source("tables.R")

shinyApp(ui = ui, server = server)
