library("shiny")
library("tidyverse")
library("scales")
library("DT")
library("bslib")
library("shinyjs")
library("plotly")

source("ui.R")
source("server.R")
source("load_functions.R")
source("helper_functions.R")

source("plots.R")
source("tables.R")

shinyApp(ui = ui, server = server)



## add year to datasettet in load_data
