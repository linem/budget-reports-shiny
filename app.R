library("shiny")
library("tidyverse")
library("scales")
library("DT")
library("bslib")
library("shinyjs")
library("plotly")


transactions_dt <- read_csv("data/mydata_2024-04-22.csv", show_col_types = FALSE) %>%
  select(date, category, subcategory, name, transaction, amount)

source("ui.R")
source("server.R")
source("plots.R")
#source("tables2-0.R")

shinyApp(ui = ui, server = server)



