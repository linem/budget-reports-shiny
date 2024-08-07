
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  
  title = "Hello Shiny!",
  
  fluidRow(
    column(3,
           selectInput(
             inputId = "year",
             label = "Year",
             choices = character(0)
             #choices = c("all", get_years(transactions_dt))
           )
    ),
    column(3,
           selectInput(
             inputId = "grouping_var",
             label = "Transactions per",
             choices = character(0)
             #choices = c("month", "year")
           )
    ),
    column(3,
           selectInput(
             inputId = "category_var",
             label = "Inspect category",
             choices = character(0)
             #choices = get_categories(transactions_dt)
           )
    ),
    column(3,
           selectInput(
             inputId = "color_scheme",
             label = "Color scheme",
             choices = character(0)
             #choices =  c("default", "colorblind1" ,"colorblind2")
           )
    )
  ),
  
  sidebar = sidebar(
    
    selectInput(
      inputId = "filepath",
      label = "Select file",
      choices = c("-- select --", list.files("data_ready")),
      selected = character(0)
    ),
    
    actionButton(
      inputId = "load_action",
      label = "Load data",
      style = "padding:4px; font-size:80%"
    ),
    
    radioButtons(
      inputId = "plot_type",
      label = "Select plot",
      choices = c(
        "income vs expense",
        "maincat expenses",
        "subcat expenses"
      ),
      selected = character(0),
    ),
    
    radioButtons(
      inputId = "table_type",
      label = "Select table",
      choices = c("expenses", "income"),
      selected = character(0),
    ),
  ),
  plotlyOutput(outputId = "plot"),
  dataTableOutput(outputId = 'table')
  
)