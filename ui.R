
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  
  title = "Hello Shiny!",
  
  fluidRow(
    column(3,
           selectInput(
             inputId = "year",
             label = "Year",
             choices = c("all", sort(unique(year(transactions_dt$year)), decreasing = TRUE)),
             # selected = NULL,
             # multiple = FALSE,
             # selectize = TRUE,
             # width = NULL,
             # size = NULL
           )
    ),
    column(3,
           selectInput(
             inputId = "grouping_var",
             label = "Transactions per",
             choices = c("month", "year"),
             # selected = NULL,
             # multiple = FALSE,
             # selectize = TRUE,
             # width = NULL,
             # size = NULL
           )
    ),
    column(3,
           selectInput(
             inputId = "category_var",
             label = "Inspect category",
             choices = sort(unique(transactions_dt$category)),
             # selected = NULL,
             # multiple = FALSE,
             # selectize = TRUE,
             # width = NULL,
             # size = NULL
           )
    ),
    column(3,
           selectInput(
             inputId = "color_scheme",
             label = "Color scheme",
             choices = c("default", "colorblind1" ,"colorblind2"),
             # selected = NULL,
             # multiple = FALSE,
             # selectize = TRUE,
             # width = NULL,
             # size = NULL
           )
    )
  ),
  
  sidebar = sidebar(
    
    radioButtons(
      inputId = "plot_type",
      label = "Choose plot",
      choices = c(
        "income vs expense", "main group expenses",
        "sub group expenses", "other"
      ),
      #selected = NULLª,
      #inline = FALSE,
      #width = NULL,
      #choiceNames = NULL,
      #choiceValues = NULL
    ),
    
    radioButtons(
      inputId = "table_type",
      label = "Choose table",
      choices = c(
        "expenses", "income"
      ),
      selected = character(0),
      #inline = FALSE,
      #width = NULL,
      #choiceNames = NULL,
      #choiceValues = NULL
    ),
  ),
  
  plotlyOutput(outputId = "plot"),
  dataTableOutput(outputId = 'table')
  
)