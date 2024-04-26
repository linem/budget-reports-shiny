
server <- function(input, output, session) {
  
  show("plot")
  hide("table")

  observeEvent(input$plot_type, {
    updateRadioButtons(
      session = session,
      inputId = "table_type",
      selected = character(0)
    )
    updateSelectInput(
      session = session,
      inputId = "year",
      choices = c("all", get_years(transactions_dt)
      )
    )
  })

  observeEvent(input$table_type, {
    updateRadioButtons(
      session = session,
      inputId = "plot_type",
      selected = character(0)
    )
    updateSelectInput(
      session = session,
      inputId = "year",
      choices = get_years(transactions_dt)
    )
  })
  
  output$plot <- renderPlotly({
    
    show("plot")
    hide("table")
    
    if (input$color_scheme == "default") {
      col_palette <- c(
        "#F8766D", "#D89000", "#A3A500", "#39B600", "#00BF7D",
        "#00BFC4", "#00B0F6", "#9590FF", "#E76BF3", "#FF62BC"
      )
      
    } else if (input$color_scheme == "colorblind1") {
      col_palette <- c(
        "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
        "#D55E00", "#CC79A7", "#118483", "#3FF8C5", "#000000"
      )
      
    } else if (input$color_scheme == "colorblind2") {
      col_palette <- c(
        "#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77",
        "#CC6677", "#AA4499", "#882255", "#3235CD", "#ABBFDE"
      )
      
    }
    
    
    if (input$plot_type == "income vs expense") {
      shinyjs::disable("category_var")
      print("plot1")
      income_expense_plot(transactions_dt, input$year, input$grouping_var, col_palette)
      
    } else if (input$plot_type == "main group expenses") {
      shinyjs::disable("category_var")
      print("plot2")
      main_group_expense_plot(transactions_dt, input$year, input$grouping_var, col_palette)
      
      
    } else if (input$plot_type == "sub group expenses") {
      shinyjs::enable("category_var")
      print("plot3")
      sub_group_expense_plot(transactions_dt, input$year, input$grouping_var, input$category_var, col_palette)
      
    }
    

    
  })
  
  output$table <- renderDataTable({
    
   # show("table")
   # hide("plot")
    
    # shinyjs::disable("grouping_var")
    # shinyjs::disable("category_var")
    # shinyjs::disable("color_scheme")
    # if (input$table_type == "expenses") {
    #   yearly_overview_table(transactions_dt, "expense", input$year)
    # } else if (input$table_type == "income") {
    #   yearly_overview_table(transactions_dt, "income", input$year)
    # }
  })
  
}