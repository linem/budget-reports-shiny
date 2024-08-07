
plot_design <- function(plot,
                        grouping_var,
                        plot_title,
                        plot_fill,
                        col_palette,
                        hover_tooltip) {
  p <- plot +
    scale_x_datetime(
      labels = date_format(get_date_format(grouping_var)),
      date_breaks = grouping_var
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.1)
    ) +
    scale_fill_manual(
      values = col_palette
    ) +
    labs(
      title = plot_title,
      fill = plot_fill,
      x = grouping_var
    ) +
    theme(
      text = element_text(size = 18),
      axis.title.x = element_text(vjust = -0.5),
      axis.text.x = element_text(angle = 90),
    )
  return(ggplotly(p, tooltip = hover_tooltip))
}

income_expense_plot <- function(dt,
                                selected_year,
                                grouping_var,
                                col_palette) {
  grouping_var_sym <- sym(grouping_var)
  
  dt <- dt %>%
    filter_year(selected_year)
  
  bar_dt <- dt %>%
    group_by(!!grouping_var_sym, transtype) %>%
    summarize(
      sum = sum(sum_month),
      .groups = "drop"
    )
  
  diff_dt <- bar_dt %>% get_difference(grouping_var_sym)
  
  options(scipen = 10000)
  plot_design(
    plot = ggplot() +
      geom_col(
        data = bar_dt,
        mapping = aes(
          x = as.POSIXct(!!grouping_var_sym),
          y = sum,
          fill = transtype
        ),
        position = position_dodge2(
          preserve = "single"
        )
      ) +
      geom_text(
        data = diff_dt,
        mapping = aes(
          x = as.POSIXct(!!grouping_var_sym),
          y = max + diff_padding,
          label = diff_symbol
        )
      ),
    grouping_var = grouping_var,
    plot_title = "Total income and expense",
    plot_fill = "transactions",
    col_palette = col_palette,
    hover_tooltip = c("transtype", "y")
  )
}

main_group_expense_plot <- function(dt,
                                    selected_year,
                                    grouping_var,
                                    col_palette) {
  
  grouping_var_sym <- sym(grouping_var)
  
  dt <- dt %>%
    filter_year(selected_year)
  
  bar_dt <- dt %>%
    filter(transtype == "expense") %>%
    group_by(!!grouping_var_sym, maincategory) %>%
    mutate(sum = sum(sum_month)) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  options(scipen = 10000)
  plot_design(
    plot = ggplot() +
      geom_col(
        data = bar_dt,
        mapping = aes(
          x = as.POSIXct(!!grouping_var_sym),
          y = sum,
          fill = maincategory
        ),
        position = position_dodge2(width = 0.9, preserve = "single")
      ),
    grouping_var = grouping_var,
    plot_title = "Expenses",
    plot_fill = "maincategory",
    col_palette = col_palette,
    hover_tooltip = c("maincategory", "y")
  )
}



sub_group_expense_plot <- function(dt,
                                   selected_year,
                                   grouping_var,
                                   category_var,
                                   col_palette) {
  
  grouping_var_sym <- sym(grouping_var)
  
  dt <- dt %>%
    filter_year(selected_year)
  
  bar_dt <- dt %>%
    filter(maincategory == !!category_var) %>% 
    filter(transtype == "expense") %>%
    group_by(!!grouping_var_sym, subcategory) %>%
    mutate(sum = sum(sum_month)) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  options(scipen = 10000)
  plot_design(
    plot = ggplot() +
      geom_col(
        data = bar_dt,
        mapping = aes(
          x = as.POSIXct(!!grouping_var_sym),
          y = sum,
          fill = subcategory
        ),
        position = position_dodge2(width = 0.9, preserve = "single")
      ),
    grouping_var = grouping_var,
    plot_title = "Sub category expenses",
    plot_fill = "subcategory",
    col_palette = col_palette,
    hover_tooltip = c("subcategory", "y")
  )
}
