get_date_format <- function(grouping_var) {
  format = "%Y"
  if (grouping_var == "month") {
    format = paste(format, "%m", sep = "-")
  }
  return(format)
}


income_expense_plot <- function(dt, selected_year, grouping_var, col_palette) {
  if (selected_year != "all") {
    dt <- dt %>% filter(selected_year == year(year))
  }
  
  grouping_var_sym <- sym(grouping_var)
  
  bar_dt <- dt %>%                     ########
    group_by(!!grouping_var_sym, transaction) %>%
    summarize(sum = sum(amount), .groups = "drop")
  
  
  diff_dt <- bar_dt %>%
    group_by(!!grouping_var_sym) %>%
    summarize(max = max(sum),
              diff = diff(sum)) %>%
    ungroup() %>%
    mutate(diff_symbol = sprintf("%+d", diff)) %>%
    mutate(diff_padding = max(max)*0.05)
  options(scipen = 10000)
  p <- ggplot() +
    geom_col(
      data = bar_dt,
      mapping = aes(
        x = as.POSIXct(!!grouping_var_sym),
        y = sum,
        fill = transaction
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
    ) +
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
      title = "Total income and expense",
      fill = "transactions",
      x = grouping_var
    ) +
    theme(
      text = element_text(size = 18),
      axis.title.x = element_text(vjust = -0.5),
      axis.text.x = element_text(angle = 90),
    )
  ggplotly(p,tooltip = c("transaction", "y"))
}


main_group_expense_plot <- function(dt, grouping_var, col_palette) {
  grouping_var_sym <- sym(grouping_var)
  bar_dt <- dt %>%
    filter(transaction == "expense") %>%
    group_by(!!grouping_var_sym, category) %>%
    mutate(sum = sum(amount)) %>%
    filter(row_number() == 1) %>%
    ungroup()
  p <- ggplot() +
    geom_col(
      data = bar_dt,
      mapping = aes(
        x = as.POSIXct(!!grouping_var_sym),
        y = sum,
        fill = category
      ),
      position = position_dodge2(width = 0.9, preserve = "single")
    ) +
    theme() +
    scale_x_datetime(
      labels = date_format(get_date_format(grouping_var)),
      date_breaks = grouping_var
    ) +
    scale_fill_manual(
      values = col_palette
    ) +
    labs(
      title = "Expenses",
      x = grouping_var
    ) +
    theme(
      text = element_text(size = 18),
      axis.title.x = element_text(vjust = -0.5),
      axis.text.x = element_text(angle = 90),          # , vjust = 0.5, hjust = 1),
    )
  ggplotly(p,tooltip = c("category", "y"))
}



sub_group_expense_plot <- function(dt, grouping_var, category_var, col_palette) {
  grouping_var_sym <- sym(grouping_var)
  bar_dt <- dt %>%
    filter(category == !!category_var) %>% 
    filter(transaction == "expense") %>%
    group_by(!!grouping_var_sym, subcategory) %>%
    mutate(sum = sum(amount)) %>%
    filter(row_number() == 1) %>%
    ungroup()
  p <- ggplot() +
    geom_col(
      data = bar_dt,
      mapping = aes(
        x = as.POSIXct(!!grouping_var_sym),
        y = sum,
        fill = subcategory
      ),
      position = position_dodge2(width = 0.9, preserve = "single")
    ) +
    scale_x_datetime(
      labels = date_format(get_date_format(grouping_var)),
      date_breaks = grouping_var
    ) +
    scale_fill_manual(
      values = col_palette
    ) +
    labs(
      title = "Sub group expenses",
      x = grouping_var
    ) +
    theme(
      text = element_text(size = 18),
      axis.title.x = element_text(vjust = -0.5),
      axis.text.x = element_text(angle = 90),      # vjust = 0.5, hjust = 1)
    )
  ggplotly(p,tooltip = c("subcategory", "y"))
}
