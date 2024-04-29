
calculate_cat_year_sum <- function(dt) {
  dt %>%
    group_by(year, month, category, subcategory) %>%
    summarize(sum_month = sum(amount), .groups = "drop")
}

to_wide_format <- function(dt) {
  dt %>%
    pivot_wider(
      id_cols = c("category", "subcategory"),
      names_from = month,
      values_from = sum_month
    ) %>%
    mutate_if(is.numeric, replace_na, 0)
}

generate_missing_columns_of_NA <- function(dt) {
  months <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  month_dt <- dt %>%
    select(-c(category, subcategory))
  
  allyear_dt <- tibble(.rows = nrow(month_dt))
  for (month in months) {
    if (month %in% colnames(month_dt)) {
      allyear_dt[[month]] = month_dt[[month]]
    } else {
      allyear_dt[[month]] = rep(NA, nrow(month_dt))
    }
  }
  
  allyear_dt_w_an <- dt %>%
    select(category, subcategory) %>%
    bind_cols(allyear_dt)
  
  return(allyear_dt_w_an)
}

calculate_row_summary <- function(dt) {
  dt %>%
    rowwise() %>%
    mutate(avg = round(mean(c_across(Jan:Dec), na.rm = TRUE))) %>%
    mutate(total = sum(c_across(Jan:Dec), na.rm = TRUE)) %>%
    arrange(category, desc(total))
}

calculate_col_summary <- function(dt) {
  totals_row <- dt %>%
    select(Jan:total) %>%
    colSums(.) %>%
    as.matrix() %>%
    t() %>%
    as_tibble() %>%
    mutate(category = "totals",
           subcategory = "-")
  
  dash_row <- c(
    rep("---", 2),
    rep(NA_integer_, length(colnames(dt))-2)
    )
  names(dash_row) <- colnames(dt)
  dash_row <- dash_row %>%
    as.matrix() %>%
    t() %>%
    as_tibble() %>%
    mutate_if(is.na , as.numeric)
  
  dt_w_totals_row <- dt %>%
    bind_rows(dash_row, totals_row)

  return(dt_w_totals_row)
}

yearly_overview_table <- function(dt, transaction_type, selected_year) {
  table_dt <- transactions_dt %>%
    filter(transaction == transaction_type) %>%
    filter(selected_year == year(date)) %>%
    arrange(date) %>% 
    mutate(month = month(date, label = TRUE),
           year = year(date)
    ) %>%
    calculate_cat_year_sum() %>%
    to_wide_format() %>%
    generate_missing_columns_of_NA() %>%
    calculate_row_summary() %>%
    calculate_col_summary()
  n_rows <- nrow(table_dt)
  DT::datatable(
    data = table_dt,
    options = list(
      paging =TRUE,
      pageLength = n_rows
    )
  )
}
