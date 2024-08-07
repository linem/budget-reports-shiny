
yearly_overview_table <- function(dt, transaction_type, selected_year) {
  table_dt <- transactions_dt %>%
    filter(transtype == "expense") %>%  #transaction_type) %>%
    filter(year(month) == "2024") %>% #selected_year) %>%
    arrange(month) %>% 
    mutate(
      month = sapply(month(month), get_month_abr_eng),
      year = year(month)
    ) %>%
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

