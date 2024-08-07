get_years <- function(dt) {
  dt %>%
    mutate(year = year(month)) %>%
    select(year) %>%
    distinct() %>%
    arrange(desc(year)) %>%
    pull(year)
}

get_categories <- function(dt) {
  dt %>%
    select(maincategory) %>%
    distinct() %>%
    arrange(maincategory) %>%
    pull(maincategory)
}

get_date_format <- function(grouping_var) {
  format = "%Y"
  if (grouping_var == "month") {
    format = paste(format, "%m", sep = "-")
  }
  return(format)
}


filter_year <- function(dt, selected_year) {
  mutate(year = year(month)) %>%
  if (selected_year != "all") {
    dt <- dt %>% filter(selected_year == year(month))
  }
  return(dt)
}


get_difference <- function(dt, grouping_var_sym) {
  dt %>% 
    group_by(!!grouping_var_sym) %>%
    summarize(
      max = max(sum),
      diff = diff(sum),
      .groups = "drop"
    ) %>%
    mutate(diff_symbol = sprintf("%+d", diff)) %>%
    mutate(diff_padding = max(max)*0.05)
}


get_month_abr_eng <- function(month_number) {
  month_abr_eng <- list(
    '1' = "Jan",
    '2' = "Feb",
    '3' = "Mar",
    '4' = "Apr",
    '5' = "May",
    '6' = "Jun",
    '7' = "Jul",
    '8' = "Aug",
    '9' = "Sep",
    '10' = "Oct",
    '11' = "Nov",
    '12' = "Dec"
  )
  month_abr_eng[[month_number]]
}


# calculate_cat_year_sum <- function(dt) {
#   dt %>%
#     group_by(year, month, maincategory, subcategory) %>%
#     summarize(sum_month = sum(amount), .groups = "drop")
# }


to_wide_format <- function(dt) {
  dt %>%
    pivot_wider(
      id_cols = c("maincategory", "subcategory"),
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
    select(-c(maincategory, subcategory))
  
  allyear_dt <- tibble(.rows = nrow(month_dt))
  for (month in months) {
    if (month %in% colnames(month_dt)) {
      allyear_dt[[month]] = month_dt[[month]]
    } else {
      allyear_dt[[month]] = rep(NA, nrow(month_dt))
    }
  }
  
  allyear_dt_w_an <- dt %>%
    select(maincategory, subcategory) %>%
    bind_cols(allyear_dt)
  
  return(allyear_dt_w_an)
}


calculate_row_summary <- function(dt) {
  dt %>%
    rowwise() %>%
    mutate(avg = round(mean(c_across(Jan:Dec), na.rm = TRUE))) %>%
    mutate(total = sum(c_across(Jan:Dec), na.rm = TRUE)) %>%
    arrange(maincategory, desc(total))
}


calculate_col_summary <- function(dt) {
  totals_row <- dt %>%
    select(Jan:total) %>%
    colSums(.) %>%
    as.matrix() %>%
    t() %>%
    as_tibble() %>%
    mutate(maincategory = "totals",
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
