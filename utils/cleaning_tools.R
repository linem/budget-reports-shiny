

check_columns <- function(dt) {
  if (!(class(dt$date) == "Date")) {
    stop("date column must be a date format, use iso 8601 (YYYY-MM-DD)")
  }
  if (!(class(dt$amount) == "numeric")) {
    stop("amount column must be in numeric format, positive number for income, negative number for expense")
  }
  if (!(class(dt$maincategory) == "character")) {
    stop("main category column must be in character format")
  }
  if (!(class(dt$subcategory) == "character")) {
    stop("sub category column must be in character format")
  }
  return(dt)
}


add_transaction_type <- function(dt) {
  dt %>%
    mutate(
      transtype = ifelse(
        str_detect(amount, "-"),
        "expense",
        "income"
      )
    )
}


format_amount <- function(dt) {
  dt %>%
    mutate(amount = abs(amount)) %>%
    mutate(amount = round(amount))
}


# add_month_and_year <- function(dt) {
#   dt %>%
#     mutate(date = ymd(date)) %>%
#     mutate(
#       month = as_date(
#         paste(
#           year(date),
#           month(date),
#           "01",
#           sep = "-"
#         )
#       )
#     ) %>%
#     mutate(year = as_date(
#       paste(
#         year(date),
#         "01-01",
#         sep = "-"
#       )
#     )
#   )
# }
add_month_and_year <- function(dt) {
  dt %>%
    mutate(date = ymd(date)) %>%
    mutate(
      month = as_date(
        glue::glue("{year(date)}-{month(date)}-01")
      )) %>%
    mutate(year = as_date(
      glue("{year(date)}-01-01")
    ))
}

