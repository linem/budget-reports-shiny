# for subcategory and month, decrease expense by reimbursement

load_data <- function(fp) {

  dt <- read_csv(fp, show_col_types = FALSE)
  
  income_dt <- dt %>%
    filter(transtype == "income" & maincategory != "reimbursement") %>%
    arrange(month, subcategory) %>%
    group_by(month, subcategory) %>%
    mutate(
      sum_month = sum(amount)
    ) %>%
    filter(row_number() == 1) %>%
    select(month, transtype, maincategory, subcategory, sum_month)
  
  reimbursement_dt <- dt %>%
    filter(transtype == "income" & maincategory == "reimbursement") %>%
    arrange(month, subcategory) %>%
    group_by(month, subcategory) %>%
    mutate(
      sum_reimbursement = sum(amount)
    ) %>%
    filter(row_number() == 1) %>%
    select(month, subcategory, sum_reimbursement)
  
  expenses_dt <- dt %>%
    filter(transtype == "expense") %>% 
    arrange(month, subcategory) %>%
    group_by(month, subcategory) %>%
    mutate(
      sum_exp = sum(amount)
    ) %>%
    filter(row_number() == 1) %>%
    select(-date, -amount)
  
  transactions_dt <- full_join(
    expenses_dt,
    reimbursement_dt,
    by = join_by(month, subcategory)
    ) %>%
    mutate_at(c("sum_exp", "sum_reimbursement"), replace_na, 0) %>%
    mutate(sum_month = sum_exp - sum_reimbursement) %>%
    select(month, transtype, maincategory, subcategory, sum_month) %>%
    bind_rows(income_dt) %>%
    mutate(year = year(month)) %>%
    arrange(month, transtype)
  
  return(transactions_dt)
}
  
