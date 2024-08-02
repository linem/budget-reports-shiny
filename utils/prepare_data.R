#!/usr/bin/env Rscript

suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("glue"))
suppressPackageStartupMessages(library("tools"))

source("utils/cleaning_tools.R")
source("utils/file_tools.R")


parser <- arg_parser(
  "Prepare transactions files for budget-reports shiny app"
)

parser <- add_argument(
  parser,
  "input_dir",
  help = "directory of input files",
)
parser <- add_argument(
  parser,
  "--date_col",
  help = "column name for the date",
  default = "date"
)
parser <- add_argument(
  parser,
  "--amount_col",
  help = "column name for the amount",
  default = "amount"
)
parser <- add_argument(
  parser,
  "--maincat_col",
  help = "column name for the main category",
  default = "maincategory"
)
parser <- add_argument(
  parser,
  "--subcat_col",
  help = "column name for the sub category",
  default = "subcategory"
)
parser <- add_argument(
  parser,
  "--output_fname",
  help = "name of the output file in data_finished folder [default: transactions_YYYY-MM-DD.csv]"
)
parser <- add_argument(
  parser,
  "-w",
  help = "allowed to overwrite output file",
  flag = TRUE
)

argv <- parse_args(parser)



files <- list.files(argv$input_dir, full.names = TRUE)
if (length(files) == 0) {
  stop("no input files found in the input directory")
}

column_names <- c(
  "amount" = argv$amount_col,
  "date" = argv$date_col,
  "maincategory" = argv$maincat_col,
  "subcategory" = argv$subcat_col
)

parse_file <- function(fp, column_names) {
  read_csv(fp, show_col_types = FALSE) %>%
    select(all_of(column_names)) %>%
    check_columns() %>%
    add_transaction_type() %>%
    format_amount() %>%
    add_month_and_year() %>%
    select(
      date, month, year, transtype, amount, maincategory, subcategory
    )
}

transactions <- map(files, parse_file, column_names) %>%
  bind_rows()

output_dir <- "data_finished"
create_dir_if_missing(output_dir)

output_file <- get_final_output_filename(
  output_dir,
  argv$output_fname,
  argv$w
)

check_output_file(output_file, argv$w)

write_csv(transactions, file = output_file)
print(glue::glue("Finished. Output written to {output_file}"))
