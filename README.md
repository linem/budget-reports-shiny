# budget-reports-shiny

### Input data
Input file(s) must be in csv format and contain at least 4 columns
- date
	- ISO 8601 format, YYYY-MM-DD
- amount
	- negative number for expenses, positive number for income
- maincategory
	- annotation for main category
- subcategory
	- annotation for sub category
Place any/all input files together in a directory

Tip: Use `reimbursement` as maincategory for income that should instead reduce expenses.

### Format input data
Run the helper script `utils/prepare_data.R` to process the input data for use in budget-reports-shiny app.
```
./utils/prepare_data.R input_directory

# help
flags:
  -h, --help          show this help message and exit
  -w, -w              allowed to overwrite output file

optional arguments:
  -x, --opts          RDS file containing argument values
  -d, --date_col      column name for the date [default: date]
  -a, --amount_col    column name for the amount [default: amount]
  -m, --maincat_col   column name for the main category [default:
                      maincategory]
  -s, --subcat_col    column name for the sub category [default:
                      subcategory]
  -o, --output_fname  name of the output file in data folder [default:
                      transactions_YYMMDD.csv]
```



### To do
- Handle missing values
- Handle wrong format

