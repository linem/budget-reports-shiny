create_dir_if_missing <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
}

get_final_output_filename <- function(output_dir, output_fname, overwrite_flag) {
  if (is.na(output_fname)) {
    todays_date <- format(Sys.Date(), "%Y-%m-%d")
    out_file <- file.path(output_dir, glue::glue("transactions_{todays_date}.csv"))
  } else {
    fname_no_ext <- tools::file_path_sans_ext(output_fname)
    out_file <- file.path(output_dir, glue::glue("{fname}.csv"))
  }
  return(out_file)
}

check_output_file <- function(output_file, overwrite_flag) {
  if (!overwrite_flag) {
    if (file.exists(output_file)) {
      stop("Output file already exist. Change filename or add flag -w to allow overwrite.")
    }
  }
}