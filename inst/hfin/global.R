#####SETUP#####
# library("tidyverse")

# utility functions on which workhorses rely

#####VALIDATION FUNcTIONS#####
# make sure item info is valid for addition
assert_rowadd <- function(name, vol, rev, names) {
  
}

# make sure row number is valid
assert_rownum <- function(rownum) {
  
}

# make sure input file is valid
assert_inputfile <- function(file_path) {
  
}

# make sure all parameters are valid
assert_params <- function(procedures, growth, comp_ratio, ins_prop,
                          tech_fee_mult, month_prop, restoration, boost_amt,
                          boost_proc, boost_start, boost_end) {
  
}

#####UTILITY FUNCTIONS#####
# assume params already assert/validated
calc_reimburse_mult <- function(comp_ratio, #compensation relative to Mcare
                                ins_prop,   #pt payor mix
                                tech_fee_ratio) {
  sum(comp_ratio() * ins_prop()) * (1 + tech_fee_ratio())
}

# constants for shiny app
CURRENCY_DIGITS <- 13
MODEL_MONTHS <- 36

# function to customize ggplot2 axis lables
custom_dollar <- function(x) {
  scales::dollar(x,
                 scale = 1e-6,
                 largest_with_cents = 0,
                 suffix = "M")
}
text_to_currency <- function(num) {
  # check parameter validity
  if (!is.numeric(num)) stop(paste("Invalid parameter passed to text_to_currency:",
                                   num))
  
  # preserve options
  temp_prev <- options()$scipen
  options(scipen = CURRENCY_DIGITS)
  
  # convert to text
  ans <- as.character(round(num, 2))
  
  # revert options and return answer
  options(scipen = temp_prev)
  return(ans)
}
str_to_vec_year <- function(txt) {
  ans <- as.numeric(
    strsplit(x = txt,
             split = ",",
             fixed = TRUE)[[1]]
  )
  # confirm there's exactly 12 non-NA, numeric elements
  if (length(ans) == 12 &
      sum(is.na(ans)) == 0) {
    return(ans)
  } else {
    stop(paste("Problem with passed parameter:",
               txt))
  }
}

# export model data to CSV file
export_model <- function(df, path) {
  if (file.exists(path)) {
    readr::write_csv(df, path, append = TRUE)
  } else {
    readr::write_csv(df, path)
  }
}