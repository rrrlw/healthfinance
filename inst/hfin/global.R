#####SETUP#####
# library("tidyverse")

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

# calculate monthly revenue from initial data
# returns list containing 36 months revenue data (target & projected)
# assumes all parameters are perfect & valid - perform data validation BEFORE this
calc_rev <- function(procedures,                    #- df or tibble containing 3 columns (name, annual volume, annual revenue)
                     growth = rep(0, 3),            #- default = 0% for all 3 years
                     comp_ratio = rep(1, 4),        #- Mcaid, Commercial, Other
                     ins_prop = rep(0.25, 4),       #- Mcare, Mcaid, Commercial
                     tech_fee_mult = 10,            #- encapsulates all other Mcare revenue?
                     month_prop = rep(1/12, 12),    #- default: 1/12 each month
                     restoration = rep(1, 36),      #- default: no change for 36 months
                     boost_amt = numeric(0),        #- default: no boost for all 8
                     boost_proc = list(),           #- default: nothing boosted
                     boost_start = integer(0),      #- default: start jan-2020
                     boost_end = integer(0)) {      #- default: end dec-2022
  
  # make sure parameters are valid
  # assert_that("Name" %in% colnames(procedures) &
  #               "Volume" %in% colnames(procedures) &
  #               "Revenue" %in% colnames(procedures))
  # assert_that(is.character(procedures$Name) &
  #               is.numeric(procedures$Volume) &
  #               is.numeric(procedures$Revenue))
  # assert_that(is.numeric(growth) & length(growth) == 3)
  # assert_that(is.numeric(comp_ratio()) & length(comp_ratio()) == 4)
  # assert_that(is.numeric(ins_prop) & length(ins_prop) == length(comp_ratio))
  # assert_that(is.numeric(tech_fee_mult))
  # assert_that(is.numeric(month_prop) & length(month_prop) == 12)
  # assert_that(is.numeric(restoration) & length(restoration) == 3 * length(month_prop))
  # assert_that(is.numeric(boost_amt) &
  #               is.numeric(boost_start) &
  #               is.numeric(boost_end) &
  #               is.list(boost_proc))
  # for (j in boost_proc) {
  #   assert_that(is.character(j))
  # }
  # assert_that(length(boost_amt) == length(boost_start) &
  #               length(boost_start) == length(boost_end) &
  #               length(boost_end) == length(boost_proc))
  
  # preprocessing w/ boost (remove NA)
  boost_amt[is.na(boost_amt)] <- 0
  
  procedures$Avg <- procedures$Revenue / procedures$Volume
  num_proc <- nrow(procedures)
  target_vol <- matrix(nrow = num_proc, ncol = 36)
  rownames(target_vol) <- procedures$Name
  colnames(target_vol) <- 1:36
  
  # setup monthly volumes for each year, accounting for growth
  for (i in 1:12) {
    target_vol[, i] <- procedures$Volume * month_prop[i] * (1 + growth[1] / 100)
  }
  for (i in 13:24) {
    target_vol[, i] <- target_vol[, i - 12] * (1 + growth[2] / 100)
  }
  for (i in 25:36) {
    target_vol[, i] <- target_vol[, i - 12] * (1 + growth[3] / 100)
  }
  
  # account for insurance + tech fee reimbursement multiplier
  reimb_mult <- sum(comp_ratio * ins_prop) * (1 + tech_fee_mult)
  target_vol <- target_vol * reimb_mult
  
  # make modifications for projected volume based on boost + restoration
  project_vol <- target_vol
  
  # account for restoration
  for (i in 1:36) {
    project_vol[, i] <- project_vol[, i] * restoration[i]
  }
  
  # add boosts
  if (length(boost_proc) > 0) {
    for (i in 1:length(boost_proc)) {
      curr_start <- boost_start[i]
      curr_end <- boost_end[i]
      curr_amt <- boost_amt[i]
      for (j in boost_proc[[i]]) {
        if (!(j %in% rownames(project_vol))) {
          print(paste(j, "not valid procedure, skipping"))
          next
        }
        project_vol[j, curr_start:curr_end] <- project_vol[j, curr_start:curr_end] * (1 + curr_amt)
      }
    }
  }
  
  # convert volumes to revenues
  for (i in 1:nrow(target_vol)) {
    target_vol[i, ] <- target_vol[i, ] * procedures$Avg[i]
    project_vol[i, ]<- project_vol[i, ] * procedures$Avg[i]
  }
  
  # sum up revenues by month
  target_rev <- numeric(ncol(target_vol))
  project_rev<- numeric(ncol(project_vol))
  for (i in 1:ncol(target_vol)) {
    target_rev[i] <- sum(target_vol[, i])
    project_rev[i]<- sum(project_vol[, i])
  }
  
  list("Target" = target_rev,
       "Projected" = project_rev)
}

# constants
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