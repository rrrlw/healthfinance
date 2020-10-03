# calculate monthly revenue from initial data
# returns list containing 36 months revenue data (target & projected)
# assumes all parameters are perfect & valid - perform data validation BEFORE this
#' Calculate 3-year Revenue for Healthcare Practice
#' 
#' Allows calculation of projected revenue for upcoming 36 months along with
#' target revenue for corresponding months.
#' 
#' @param procedures df or tibble containing 3 columns (name, annual volume, annual revenue)
#' @param growth numeric vector of length 3; c(1, 10, 100) would represent
#'   expected growth of 1 percent in year 1, 10 percent in year 2
#'   (compared to year 1), and 100 percent in year 3 (compared to year 2)
#' @param comp_ratio numeric vector of length 4 containing compensation ratio
#'   (on average) of following insurances relative to Medicare: Medicare
#'   (should be 1), Medicaid, Commercial (private), and Other (self-pay, bad
#'   debt)
#' @param ins_prop numeric vector of length 4 containing proportion of
#'   patients with following types of insurance: Medicare, Medicaid,
#'   Commercial (private), and Other (self-pay, bad debt); sum of
#'   this vector should equal unity
#' @param tech_fee_mult technical fee as a multiple of procedural fee
#' @param month_prop proportion of revenue expected in each of 12 months of
#'   the year
#' @param restoration proportion of expected revenue expected in each of
#'   36 upcoming months due to acute economic event being modeled
#' @param boost_amt boost amount for up to 8 procedure sets
#' @param boost_proc list of boost procedures for each of 8 boosts above
#' @param boost_start start month (between 1 and 36, inclusive) for each of
#'   8 boosts above
#' @param boost_end end month (between 1 and 36, inclusive) for each of 8
#'   boosts above
#' @return list with 2 numeric vectors of length 36 each
#' @export
#' @examples 
#' 
#' # sample dataset of procedures
#' eg_procs <- data.frame(Name = c("Sample 1", "Sample 2", "Sample 3"),
#'                        Revenue = c(100000, 200000, 150000),
#'                        Volume = 1000, 25, 750)
#' 
#' # calculate revenue projections for next 36 months with default parameters
#' proj <- calc_rev(eg_procs)
#' 
#' # print 36-month target revenues
#' print(proj$Target)
#' 
#' # print 36-month projected revenues
#' print(proj$Projected)
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