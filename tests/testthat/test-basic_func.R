# parameters for testing
TOLERANCE <- 1e-4
PRECALCULATED <- 779.1667
GROWTH <- 0.05

# sample data
tester <- data.frame(Name = c("Proc 1", "Proc 2", "Proc 3"),
                     Volume = c(1000, 2000, 3000),
                     Revenue = c(250, 100, 500))

test_that("default function params", {
  # calculate revenue
  rev <- calc_rev(procedures = tester)
  
  # confirm output w/ precalculated output (manually confirmed?)
  placeholder <- vapply(X = rev$Target,
                        FUN.VALUE = logical(1),
                        FUN = function(i) {
                          # check equality
                          cond <- abs(i - PRECALCULATED) < TOLERANCE
                          
                          expect_true(cond)
                          
                          # needed for vapply
                          return(cond)
                        })
  placeholder <- vapply(X = rev$Projected,
                        FUN.VALUE = logical(1),
                        FUN = function(i) {
                          # check equality
                          cond <- abs(i - PRECALCULATED) < TOLERANCE
                          
                          expect_true(cond)
                          
                          # needed for vapply
                          return(cond)
                        })
})

test_that("growth param", {
  # calculate revenue
  rev <- calc_rev(procedures = tester,
                  growth = rep(GROWTH * 100, 3))
  
  # confirm output w/ precalculated output (manually confirmed?)
  for (i in 1:36) {
    # calculate amount of growth needed for current month
    num_periods <- ceiling(i / 12)
    growth_val <- (1 + GROWTH) ^ num_periods
    curr_expected <- PRECALCULATED * growth_val
    
    # check target
    cond <- abs(curr_expected - rev$Target[i]) < TOLERANCE
    expect_true(cond)
    
    # check projected (not changing any other params, so should equal Target)
    expect_equal(rev$Target[i], rev$Projected[i])
  }
})
