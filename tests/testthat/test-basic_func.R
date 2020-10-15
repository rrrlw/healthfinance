test_that("default function params", {
  # parameters for testing
  TOLERANCE <- 1e-4
  PRECALCULATED <- 779.1667
  
  # sample data
  tester <- data.frame(Name = c("Proc 1", "Proc 2", "Proc 3"),
                       Volume = c(1000, 2000, 3000),
                       Revenue = c(250, 100, 500))
  
  # calculate revenue
  rev <- healthfinance::calc_rev(procedures = tester)
  
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
  
})
