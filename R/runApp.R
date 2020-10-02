#' Shiny App for Health Finance
#' 
#' Opens the shiny interface for the health finance functionality
#' provided by the `healthfinance` package. The interface currently
#' consists of 3 tabs: (1) import; (2) model; and (3) export.
#' 
#' @return shiny application object
#' @import shiny
#' @import ggplot2
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom scales dollar
#' @importFrom lubridate month year
#' @export
hfin <- function() {
  app_dir <- system.file("hfin", package = "healthfinance")
  
  # something's gone wrong
  if (app_dir == "") {
    stop("Could not find app. Try re-installing the `healthfinance` package.",
         call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}