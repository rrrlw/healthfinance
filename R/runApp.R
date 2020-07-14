#' Shiny App for Health Finance
#' 
#' Opens the shiny interface for the health finance functionality
#' provided by the `healthfinance` package.
#' 
#' @return shiny application object
#' @import shiny
#' @import ggplot2
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