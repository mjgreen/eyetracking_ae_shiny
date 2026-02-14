library(shiny)
library(shinyjs) 
library(bslib)
library(DT)
library(deldir)
library(jpeg)
library(dplyr)
library(renv)
library(readr)

# Non-reactive functions

# Format timestamps nicely
safe_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}
