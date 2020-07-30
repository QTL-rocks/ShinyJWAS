#setwd("/Users/wangzigui/Documents/Animal/Shiny/my_shiny/inst/shiny_app")
# Define UI ----
rm(list=ls())
# Load required packages
library(shiny)
library(shinyFiles)
library(rclipboard)
library(ggplot2)
#library(lattice)
library(coda)



# Run the app ----
shinyApp(ui = ui, server = server)

