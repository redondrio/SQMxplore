# Libraries ----
library(shiny)
library(SQMtools)
library(ggplot2)
library(DT)
library(data.table)
library(vegan)
library(caret)
library(compositions)

# Create Shiny app
SQMlens <- function(){
    source("lone_app.R")
    shinyApp(ui = ui, server = server)
}