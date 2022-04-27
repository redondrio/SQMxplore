# Create Shiny app
SQMxplore <- function(){
    shinyApp(ui = generateUI(), server = server)
}
