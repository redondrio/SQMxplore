# Create Shiny app
SQMxplore <- function(){
    source("app.R")
    shinyApp(ui = ui, server = server)
}
