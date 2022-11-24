# Create Shiny app
SQMxplore <- function() {
    shinyApp(ui = generate_ui(), server = server)
}