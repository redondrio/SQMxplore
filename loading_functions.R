loadProject <- function(){
    SQM <- switch(input$type_load,
           "Load SQMlite from minimum tables" = {
               loadSQMlite(proj_dir())
           },
           "Load longreads project" = {
               loadSQMlite(proj_dir())
           },
           "Load project from RDS file" = {
               readRDS(paste0(samples_path,input$project))
           })
    SQM
}