# Shiny app
# Sourced code ----
source("generateUI.R")
# Global variables ----
SQM <<- NULL
samples_path <<- "../shiny_input/" #input directory with samples as subdirectories
# sample tables are expected to be extracted and parsed with extract_lite.sh
data_path <<- "../shiny_data/" #input directory with data and metadata tables
# UI ----
ui <- generateUI()
  
# Server ----
server <- function(input, output, clientData, session) {
  # Increase maximum upload size
  options(shiny.maxRequestSize=10*1024^3)
  
  # Reactive values to store data
  reactiveData <- reactiveValues()
  
  # Load the SQM object and stats files ----
  proj_dir <- reactive(paste0(samples_path,input$project,"/"))
  observeEvent(input$proj_load,{
    tryCatch({
      showModal(modalDialog(title = "Loading", easyclose = TRUE))
      print("In tryCatch")
      reactiveData$SQM <- switch(input$type_load,
                                 "Load directly from SQM project" = {
                                   loadSQMlite(proj_dir())
                                 },
                                 "Load from pre-saved RDS file" = {
                                   readRDS(paste0(samples_path,input$project))
                                 })
      # Load the stats file
      # add check.names=FALSE to prevent R from changing column names
      print("In stats")
      if (input$type_load=="Load directly from SQM project"){
        reactiveData$reads_st <- read.csv(paste0(proj_dir(),paste0("22.reads.tsv")),header=TRUE,sep="\t")
        reactiveData$contigs_st <- read.csv(paste0(proj_dir(),paste0("22.contigs.tsv")),header=TRUE,sep="\t")
        reactiveData$taxa_st <- read.csv(paste0(proj_dir(),paste0("22.taxa.tsv")),header=TRUE,sep="\t")
        reactiveData$orfs_st <- read.csv(paste0(proj_dir(),paste0("22.orfs.tsv")),header=TRUE,sep="\t")
        reactiveData$bins_st <- read.csv(paste0(proj_dir(),paste0("22.bins.tsv")),header=TRUE,sep="\t")
      } else { # if loading from an rds, there are no stat files, yet empty objects would raise an error
        reactiveData$reads_st <- matrix()
        reactiveData$contigs_st <- matrix()
        reactiveData$taxa_st <- matrix()
        reactiveData$orfs_st <- matrix()
        reactiveData$bins_st <- matrix()
      }
      output$out_project <- renderText(isolate(input$project))
      showModal(modalDialog(title = "Loaded", "Your project is ready", easyclose = TRUE))
      print("Out tryCatch")
    }, warning = function(warn) {
      showModal(modalDialog(title = "Loading error", "Please check input type", easyclose = TRUE))
    }, error = function(error) {
      showModal(modalDialog(title = "Loading error", "Please check input type", easyclose = TRUE))
    }  ) # Close tryCatch
  }) # Close proj_load observer
  # Update Taxonomy Inputs ----
  observe({
    updateSelectInput(session, "rank_tax",
                      choices = names(reactiveData$SQM[["taxa"]])
    )
  }) # Close observer
  
  observe({
    updateSelectInput(session, "count_tax",
                      choices = names(reactiveData$SQM[["taxa"]][[input$rank_tax]])
    )
  }) # Close observer
  
  observe({
    updateNumericInput(session, "n_tax",
                      max = length(unique(rownames(
                        reactiveData$SQM[["taxa"]][[input$rank_tax]][[input$count_tax]]))),
                      value = 1
    )
    updateSelectizeInput(session, "tax_tax",
                         choices = unique(rownames(
                           reactiveData$SQM[["taxa"]][[input$rank_tax]][[input$count_tax]]),
                           multiple = TRUE)
    )
  }) # Close observer

  observe({updateSelectizeInput(session,"samples_tax", 
                       choices = reactiveData$SQM$misc$samples,
                       selecte = reactiveData$SQM$misc$samples
  )})
  
  # Output Taxonomy ----
  reactTaxPlot <- reactive({
    if (input$sel_tax){
      # Plot by names of taxa
      plotTaxonomy(reactiveData$SQM,
                   rank = input$rank_tax,
                   count = input$count_tax,
                   tax = input$tax_tax,
                   others = input$others_tax,
                   samples = input$samples_tax,
                   ignore_unmapped = input$unmapped_tax,
                   ignore_unclassified = input$unclass_tax,
                   no_partial_classifications = input$partial_tax,
                   rescale = input$rescale_tax,
                   base_size = input$base_size_tax)
    } else {
      # Plot by number of taxa
      plotTaxonomy(reactiveData$SQM,
                   rank = input$rank_tax,
                   count = input$count_tax,
                   N = input$n_tax,
                   others = input$others_tax,
                   samples = input$samples_tax,
                   ignore_unmapped = input$unmapped_tax,
                   ignore_unclassified = input$unclass_tax,
                   no_partial_classifications = input$partial_tax,
                   rescale = input$rescale_tax,
                   base_size = input$base_size_tax)
    }
  })  
  
  output$taxPlot <- renderPlot({
    print(reactTaxPlot())
  })
  
  output$taxPlotDown <- downloadHandler(
    filename = function() {paste("Tax_plot_", Sys.time(), '.pdf', sep='') },
    content = function(file) {
      ggsave(file,reactTaxPlot(),device="pdf")
    }
  )
  
  # Update Functions Inputs ----
  observe({
    updateSelectInput(session, "fun_level_fun", 
                      choices = names(reactiveData$SQM$functions),
    )
  })
  
  observe({
    updateSelectizeInput(session, "count_fun",
                         choices = names(reactiveData$SQM[["functions"]][[input$fun_level_fun]])
    )
  })
  
  observe({
    updateSelectizeInput(session, "samples_fun", 
                         choices = reactiveData$SQM$misc$samples,
                         selected = reactiveData$SQM$misc$samples
    )
  })
  
  observe({
    updateNumericInput(session, "n_fun",
                      max = length(unique(rownames(
                        reactiveData$SQM[["functions"]][[input$fun_level_fun]][[input$count_fun]])))
    )
  })
  
  observe({
    updateSelectizeInput(session, "fun_fun",
                         choices = unique(rownames(
                           reactiveData$SQM[["functions"]][[input$fun_level_fun]][[input$count_fun]])),
                         server = TRUE
    )
  })
  
  observeEvent(input$fun_level_fun,{
    updateSelectizeInput(session, 'fun_fun',
                         choices = rownames(
                           reactiveData$SQM[['functions']][[input$fun_level_fun]][[input$count_fun]]),
                         server = TRUE
    )
  }) # Close fun_level_fun observer
  # Output Functions ----
  reactFunPlot <- reactive({
    if (input$sel_fun){
      plotFunctions(reactiveData$SQM,
                    fun_level = input$fun_level_fun,
                    count = input$count_fun,
                    fun = input$fun_fun,
                    samples = input$samples_fun,
                    ignore_unmapped = input$unmapped_fun,
                    ignore_unclassified = input$unclass_fun,
                    base_size = input$base_size_fun)
    } else {
      plotFunctions(reactiveData$SQM,
                    fun_level = input$fun_level_fun,
                    count = input$count_fun,
                    N = input$n_fun,
                    samples = input$samples_fun,
                    ignore_unmapped = input$unmapped_fun,
                    ignore_unclassified = input$unclass_fun,
                    base_size = input$base_size_fun)
    }
  })
  
  output$funPlot <- renderPlot({
    print(reactFunPlot())
  })
  
  output$funPlotDown <- downloadHandler(
    filename = function() {
      paste("Fun_plot_", Sys.time(), '.pdf', sep='') 
    },
    content = function(file) {
      ggsave(file, reactFunPlot(), device="pdf")
    }
  )
  
  # Update Table Inputs ----
  observe({updateSelectInput(session,"lev1_tab",
                             choices = switch(class(reactiveData$SQM),
                                              "SQM" = c("orfs","contigs","bins","taxa","functions"),
                                              "SQMlite" = c("taxa","functions"),
                                              "Loaded object is not an SQM[lite] object"
                             ),
                             selected = ""
  )})
  
  observeEvent(input$lev1_tab,{
    updateSelectInput(session,"lev2_tab",
                      choices = names(reactiveData$SQM[[input$lev1_tab]]),
                      selected = ""
    )
  }) # Close observer
  
  observeEvent(c(input$lev1_tab,input$lev2_tab),{
    updateSelectInput(session,"lev3_tab",
                      choices = switch(input$lev1_tab,
                                       "orfs" = "This subsection does not have units",
                                       "contigs" = "This subsection does not have units",
                                       "bins" = "This subsection does not have units",
                                       names(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]])
                      ),
                      selected = ""
    )
  }) # Close lev1+2 observer
  
  observeEvent(input$lev3_tab,{
    updateSelectizeInput(session,"cols_tab",
                         choices = switch(input$lev1_tab,
                                          "orfs" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                          "contigs" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                          "bins" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                          "taxa" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]]),
                                          "functions" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]])
                         ),
                         selected = switch(input$lev1_tab,
                                           "orfs" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                           "contigs" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                           "bins" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]]),
                                           "taxa" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]]),
                                           "functions" = colnames(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]])
                         )
    )
  }) # Close lev3 observer
  
  # Output Table ----
  reactTable <- reactive({
    table <- switch(input$lev1_tab,
                    'orfs'      = as.data.frame(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][,input$cols_tab]),
                    'contigs'   = as.data.frame(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][,input$cols_tab]),
                    'bins'      = as.data.frame(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][,input$cols_tab]),
                    'taxa'      = as.data.frame(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]][,input$cols_tab]),
                    'functions' = as.data.frame(reactiveData$SQM[[input$lev1_tab]][[input$lev2_tab]][[input$lev3_tab]][,input$cols_tab])
    )
    
    if (input$lev2_tab %in% c("KEGG","COG")){
      annot <- switch(input$lev2_tab,
                      "KEGG" = "KEGG_names",
                      "COG"  = "COG_names")
      fun_names <- reactiveData$SQM[["misc"]][[annot]][rownames(table)]
      fun_names[length(fun_names)] <- "Unmapped"
      names(fun_names)[length(fun_names)] <- "Unmapped"
      table <- cbind(fun_names,table)
      names(table)[1] <- c("Function name")
    }
    table
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(reactTable())
  })
  
  output$tabDown <- downloadHandler(
    filename = function() {
      paste("Table", Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactTable(), file, row.names = TRUE)
    }
  )
  # Update Summary Inputs ----
  observe({
    updateSelectInput(session,"orfs_row1",
                      choices = reactiveData$orfs_st[,1]
    )        
    updateSelectInput(session,"orfs_row2",
                      choices = reactiveData$orfs_st[,1]
    )
  })
  # Output Summary ----
  # Reads
  reactReadsSum <- reactive({ #create reactive function
    as.data.frame(reactiveData$reads_st[,-1],row.names=reactiveData$reads_st[,1])
  })
  output$reads_sum <- DT::renderDataTable({ #generate output 
    DT::datatable(reactReadsSum(),options=list(
      paging=FALSE, searching=FALSE  
    )
    )
  })
  
  # Barplots from Reads
  output$reads_readbar <- renderPlot({
    barplot(as.matrix(reactReadsSum()[1,! names(reactReadsSum()) %in% c("Assembly")]), main="Reads", col="#008080", las=2)
  })
  output$reads_basebar <- renderPlot({
    barplot(as.matrix(reactReadsSum()[2,! names(reactReadsSum()) %in% c("Assembly")]), main="Bases", col="#00CED1", las=2)
  })
  
  # Contigs
  reactContigsSum <- reactive({ #create reactive function
    df <- as.data.frame(reactiveData$contigs_st[,-1],row.names=reactiveData$contigs_st[,1])
    if (ncol(df)==1){names(df) <- c("Value")}
    df
  })
  output$contigs_sum <- DT::renderDataTable({ #generate output 
    DT::datatable(reactContigsSum(),options=list(
      paging=FALSE, searching=FALSE  
    )
    )
  })
  
  # Taxa
  reactTaxaSum <- reactive({ #create reactive function
    as.data.frame(reactiveData$taxa_st[,-1],row.names=reactiveData$taxa_st[,1])
  })
  output$taxa_sum <- DT::renderDataTable({ #generate output 
    DT::datatable(reactTaxaSum(),options=list(
      paging=FALSE, searching=FALSE  
    )
    )
  })
  
  # Orfs
  reactOrfsSum <- reactive({ #create reactive function
    as.data.frame(reactiveData$orfs_st[,-1],row.names=reactiveData$orfs_st[,1])
  })
  output$orfs_sum <- DT::renderDataTable({ #generate output 
    DT::datatable(reactOrfsSum(),options=list(
      paging=FALSE, searching=FALSE  
    )
    )
  })
  
  # Barplots from Orfs
  output$orfs_bar1 <- renderPlot({
    barplot(as.matrix(reactOrfsSum()[input$orfs_row1,]), main=input$orfs_row1, 
            col="#FF7F50", ylim=c(0,max(reactOrfsSum()[input$orfs_row1,])), las=2)
  })
  output$orfs_bar2 <- renderPlot({
    barplot(as.matrix(reactOrfsSum()[input$orfs_row2,]), main=input$orfs_row2, 
            col="#4169E1", las=2)
  })
  
  # Bins
  reactBinsSum <- reactive({ #create reactive function
    df <- as.data.frame(reactiveData$bins_st[,-1],row.names=reactiveData$bins_st[,1])
    if (ncol(df)==1){names(df) <- c("Value")}
    df
  })
  output$bins_sum <- DT::renderDataTable({ #generate output 
    DT::datatable(reactBinsSum(),options=list(
      paging=FALSE, searching=FALSE  
    )
    )
  })
  # Update Multivariate Analysis Inputs ----
  observeEvent(input$lev1_ma,{
    updateSelectInput(session,"lev2_ma",
                      choices = names(reactiveData$SQM[[input$lev1_ma]]),
                      selected = ""
    )
  }) # Close lev1 observer
  
  observeEvent(input$lev2_ma,{
    updateSelectInput(session,"lev3_ma",
                      choices = names(reactiveData$SQM[[input$lev1_ma]][[input$lev2_ma]]),
                      selected = ""
    )
  }) # Close lev2 observer
  
  observeEvent(input$lev3_ma,{
    updateSliderInput(session, 'n_ma', value = 2,
                      max = length(unique(rownames(
                        reactiveData$SQM[[input$lev1_ma]][[input$lev2_ma]][[input$lev3_ma]]))) -1
                      # -1 because Unclassified will be removed
    )
    updateSelectizeInput(session, "var_ma",
                         choices = rownames(
                           reactiveData$SQM[[input$lev1_ma]][[input$lev2_ma]][[input$lev3_ma]]))
  }) # Close lev3 observer
  
  observeEvent(input$load_ma,{
    updateSelectizeInput(session,"samples_ma", 
                         choices = reactiveData$SQM$misc$samples,
                         selected = reactiveData$SQM$misc$samples
    )
  }) # Close load_ma observer
  
  # Auxiliary Multivariate Analysis ----
  # To add a new analysis, these are the steps:
  #     1. Add the analysis method as a ma_method choice
  #     2. Add the associated conditional panel for the parameters
  #     3. Add the auxiliary function to obtain the ordination
  #     4. Add the plotting function within reactMaPlot (and plotting function)
  
  # Functions to make the data to plot
  pca_ord <- function(data_in){
    pca_res = rda(data_in)
    return(pca_res)
  }
  
  pcoa_ord <- function(data_in){
    pcoa_res = wcmdscale(vegdist(data_in, method=input$dist_pcoa))
    return(pcoa_res)
  }
  
  nmds_ord <- function(data_in){
    nmds_res = metaMDS(data_in)
    return(nmds_res)
  }

  # Load Data Multivariate Analysis ----
  
  # Select and load dataset
  # Update filters
  observeEvent(input$load_ma,{
    if (input$dataset_ma == "Current project"){
      output$out_dataset_ma <- renderText(isolate(input$project))
      updateSelectizeInput(session, "lev1_ma",
                           choices = c("","functions","taxa"),
                           selected = "")
    } else {
      tryCatch({
        reactiveData$temp_ma_data <- read.csv(file = paste0(data_path,input$dataset_ma),
                                 header = input$head_data_ma,
                                 row.names = switch(input$rown_data_ma, "TRUE" = 1, "FALSE" = NULL),
                                 sep = switch(input$sep_data_ma, ".csv" = ",", ".tsv" = "\t"))
      }, warning = function(warn) {
        showModal(modalDialog(title = "Loading error", "Please check table format", easyclose = TRUE))
      }) # Close tryCatch
      updateSelectizeInput(session, "rows_ma",
                           choices = rownames(reactiveData$temp_ma_data),
                           selected = rownames(reactiveData$temp_ma_data))
      updateSelectizeInput(session, "cols_ma",
                           choices = colnames(reactiveData$temp_ma_data),
                           selected = colnames(reactiveData$temp_ma_data))
      output$out_dataset_ma <- renderText(isolate(input$dataset_ma))
    }
  }) # Close load_ma observer
  
  # Filter and Generate Tables for Multivariate Analysis ----
  # Reactive functions to generate the data when action buttons are hit
  # Generates the table with the raw data
  reactMaData <- eventReactive(input$filter_ma,{
    if (input$dataset_ma == "Current project"){
      ma_data <- reactiveData$SQM[[input$lev1_ma]][[input$lev2_ma]][[input$lev3_ma]]
      if (input$sel_var_ma){
        ma_data <- t(ma_data[input$var_ma,input$samples_ma])
      } else {
        ma_abun <- rownames(mostAbundant(ma_data, input$n_ma+1)) # +1 because Unclassified will be removed
        ma_abun <- ma_abun[ma_abun!='Unclassified']
        ma_data <- t(ma_data[ma_abun,input$samples_ma])
      }
    } else {
      ma_data <- reactiveData$temp_ma_data
      ma_data[is.na(ma_data)] <- 0
      ma_data <- ma_data[input$rows_ma,input$cols_ma]
    }
    ma_data
  })
  # Store it into a reactive value
  observeEvent(input$filter_ma,{
    reactiveData$curr_ma_data <- reactMaData() # Analysed data
    reactiveData$org_ma_data <- reactMaData() # Displayed data
    })
  
  # Output Multivariate Analysis ----
  # Output tables in Data selection
  output$table_ma <- DT::renderDataTable({
    DT::datatable(reactiveData$org_ma_data)
  })
  
  # Reactive Plotting Function
  # try ggrepel for not overlapping text labels (works with ggplot)
  reactMaPlot <- eventReactive(input$ma_plot,{
    if (input$ma_method == "PCA"){
      pca_result <- pca_ord(reactiveData$curr_ma_data)
      plot(pca_result,type = "n", 
           xlab = paste0(
             "PC1 (",round(summary(pca_result)$cont$importance[2,1],2)*100,"%)"
           ), ylab = paste0(
             "PC2 (",round(summary(pca_result)$cont$importance[2,2],2)*100,"%)"
           )
      )
      points(pca_result,display = "species", pch = 21)
      text(pca_result, display = "sites", cex = 0.8)
    }
    if (input$ma_method == "PCoA (MDS)"){
      ordiplot(pcoa_ord(reactiveData$curr_ma_data), type = 't', display = 'sites')
    }
    if (input$ma_method == "NMDS"){
      ordiplot(nmds_ord(reactiveData$curr_ma_data), type = 't',display = 'sites')
    }
  })
  
  output$maPlot <- renderPlot({
    print(reactMaPlot())
  })
  
  output$maPlotDown <- downloadHandler(
    # Should be able to use the reactive functions 
    # but this generates nothing
    filename = function() {
      paste("Ma_plot_", Sys.time(), '.pdf', sep='') 
    },
    content = function(file) {
      pdf(file = file)
      if (input$ma_method == "PCA"){
        pca_result <- pca_ord(reactiveData$curr_ma_data)
        plot(pca_result,type = "n", 
             xlab = paste0(
               "PC1 (",round(summary(pca_result)$cont$importance[2,1],2)*100,"%)"
             ), ylab = paste0(
               "PC2 (",round(summary(pca_result)$cont$importance[2,2],2)*100,"%)"
             )
        )
        points(pca_result,display = "species", pch = 21)
        text(pca_result, display = "sites", cex = 0.8)
      }
      if (input$ma_method == "PCoA (MDS)"){
        ordiplot(pcoa_ord(reactiveData$curr_ma_data), type = 't', display = 'sites')
      }
      if (input$ma_method == "NMDS"){
        ordiplot(nmds_ord(reactiveData$curr_ma_data), type = 't',display = 'sites')
      }
      dev.off()
    }
  )
} # Close server
