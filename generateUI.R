generateUI <- function(){
    ui <- navbarPage("SQMxplore",
               # Page Input ----
               tabPanel("Dataset",
                        fluidPage("",
                                  mainPanel(
                                      # If an option is added, include it in the switch at the load section
                                      selectInput("type_load","Select type of input",
                                                  choices=c("Load directly from SQM project",
                                                            "Load from pre-saved RDS file")),
                                      h1(),
                                      selectInput("project","Select project",
                                                  choices=list.files(samples_path)),

                                      h5("Currently loaded project:"),
                                      verbatimTextOutput("out_project",placeholder=TRUE),

                                      h1(),
                                      actionButton("proj_load","Load project")
                                  ) # Close panel
                        ) # Close layout
               ), # Close Input page

               # Page Summary ----
               tabPanel("Summary",
                        # Sidebar layout with input and output definitions
                        fluidPage("", h1("Summary page"),
                                  # Display Table Output
                                  conditionalPanel(
                                      condition = "input.type_load=='Load directly from SQM project'",
                                      h3("Reads summary"),
                                      DT::dataTableOutput("reads_sum"),
                                      fluidRow(
                                          column(6,plotOutput(outputId = "reads_readbar")),
                                          column(6,plotOutput(outputId = "reads_basebar"))
                                      ),

                                      h3("Contigs summary"),
                                      DT::dataTableOutput("contigs_sum"),

                                      h3("Taxa summary"),
                                      DT::dataTableOutput("taxa_sum"),

                                      h3("Orfs summary"),
                                      DT::dataTableOutput("orfs_sum"),
                                      fluidRow(
                                          column(6,selectInput("orfs_row1", "Choose a value to display",
                                                               choices = NA)),
                                          column(6,selectInput("orfs_row2", "Choose a value to display",
                                                               choices = NA))
                                      ),
                                      fluidRow(
                                          column(6,plotOutput(outputId = "orfs_bar1")),
                                          column(6,plotOutput(outputId = "orfs_bar2"))
                                      ),

                                      h3("Bins summary"),
                                      DT::dataTableOutput("bins_sum")
                                  ), # Close conditional panel
                                  conditionalPanel(
                                      condition = "input.type_load!='Load directly from SQM project'",
                                      h5("This object does not include summary tables")
                                  )
                        ) # Close layout
               ), # Close page
               # Page Tables ----
               tabPanel("Tables",
                        # Sidebar layout with input and output definitions
                        fluidPage("DataTable",
                                  # Row panel for inputs
                                  fluidRow(
                                      # Input:
                                      column(3,
                                             selectInput("lev1_tab", "Choose a section to display", #updated
                                                         choices = "", selected = ""),
                                             selectInput("lev2_tab", "Choose a subsection to display", #updated
                                                         choices = ""),
                                             selectInput("lev3_tab", "Choose a unit to display", #updated
                                                         choices = ""),
                                             downloadButton('tabDown', 'Download Table')
                                      ),
                                      column(8,
                                             selectizeInput("cols_tab","Choose columns to display", #updated
                                                            choices = NA,
                                                            multiple = TRUE)
                                      )
                                  ), # Close row panel

                                  # Display Table Output
                                  DT::dataTableOutput("table")
                        ) # Close layout
               ), # Close page
               # Page Taxonomy ----
               tabPanel("Plot Taxonomy",
                        sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                h4("Data"),
                                # Input: Menu for the rank, units and samples
                                selectInput("rank_tax", "Choose a rank to display", #updated
                                            choices = "", selected = ""),
                                selectInput("count_tax", "Choose a unit to display", #updated
                                            choices = "", selected = ""),
                                selectizeInput("samples_tax", "Select samples", #updated
                                               choices = NULL,
                                               selecte = NULL,
                                               multiple = TRUE),

                                h4("Taxa"),
                                # Input: Slider for the number of taxa
                                numericInput("n_tax", "Choose the number of taxa", #updated
                                            value = 1, min = 0),
                                # Input: Text input to specify taxa and checkbox to override N
                                checkboxInput("sel_tax", "Select displayed taxa",
                                              value = F),
                                selectizeInput("tax_tax", "Selected taxons", #updated
                                               choices = NULL,
                                               multiple = TRUE),

                                h4("Options"),
                                checkboxInput("others_tax", "Show other reads",
                                              value = T),
                                conditionalPanel(
                                    condition = "!input.sel_tax",
                                    checkboxInput("unmapped_tax", "Ignore unmapped reads",
                                                  value = F),
                                    checkboxInput("unclass_tax", "Ignore unclassified reads",
                                                  value = F),
                                    checkboxInput("partial_tax", "Ignore partial classifications",
                                                  value = F),
                                ),
                                checkboxInput("rescale_tax", "Rescale to 100%",
                                              value = F),
                                numericInput("base_size_tax", "Change font size",
                                             value = 11)
                            ), # Close sidebar panel

                            # Main panel for displaying outputs ----
                            mainPanel(
                                plotOutput(outputId = "taxPlot"),
                                downloadButton('taxPlotDown', 'Download Plot')
                            ) # Close main panel
                        ) # Close layout
               ), # Close Taxonomy page

               # Page Functions ----
               tabPanel("Plot Functions",
                        sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                h4("Data"),
                                # Input: Menu for the rank, units and samples
                                selectInput("fun_level_fun", "Choose an annotation to display", #updated
                                            choices = "", selected = ""),
                                selectInput("count_fun", "Choose a unit to display", #updated
                                            choices = "", selected = ""),
                                selectizeInput("samples_fun", "Select samples", #updated
                                               choices = NULL,
                                               selected = NULL,
                                               multiple = TRUE),

                                h4("Functions"),
                                # Input: Slider for the number of taxa
                                numericInput("n_fun", "Choose the number of functions",
                                            value = 1, min = 0), #updated
                                # Input: Text input to specify taxa and checkbox to override N
                                checkboxInput("sel_fun", "Select functions",
                                              value = F),
                                selectizeInput("fun_fun", "Name of the function", #updated
                                               choices = NULL,
                                               selected  = NULL,
                                               multiple = TRUE),

                                h4("Options"),
                                checkboxInput("unmapped_fun", "Ignore unmapped reads",
                                              value = F),
                                checkboxInput("unclass_fun", "Ignore unclassified reads",
                                              value = F),
                                numericInput("base_size_fun", "Change font size",
                                             11)
                            ), # Close sidebar panel

                            # Main panel for displaying outputs ----
                            mainPanel(
                                # Output: Histogram
                                plotOutput(outputId = "funPlot"),
                                downloadButton("funPlotDown", "Download Plot")
                            ) # Close main panel
                        ) # Close layout
               ) # Close Functions page
    ) # Close UI
    ui
} # Close function
