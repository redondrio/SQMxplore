generateUI <- function(){
    ui <- navbarPage("SQMxplore",
               # Page Input ----
               tabPanel("Dataset",
                        fluidPage("",
                                  mainPanel(
                                      # If an option is added, include it in the switch at the load section
                                      selectInput("type_load","Select type of input",
                                                  choices=c("Load SQMlite from minimum tables",
                                                            "Load longreads project",
                                                            "Load project from RDS file")),
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
                                      condition = "input.type_load=='Load SQMlite from minimum tables'",
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
                                      condition = "input.type_load!='Load SQMlite from minimum tables'",
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
               ), # Close Functions page
               # Page Multivariate Analysis ----
               tabPanel("Multivariate Analysis",
                        sidebarLayout(
                            # Sidebar panel for inputs
                            sidebarPanel(
                                # Input Panel About ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==1",
                                    p("This is the Multivariate Analysis tab."),
                                    p("Here you can explore your data with a set of ordination techniques."),
                                    p("Please bare in mind that this section was designed only for data exploration purposes,
                                    and it is not an exhaustive collection of all multivariate analysis methods.
                                    For this reason, the most appropriate analysis or parameters for your data may not be available.")
                                ),

                                # Input Panel Data Selection ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==2",
                                    h2("Load input data"),
                                    selectInput("dataset_ma","Select dataset",
                                                choices=c("Current project",list.files(data_path)),
                                                selected="Current project"),
                                    conditionalPanel(
                                        condition = "input.dataset_ma!='Current project'",
                                        checkboxInput("head_data_ma", "Input table includes header", value = T),
                                        checkboxInput("rown_data_ma", "Input table includes rownames", value = T),
                                        selectInput("sep_data_ma", "Format",
                                                    choices=c(".csv",".tsv"), selected = ".csv")
                                    ),
                                    actionButton("load_ma","Load dataset"),
                                    h5("Current dataset:"),
                                    verbatimTextOutput("out_dataset_ma",placeholder=TRUE),

                                    h2("Subset input data"),
                                    conditionalPanel(
                                        condition = "input.dataset_ma=='Current project'",
                                        selectInput("lev1_ma", "Choose a set of data to analyse",
                                                    choices = ""),
                                        selectInput("lev2_ma", "Choose a subsection to display", #updated
                                                    choices = ""),
                                        selectInput("lev3_ma", "Choose a unit to display", #updated
                                                    choices = ""),
                                        selectizeInput("samples_ma", "Select samples", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE),
                                        sliderInput("n_ma", "Choose the number variables (most abundant)", #updated
                                                    min = 2, max = 10, value = 2),
                                        checkboxInput("sel_var_ma", "Select variables",
                                                      value = F),
                                        selectizeInput("var_ma", "Selected variables", #updated
                                                       choices = NULL,
                                                       multiple = TRUE)
                                    ),
                                    conditionalPanel(
                                        condition = "input.dataset_ma!='Current project'",
                                        selectizeInput("rows_ma", "Select rows", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE),
                                        selectizeInput("cols_ma", "Select columns", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE)
                                    ),
                                    actionButton("filter_ma","Filter dataset")
                                ), # Close conditional panel 2 - Data
                                # Input Panel Constrained Variable Selection ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==3",
                                    h2("Select variables for constrained analyses"),
                                    selectInput("const_ma", "Choose a source of constrained variables",
                                                choices = c("Current project",list.files(data_path)),
                                                selected = "Current project"),
                                    conditionalPanel(
                                        condition = "input.const_ma!='Current project'",
                                        checkboxInput("head_const_ma", "Input table includes header", value = T),
                                        checkboxInput("rown_const_ma", "Input table includes rownames", value = T),
                                        selectInput("sep_const_ma", "Format",
                                                    choices=c(".csv",".tsv"), selected = ".csv")
                                    ),
                                    actionButton("load_const_ma","Load dataset"),

                                    h5("Current constrained variables from:"),
                                    verbatimTextOutput("out_const_ma",placeholder=TRUE),

                                    h2("Subset constrained variables"),
                                    conditionalPanel(
                                        condition = "input.const_ma=='Current project'",
                                        selectInput("lev1_const_ma", "Choose a set of data to analyse",
                                                    choices = c("","functions","taxa"),
                                                    selected = ""),
                                        selectInput("lev2_const_ma", "Choose a subsection to display", #updated
                                                    choices = ""),
                                        selectInput("lev3_const_ma", "Choose a unit to display", #updated
                                                    choices = ""),
                                        selectizeInput("samples_const_ma", "Select samples", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE),
                                        sliderInput("n_const_ma", "Choose the number variables (most abundant)", #updated
                                                    min = 2, max = 10, value = 2),
                                        checkboxInput("sel_var_const_ma", "Select variables",
                                                      value = F),
                                        selectizeInput("var_const_ma", "Selected variables", #updated
                                                       choices = NULL,
                                                       multiple = TRUE)
                                    ),
                                    conditionalPanel(
                                        condition = "input.const_ma!='Current project'",
                                        selectizeInput("rows_const_ma", "Select rows", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE),
                                        selectizeInput("cols_const_ma", "Select columns", #updated
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE)
                                    ),
                                    actionButton("filter_const_ma","Filter constrained variables")
                                ), # Close conditional panel 3 - Constrained variables

                                # Input Panel Data Transformations ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==4",
                                    h2("Imputation"),
                                    selectInput("method_imp_ma", "Choose method of imputation",
                                                choices = c("None","Median","NZV"),
                                                selected = "Median"),

                                    actionButton("imp_ma","Impute Data"),

                                    h2("Transformation"),
                                    selectInput("method_trans_ma", "Choose method of transformation",
                                                choices = c("None","Normalise","CLR","ALR", "ILR"),
                                                selected = "CLR"),

                                    actionButton("trans_ma","Transform Data")
                                ),
                                # Input Panel Constrained Variables Transformations ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==5",
                                    h2("Imputation"),
                                    selectInput("method_imp_const_ma", "Choose method of imputation",
                                                choices = c("None","Median","NZV"),
                                                selected = "Median"),

                                    actionButton("imp_const_ma","Impute Variables"),

                                    h2("Transformation"),
                                    selectInput("method_trans_const_ma", "Choose method of transformation",
                                                choices = c("None","Normalise","CLR","ALR", "ILR"),
                                                selected = "CLR"),

                                    actionButton("trans_const_ma","Transform Variables")
                                ),
                                # Input Panel Analysis ----
                                conditionalPanel(
                                    condition = "input.sel_tab_ma==6",
                                    h2("Analysis"),
                                    selectInput("ma_method", "Choose method of analysis",
                                                choices = c("PCA","RDA","PCoA (MDS)","NMDS","CA","CCA","DCA","Permanova"),
                                                selected = "PCA"),

                                    h2("Parameters"),
                                    conditionalPanel( # PCA parameters
                                        condition = "input.ma_method=='PCA'",
                                        selectInput("test_pca","Test parameter",choices=c(1,2,3),selected=1)
                                    ), # Close conditional panel PCA

                                    conditionalPanel( # RDA parameters
                                        condition = "input.ma_method=='RDA'",
                                        selectInput("test_rda","Test parameter",choices=c(1,2,3),selected=1)
                                    ), # Close conditional panel RDA

                                    conditionalPanel( # PCoA parameters
                                        condition = "input.ma_method=='PCoA (MDS)'",
                                        selectInput("dist_pcoa","Distance formula",choices=c(
                                            "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard",
                                            "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial",
                                            "chao", "cao", "mahalanobis"),selected="bray")
                                    ), # Close conditional panel RDA

                                    conditionalPanel( # NMDS parameters
                                        condition = "input.ma_method=='NMDS'",
                                        selectInput("test_nmds","Test parameter",choices=c(1,2,3),selected=1)
                                    ), # Close conditional panel CA

                                    conditionalPanel( # CA parameters
                                        condition = "input.ma_method=='CA'",
                                        selectInput("test_ca","Test parameter",choices=c(1,2,3),selected=1)
                                    ), # Close conditional panel CA

                                    conditionalPanel( # CCA parameters
                                        condition = "input.ma_method=='CCA'",
                                        selectInput("test_cca","Test parameter",choices=c(1,2,3),selected=1)
                                    ) # Close conditional panel CCA
                                ) # Close conditional panel 3 - Analysis
                            ), # Close sidebar panel

                            # Main panel for displaying outputs ----
                            mainPanel(
                                tabsetPanel(
                                    # Output Panel About ----
                                    tabPanel("About",value=1
                                             # Comments on the MA tab
                                    ),
                                    # Output Panel Data Selection ----
                                    tabPanel("Data selection",value=2,
                                             # Display Table Output
                                             h1("Loaded data"),
                                             h5("Loaded data will not be updated until you press
                                            the 'Filter dataset' button at the bottom of the page"),
                                             DT::dataTableOutput("table_ma")
                                    ),
                                    # Output Panel Constrained Variable Selection ----
                                    tabPanel("Constrained variable selection",value=3,
                                             # Display Table Output
                                             h1("Selected variables"),
                                             h5("Selected variables will not be updated until you press
                                            the 'Filter constrained variables' button at the bottom of the page"),
                                             DT::dataTableOutput("table_const")
                                    ),
                                    # Output Panel Data Transformation ----
                                    tabPanel("Data transformation",value=4,
                                             h1("Imputed data"),
                                             h5("Data will not be imputed until you press the 'Impute Data' button"),
                                             DT::dataTableOutput("table_imp_ma"),
                                             h1("Transformed data"),
                                             h5("Data will not be transformed until you press the 'Transform Data' button"),
                                             DT::dataTableOutput("table_trans_ma")
                                             # Visualise transformed data
                                    ),
                                    # Output Panel Constrained Variable Transformation ----
                                    tabPanel("Constrained variable transformation",value=5,
                                             h1("Imputed variables"),
                                             h5("Variables will not be imputed until you press the 'Impute Variables' button"),
                                             DT::dataTableOutput("table_imp_const_ma"),
                                             h1("Transformed variables"),
                                             h5("Variables will not be transformed until you press the 'Transform Variables' button"),
                                             DT::dataTableOutput("table_trans_const_ma")
                                             # Visualise transformed data
                                    ),
                                    # Output Panel Analysis ----
                                    tabPanel("Plots",value=6,
                                             # Output: MA graph
                                             actionButton("ma_plot","Plot"),
                                             plotOutput(outputId = "maPlot"),
                                             downloadButton("maPlotDown", "Download Plot")
                                    ),
                                    id = "sel_tab_ma" # id to input object with selected tab value
                                ) # Close tabset
                            ) # Close main panel
                        ) # Close layout
               ) # Close MA page
    ) # Close UI
    ui
} # Close function
