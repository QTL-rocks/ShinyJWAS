library(rintrojs)

library(shiny)
library(shinyFiles)
library(rclipboard)
library(ggplot2)

library(coda)
library(markdown)
library(manhattanly)

library(shinythemes)
library(tidyverse)
library(dplyr)
library(grid)
library(plotly)
library(DT)
library(VennDiagram)
library(RColorBrewer)
library(gridExtra)
library(visPedigree)
library(rintrojs)

library(sjmisc)

require(RCurl)


ui<-fluidPage(
  rintrojs::introjsUI(),
shinydashboard::dashboardPage(
  skin = "black",
  shinydashboard::dashboardHeader(
    title = "ShinyJWAS"
  ),
  shinydashboard::dashboardSidebar(
    
    shinydashboard::sidebarMenu(
      
      id = "tabs",
      shinydashboard::menuItem("Home",
                               tabName = "dashboard",
                               icon = shiny::icon("home")
      ),
      shinydashboard::menuItem("Julia Script",
                               icon = shiny::icon("laptop-code"),
                               tabName = "julia_script"
      ),
      shinydashboard::menuItem("Phenotype",
                               tabName = "phenotype",
                               icon = shiny::icon("bar-chart")
      ),
      shinydashboard::menuItem("Pedigree",
                               icon = shiny::icon("sitemap"),
                               tabName = "pedigree"
                              
      ),
      shinydashboard::menuItem("Venn Diagram",
                               tabName = "venn_diagram",
                               icon = shiny::icon("atom")
      ),
      shinydashboard::menuItem("Manhattan Plot",
                               tabName = "manhattan_plot",
                               icon = shiny::icon("dna")
      ),
      shinydashboard::menuItem("Convergence Diagnosis",
                               tabName = "convergence_diagnosis",
                               icon = shiny::icon("random")
      ),
      shinydashboard::menuItem("Posterior Distribution",
                               tabName = "posterior_distribution",
                               icon = shiny::icon("bar-chart")
      ),
      
      
      
      shinydashboard::menuItem("Information",
                               icon = icon("info-circle"),
                               actionButton("help",
                                            label = "Help",
                                            icon = icon("question"),
                                            style="color: #fff; background-color: #212F3C; border-color: #212F3C"
                               ),
                               actionButton("github",
                                            label = "Code",
                                            icon = icon("github"),
                                            #width = "80px",
                                            onclick ="window.open(`https://github.com/justinwang97/jwas_shiny`, '_red')",
                                            style="color: #fff; background-color: #212F3C; border-color: #212F3C")
                               
                               )
      
      
      
      
    )
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$title("ShinyJWAS")
    ),
    
    
    shinydashboard::tabItems(
      ############################  Home  ############################ 
      
      shinydashboard::tabItem(
        tabName = "dashboard",
    
          shinydashboard::box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            shiny::h3("Welcome to ShinyJWAS!"),
            shiny::h4("This ShinyJWAS is implemented entirely in R and consists of one 
                      information panel and seven functional panels: (1) Julia Scripts, 
                      (2) Phenotype, (3) Pedigree, (4) Venn Diagram, (5) Manhattan Plot, 
                      (6) Convergence Diagnosis, and (7) Posterior Distribution.")
            
            
          ),
        
        shinydashboard::box(
          title = "Julia Script",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section helps to generate a Julia script to run Bayesian analysis using the package JWAS. 
                    Users can define their models and several parameters in JWAS using the interactive interface.")
        ),
        shinydashboard::box(
          title = "Phenotype",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section aims to visualize the relationship among phenotypic data described by scatter plots. 
                    The default input file format is CSV. Each column, up to 4 traits, in the input file will be plotted.")
        ),
        shinydashboard::box(
          title = "Pedigree",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section aims to visualize the pedigree structure. The R package 
                    \"visPedigree\" is used to make the pedigree plot. The format of
                    pedigree file follows the same requirement as \"visPedigree\" package. 
                    The first three columns must be ID of the individual, sire, and dam. Names of the three 
                    columns can be changed, but their orders must maintain the same. Missing individuals or 
                    parents can be denoted as \"0\", \"NA\", or asterisk. More columns, such as sex or 
                    generation, can be included in the pedigree file, but will be ignored.")
        ),
        shinydashboard::box(
          title = "Venn Diagram",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section generates a Venn diagram of the sets A = {phenotyped individuals}, 
                    B={genotyped individuals}, and C={individuals in the pedigree}. It helps the user visually 
                    organize the individual information and see the relationship among them, such as 
                    commonalities and differences. Users need to upload at least one ID file of A, B, or C to 
                    generate the Venn diagram.")
        ),
        shinydashboard::box(
          title = "Manhattan Plot",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section helps the user to visualize the GWAS result. Once uploading the file containing
                    GWAS results, a preview will be shown on the right top. And the interactive manhattan plot
                    provided by R package \"manhattanly\" will also be shown. Users can use slider inputs to 
                    change the title and thresholds. The information of the genomic marker/window can be shown 
                    by clicking the point. it is straightforward to download, zoom in, or zoom out the plot.")
        ),
        shinydashboard::box(
          title = "Convergence Diagnosis",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section helps the user to test the convergence of the MCMC chains. 
                    This section uses the Gelman-Rubin method, so multiple MCMC chains are 
                    required. Users need to upload multiple files, where each file includes MCMC 
                    samples from one chain. In this section, the default number of uploading 
                    files is two. Once these files are uploaded, the diagnosis results from the 
                    R package \"coda\" will be provided.")
        ),
        shinydashboard::box(
          title = "Posterior Distribution",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width = 12,
          shiny::h4("This section aims to visualize the posterior distribution of model
                    parameters (e.g., heritability) described by histograms. The input 
                    file is the MCMC samples file generated from the JWAS package. Each 
                    column, up to 16 variables, in the input file will be plotted.")
        )
        
        
      ),
      ############################  Julia Script  ############################ 
      
      shinydashboard::tabItem(tabName = "julia_script",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 12,
                                  
                                  shinydashboard::box(
                                    title = "Build Model",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    HTML('<label>Please enter the variable included in your model</label>'),
                                    downloadButton('Bayesian_Data', tags$b('Download Sample data',id = "fifth_sample_data"),style = 'float: right;'  ),
                                    actionButton('jw_effects_example_multiple', tags$b('Multiple Trait Example',id = "fifth_step1_example_multiple"), style = 'float: right;'),
                                    actionButton('jw_effects_example_single', tags$b('Single Trait Example',id = "fifth_step1_example_single"), style = 'float: right;'),
                                    
                                    hr(),
                                    
                                    h5(numericInput("n_trait", "Number of traits:", 1, min = 1, max = 100), id =  "number_of_traits_input" ),
                                    verbatimTextOutput("default"),
                                    h5(uiOutput("model_traits"), id = "fifth_model_traits" ),
                                    fluidRow(
                                      column(3,
                                             h5(numericInput("n_genotype", "Number of genotype:", 1, min = 1, max = 100), id = "fifth_number_genotype")  ),
                                      column(9,    h5(uiOutput("genotype_data"),id = "fifth_genotype_data") )
                                    )
                                    
                                    
                                  ),
                                  
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "Set Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      HTML('<label>Please set the parameters for JWAS running </label>'),
                                      actionButton('jw_config_example', tags$b('Example', id = "fifth_step2_example" ), style = 'float: right;'),
                                      hr(),
                                      fluidRow(
                                        column(4,
                                               fluidRow(
                                                 column(6,h5(numericInput("jw_chain_length", label = "Chain length",value = 5000),id = "fifth_chain_length" )),
                                                 column(6,h5(numericInput("jw_burnin",label = "Burnin length",value = 1000),id = "fifth_burn_in") )
                                               ),
                                               
                                               h5(numericInput("jw_output_samples_frequency",label = "Output samples frequency",value = 100), id = "fifth_output_frequency"),
                                               
                                               #numericInput("jw_pi", label = "Pi",value = 0.99)
                                        ),
                                        
                                        column(4,
                                               h5(tags$b('Other parameters:', id = "fifth_other_parameters") ),
                                               
                                               checkboxInput("jw_pedigree", "pedigree", FALSE),
                                               checkboxInput("jw_single_step_analysis", "single step analysis", FALSE),
                                               checkboxInput("jw_categorical_trait", "categorical trait", FALSE),
                                               checkboxInput("jw_constraint", "constraint", FALSE)
                                               
                                               
                                               
                                        ),
                                        column(4,
                                               
                                               checkboxInput("jw_estimate_variance", "estimate variance", TRUE),
                                               checkboxInput("jw_missing_phenotypes", "missing phenotypes", TRUE),
                                               checkboxInput("jw_outputebv", "outputEBV", TRUE),
                                               checkboxInput("jw_output_heritability", "output heritability", TRUE),
                                               checkboxInput("jw_printout_model_info", "printout model info", TRUE)
                                               
                                        )
                                        
                                        
                                      )
                                      
                                      #shiny::verbatimTextOutput("score")
                                    )
                                  )
                                ),
                                shiny::column(
                                  width = 12,
                                  shinydashboard::box(
                                    title = "Julia Script",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    verbatimTextOutput("jw_run_script"),
                                    div(
                                      downloadButton("jw_download_script", "Download", style = "margin-right: 5px;"),
                                      uiOutput("jw_copy_script", inline = TRUE),
                                      style = "margin-bottom: 10px;"
                                    )
                                    #networkD3::simpleNetworkOutput("netPlot")
                                  )
                                )
                              )),
      
      ############################  Phenotype  ############################ 
      
      shinydashboard::tabItem(tabName = "phenotype",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,
                                  
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      
                                      downloadButton('phenotype_Data', tags$b('Download Sample data',id = "eighth_sample_data_button") ),
                                      
                                      actionButton('phenotype_graph_example', tags$b('Example',id = "eighth_example_button"), style = 'float: right;')
                                    )
                                  ),
                                  
                                  shinydashboard::box(
                                    title = "Phenotype",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    h5(fileInput("phenotype_graph_file", tags$b("Upload phenotype file")   ),id = "eighth_file_input_button" ),
                                    h5(textInput("phenotype_index", "choose the phenotype used for the graph ", width = NULL,
                                                 placeholder = NULL), id = "eighth_phenotype_index_button"  ),
                                    actionButton("phenotype_graph_indicate", tags$b("Plot",id = "eighth_plot_button") )
                                    
                                   
                                  )
                                  
                                 
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "Phenotype Plot",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    shiny::plotOutput("phenotype_graph")
                                  )
                                )
                              )),
      
      ############################  Pedigree  ############################ 
      shinydashboard::tabItem(tabName = "pedigree",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      downloadButton('pedigree_Data', tags$b('Download Sample data',id = "third_sample_data_button")),
                                      actionButton('pedigree_plot_example', tags$b('Example',id = "third_example_button"), style = 'float: right;')
                                      
                                    )
                                  ),
                                  
                                  
                                  shinydashboard::box(
                                    title = "Pedigree Data",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    h5(fileInput("pedigree_plot_file", tags$b("Upload pedigree file") ),id = "third_file_input_button"),
                                    h5(textInput("pedigree_candidate", label = HTML("Candidate Animal")),id = "third_candidate_animal" ),
                                    tags$b(actionButton("pedigree_plot_indicate", "Plot"),id = "third_plot_button")
                                  )
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "Pedigree Plot",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    shiny::plotOutput("vidpedigree_plot")
                                  )
                                )
                              )),
      
      ############################  Venn Digram  ############################ 
      
      shinydashboard::tabItem(tabName = "venn_diagram",
                              shiny::fluidRow(
                                
                                shiny::column(
                                  width = 4,
                                
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      downloadButton('Venn_Data', tags$b('Download Sample data', id = "fourth_sample_data")),
                                      actionButton('Venn_example', tags$b('Example',id = "fourth_example_button"), style = 'float: right;')
                                      
                                    )
                                  ),
                                  
                                shinydashboard::box(
                                  title = "Venn diagram",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  width = NULL,
                                  h5(fileInput("pen_id_file", "Upload Phenotype ID file",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                  ), id = "fourth_file_pheno_input" ),
                                  h5(fileInput("geno_id_file", "Upload Genotype ID file",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                  ), id = "fourth_file_geno_input"),
                                  
                                  h5(fileInput("pedigree_id_file", "Upload Pedigree ID file",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                  ), id = "fourth_file_pedigree_input" ),
                                  actionButton("id_plot", tags$b("Plot", id = "fourth_plot_button") )
                                  
                                  
                                )
                                ),
                                
                                
                                shiny::column(
                                width = 8,
                                shinydashboard::box(
                                  
                                  title = "Venn Diagram Plot",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  width = NULL,
                                  shiny::plotOutput("Venn_Diagram")
                    
                                )
                                )
                              
      )),
      
      ############################  Manhattan Plot  ############################                                                               
      shinydashboard::tabItem(tabName = "manhattan_plot",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      downloadButton('manhattan_Data', tags$b('Download Sample data', id = "sixth_sample_data") ),
                                      actionButton('manhattan_example', tags$b('Example', id = "sixth_example_button"), style = 'float: right;')
                                    )
                                  ),
                                  
                                  shinydashboard::box(
                                    title = "Manhattan Plot Data",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    h5(fileInput("man_file", "Upload GWAS result file",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                    ), id = "sixth_file_input"),
                                    h5(checkboxInput("man_header", "headers are variable names", TRUE), id = "sixth_file_header"),
                                    
                                    h5(fileInput("man_map_file", "Upload Map file",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                    ), id = "sixth_map_file" )
                                    
                                  ),
                                  
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "Plot Parameter Setting",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                    
                                      
                                      h5(textInput("man_p_value", "Choose the test statistics column", value = "WPPA", width = NULL,
                                                   placeholder = NULL), id = "sixth_y_axis"),
                                      
                                      h5(textInput("man_title", "Graph title", value = "Manhattan Plot", width = NULL,
                                                   placeholder = NULL), id = "sixth_graph_title"),
                                      
                                      h5(textInput("man_ylab", "Y axis label", value = "WPPA", width = NULL,
                                                   placeholder = NULL), id = "sixth_y_label"),
                                      
                                      h5(sliderInput("WPPA_threshold","Choose WPPA threshold:",
                                                     min = 0, max = 1, value = 0.7,step=0.01), id = "sixth_WPPA_threshold"),
                                      actionButton("man_plot", tags$b("Plot", id = "sixth_plot_button"))
                                    )
                                  )
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "Manhattan Data Preview",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    height = 300,
                                    DT::dataTableOutput('man_data_table')
                                  ),
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "Manhattan Plot",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      plotlyOutput("manhattan_plot")
                                    )
                                  )
                                )
                                
                            
                                
                                
                                
                              ),
                              
                              # QTL search
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,
                                  
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "Plot Parameter Setting",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      h5(selectInput("qtl_database", "Select QTL database", choices = list(
                                        "Cattle" = "BT",
                                        "Chicken" = "GG",
                                        "Goat" = "CH",
                                        "Horse" = "EC",
                                        "Pig" = "SS",
                                        "Rainbow Trout" = "OM",
                                        "Sheep" = "OA"
                                      ))),
                                      h5(selectInput("qtl_search_start_col", "Select column for QTL start", choices = "wStart")),
                                      h5(selectInput("qtl_search_end_col", "Select column for QTL end", choices = "wEnd")),
                                      h5(selectInput("qtl_search_range", "Select QTL range", choices = "", multiple = T))
                                      
                                    )
                                  )
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "QTL",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    height = NULL,
                                    DT::dataTableOutput('qtl_search_table')
                                  )
                                  
                                  
                                )
                                
                              ) 
                              
                              ),
      
      ############################  Converge Diagnosis  ############################ 
      
      shinydashboard::tabItem(tabName = "convergence_diagnosis",
                              shiny::fluidRow(
                                
                                
                                
                                shiny::column(
                                  width = 4,
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      downloadButton('converge_Data', tags$b('Download Sample data', id = "seventh_sample_data") ),
                                      actionButton('convergence_example', tags$b('Example', id = "seventh_example_button"), style = 'float: right;')
                                    )
                                  ),
                                  shinydashboard::box(
                                    title = "MCMC data upload",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    h5(numericInput("n_file", "Number of chains:", 2, min = 2, max = 100), id = "seventh_number_of_chain"),
                                    verbatimTextOutput("value"),
                                    
                                    h5(uiOutput("data_upload"), id = "seventh_file_input"),
                                    actionButton("CD_plot", tags$b("Plot",id = "seventh_plot_button") )
                                  )
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "Converge Diagnosis Result",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    span(textOutput("converge_psrf"), style="color:blue"),
                                    hr(),
                                    shiny::plotOutput("converge_plot2")
                                  )
                                )
                              )),
      
      ############################  Posterior Distribution  ############################ 
      
      shinydashboard::tabItem(tabName = "posterior_distribution",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,
                                  
                                  shiny::conditionalPanel(
                                    condition = "true",
                                    shinydashboard::box(
                                      title = "sample data and example",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = NULL,
                                      downloadButton('jwas_output_Data', tags$b('Download Sample data',id = "second_sample_data_button") ),
                                      
                                      actionButton('jwas_output_example', tags$b('Example',id = "second_example_button"), style = 'float: right;')
                                    )
                                  ),
                                  
                                  shinydashboard::box(
                                    title = "Sample data upload",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    h5(fileInput("jwas_output_file", tags$b("Upload JWAS output file")   ),id = "second_file_input_button" ),
                                    actionButton("jwas_output_plot_indicate", tags$b("Plot",id = "second_plot_button") )
                                  )
                                ),
                                shiny::column(
                                  width = 8,
                                  shinydashboard::box(
                                    title = "Posterior Distribution Visualization",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    plotOutput("jwas_output_plot")
                                    
                                  )
                                )
                              ))
      
    )
  )
)

)
