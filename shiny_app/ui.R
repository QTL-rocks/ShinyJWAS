rm(list=ls())
library(rintrojs)

library(shiny)
library(shinyFiles)
library(rclipboard)
library(ggplot2)

library(coda)
library(markdown)
library(manhattanly)

library(shinythemes)
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


bool_juila <- function(x) {
  if (x) {
    return("true")
  } else {
    return("false")
  }
}

ui<-fluidPage(
  #theme = shinytheme("cerulean"),
  rintrojs::introjsUI(),
  titlePanel(tagList(
    img(src = "https://raw.githubusercontent.com/justinwang97/jwas_shiny/master/shiny_app/images/ChengLabLogo.png",
        height = 60, width = 137.15),

    span( strong("ShinyJWAS"),
         span(
           actionButton("help",
                        label = "Help",
                        icon = icon("question")
           ),
             actionButton("github",
                          label = "Code",
                          icon = icon("github"),
                          width = "80px",
                          onclick ="window.open(`https://github.com/justinwang97/jwas_shiny`, '_red')",
                          style="color: #fff; background-color: #767676; border-color: #767676"),
           style = "position:absolute;right:1em;"))),
      windowTitle = "JWAS Helper"),


  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),



  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),

  # Create tabset and tabPanels for the application

  tabsetPanel(id = "tabs",

    # First tabpanel is the "Information" and all the information for application can be found in Information.md file
    tabPanel(
      h4(tags$b("Information", id = "first_panel"), style = "color: #800080;"),
      includeMarkdown("Information.md")
    ),
    ####################################################################################
    # Julia Script
    ####################################################################################

    # Second tabpanel is the "Julia Script"
    tabPanel(h4(tags$b("Julia Script", id = "fifth_panel"), style = "color: #800080;"),
             h6("This section will generate the Julia script to
                           run the model defined by users."),
             hr(),
             downloadButton('Bayesian_Data', tags$b('Download Sample data',id = "fifth_sample_data")  ),
             rclipboardSetup(),
             tags$head(tags$script(src = "ipag.js")),
             hr(),


             h4("Step 1. Build model", id = "fifth_step_1" ),
             # effects
             wellPanel(
               HTML('<label>Please enter the variable included in your model</label>'),
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
               ),

             ),

             h4("Step 2. Set parameters", id = "fifth_step_2"),

             wellPanel(
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


               ),

             ),
             h3("Juila Script"),
             hr(),
             verbatimTextOutput("jw_run_script"),
             div(
               downloadButton("jw_download_script", "Download", style = "margin-right: 5px;"),
               uiOutput("jw_copy_script", inline = TRUE),
               style = "margin-bottom: 10px;"
             )
    ),

    ####################################################################################
    # Phenotype
    ####################################################################################

    tabPanel(
      h4(tags$b("Phenotype"), style = "color: #800080;"),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('phenotype_Data', tags$b('Download Sample data',id = "eighth_sample_data_button") ),

          actionButton('phenotype_graph_example', tags$b('Example',id = "eighth_example_button"), style = 'float: right;'),

          hr(),

          h5(fileInput("phenotype_graph_file", tags$b("Upload JWAS output file")   ),id = "eighth_file_input_button" ),
          h5(textInput("phenotype_index", "choose the phenotype used for the graph ", width = NULL,
                    placeholder = NULL), id = "eighth_phenotype_index_button"  ),
          actionButton("phenotype_graph_indicate", tags$b("Plot",id = "eighth_plot_button") )
        ),
        mainPanel(
          plotOutput("phenotype_graph")
        )
      )
    ),
    ####################################################################################
    # Pedigree
    ####################################################################################

    tabPanel(
      h4(tags$b("Pedigree", id = "third_panel"), style = "color: #800080;"),
      h6("This section aims to visualize the pedigree information for the uses. The R package “visPedigree” was used to make the pedigree plot.
         The format of pedigree file follows the same requirement as “visPedigree” package. The first three columns must be ID of individual, sire,
         and dam. Names of the three columns can be changed, but their orders must maintain the same. Missing individual can be denoted as “”, “ ” ,
         “0” , asterisk, and “NA”, and these missing individuals will be deleted from the pedigree. Missing parents can be denoted by either “NA”,“0”,
         asterisk. More columns, such as sex, generation can be included in the pedigree file."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('pedigree_Data', tags$b('Download Sample data',id = "third_sample_data_button")),
          actionButton('pedigree_plot_example', tags$b('Example',id = "third_example_button"), style = 'float: right;'),
          hr(),
          h5(fileInput("pedigree_plot_file", tags$b("Upload pedigree file") ),id = "third_file_input_button"),
          h5(textInput("pedigree_candidate", label = HTML("Candidate Animal")),id = "third_candidate_animal" ),
          tags$b(actionButton("pedigree_plot_indicate", "Plot"),id = "third_plot_button")
        ),
        mainPanel(
          plotOutput("vidpedigree_plot")
        )
      )

    ),

    ####################################################################################
    # Venn Diagram
    ####################################################################################
    tabPanel(
      h4(tags$b("Venn Diagram", id = "fourth_panel"), style = "color: #800080;"),
      h6("This section generates a Venn diagram of individuals involved in phenotype, genotype, and pedigree data.
         It helps the user visually organize the sample individual information and see the relationship between them,
         such as commonalities and differences. User needs to upload at least one ID file of either phenotype data,
         genotype data, or pedigree data to generate the Venn diagram. The sample file is available by clicking the
         Download Sample Data button. Users can upload example files or click the example button to check the example output."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('Venn_Data', tags$b('Download Sample data', id = "fourth_sample_data")),
          actionButton('Venn_example', tags$b('Example',id = "fourth_example_button"), style = 'float: right;'),
          hr(),

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


        ),
        mainPanel(
          plotOutput("Venn_Diagram")
        )
      )
    ),

    ####################################################################################
    # Manhattan Plot
    ####################################################################################

    tabPanel(
      h4(tags$b("Manhattan Plot", id = "sixth_panel"), style = "color: #800080;"),
      h6("This section helps the user visualize the GWAS result. Once uploading the file containing GWAS results, the preview of the
         uploading data will be shown on the right top. And the interactive manhattan plot provided by R package “manhattanly” will
         also be shown on the right-hand side. Users can use slider input to change the title and threshold line. The information of
         the dot can be shown by clicking the point. The plot is also available for downloading, zoom in, or zoom out. The sample file
         is available by clicking the Download Sample Data button. Users can upload example files or click the example button to
         check the example output."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('manhattan_Data', tags$b('Download Sample data', id = "sixth_sample_data") ),
          actionButton('manhattan_example', tags$b('Example', id = "sixth_example_button"), style = 'float: right;'),
          hr(),
          h5(fileInput("man_file", "Upload GWAS result file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ), id = "sixth_file_input"),

          h5(fileInput("man_map_file", "Upload Map file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ), id = "sixth_map_file" ),

          h5(checkboxInput("man_header", "headers are variable names", TRUE), id = "sixth_file_header"),

          # The choose of file separator, removed since we assume file comes from the JWAS package
          #h5(radioButtons('man_sep','Data file separator vale:',
          #             c(Comma = ',',
          #               Semicolon=';',
          #               Tab = '\t')
          #), id = "sixth_file_separator"),

          h5(textInput("man_p_value", "Choose the test statistics column", value = "WPPA", width = NULL,
                    placeholder = NULL), id = "sixth_y_axis"),

          h5(textInput("man_title", "Graph title", value = "Manhattan Plot", width = NULL,
                    placeholder = NULL), id = "sixth_graph_title"),

          h5(textInput("man_ylab", "Y axis label", value = "WPPA", width = NULL,
                       placeholder = NULL), id = "sixth_y_label"),

          h5(sliderInput("WPPA_threshold","Choose WPPA threshold:",
                      min = 0, max = 1, value = 0.7,step=0.01), id = "sixth_WPPA_threshold"),
          actionButton("man_plot", tags$b("Plot", id = "sixth_plot_button"))
        ),

        mainPanel(
          DT::dataTableOutput('man_data_table'),
          hr(),
          plotlyOutput("manhattan_plot")

        )
      )
    ),
    ####################################################################################
    # Convergence diagnosis
    ####################################################################################


    # seventh tabpanel is Convergence diagnosis
    tabPanel(
      h4(tags$b("Convergence Diagnosis", id = "seventh_panel"), style = "color: #800080;"),
      h6("This section helps the user verify the convergence of their MCMC chains. This section uses the Gelman-Rubin method,
         so multiple chain data are required. User needs to upload multiple files, where each file represents one MCMC chain
         data. In this section, the default number of uploading files is two. Once uploading the files, then the diagnosis
         result from the R package “coda” will be provided. The sample file is available by clicking the Download Sample Data
         button. Users can upload example files or click the example button to check the example output."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('converge_Data', tags$b('Download Sample data', id = "seventh_sample_data") ),
          actionButton('convergence_example', tags$b('Example', id = "seventh_example_button"), style = 'float: right;'),
          hr(),

          h5(numericInput("n_file", "Number of chains:", 2, min = 2, max = 100), id = "seventh_number_of_chain"),
          verbatimTextOutput("value"),

          h5(uiOutput("data_upload"), id = "seventh_file_input"),
          actionButton("CD_plot", tags$b("Plot",id = "seventh_plot_button") )


        ),
        mainPanel(
          span(textOutput("converge_psrf"), style="color:blue"),
          hr(),
          plotOutput("converge_plot2")
        )
      )
    ),
    ####################################################################################
    # Posterior Distribution
    ####################################################################################

    tabPanel(
      h4(tags$b("Posterior Distribution", id= "second_panel"),
         style = "color: #800080;"),
      #("Posterior Distribution Visualization"),
      h6("This section aims to visualize the posterior distribution of model parameters (like residual variance) based on
         the MCMC samples by histogram. The default input file will be sample file from the JWAS package output. The header
         of each column will be the x-axis label for each single histogram graph. Maximum number of parameters allowed in one run is 16."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          downloadButton('jwas_output_Data', tags$b('Download Sample data',id = "second_sample_data_button") ),

          actionButton('jwas_output_example', tags$b('Example',id = "second_example_button"), style = 'float: right;'),

          hr(),

          h5(fileInput("jwas_output_file", tags$b("Upload JWAS output file")   ),id = "second_file_input_button" ),
          actionButton("jwas_output_plot_indicate", tags$b("Plot",id = "second_plot_button") )
        ),
        mainPanel(
          plotOutput("jwas_output_plot")
        )
      )

    )


)
)
