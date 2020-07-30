
# Define UI ----
rm(list=ls())
# Load required packages
library(shiny)
library(shinyFiles)
library(rclipboard)
library(ggplot2)
library(coda)
library(manhattanly)
library(dplyr)
library(grid)
library(plotly)
library(shinythemes)




##########################################################################################
#
#                                      Server
#
##########################################################################################


# Define server logic ----
server <- shinyServer(function(input, output, session){
  
pushR = function(foo, bar){
    foo[[length(foo)+1]] <- bar
    foo
  }  
  
prepare_GWAS<-function(gwas_data , map_data){
  gwas_data$chr = 0;gwas_data$pos = 0
  for (i in 1:dim(gwas_data)[1]){
    keep_index = (gsub('\"','',gwas_data[i,"wEnd"]) == map_data[,1])
    
    gwas_data[i,"chr"] = map_data[keep_index,2]
    gwas_data[i,"pos"] = map_data[keep_index,3]
    
  }
  return(gwas_data)
}



##########################################################################################
#                                  Help section
##########################################################################################

observeEvent(input$help,{
             if (str_contains(input$tabs,"Phenotype")) {
          
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c("#eighth_sample_data_button",
                                                "#eighth_example_button",
                                                "#eighth_file_input_button",
                                                "#eighth_phenotype_index_button",
                                                "#eighth_plot_button"
                                                ),
                                    intro = c("The sample data for phenotype can be download here",
                                              "Click this button to show the phenotype plot of sample data",
                                              "Upload the the phenotype data here, please noticed that this app at most can handle the 4 traits,
                                              the data more than 4 traits will be ignored. ",
                                              "Pick the phenotype used in the plot, th first 4 phenotype in uploaded data will be used 
                                              if there is no input",
                                              " Click this button to generate the phenotype plot"
                                              )))
               )
             } else if (str_contains(input$tabs,"Posterior Distribution")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c("#second_sample_data_button",
                                                "#second_example_button",
                                                "#second_file_input_button",
                                                "#second_plot_button"
                 ),
                 intro = c("The sample data can be download here",
                           "Click this button will show the pedigree plot of the sample data",
                           "Upload the pedigree data. The first three columns must be individual, sire, and dam IDs. 
                           Missing data can be denoted as \"0\",\"*\",\"NA\",or NA, but rows with missing individual
                           will be deleted from the data set.
                            More detailed can be found in the information section.",
                           "Click this button to generate the histogram plot."
                 )))
               )
             } else if (str_contains(input$tabs,"Pedigree")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c("#third_sample_data_button",
                                                "#third_example_button",
                                                "#third_file_input_button",
                                                "#third_candidate_animal",
                                                "#third_plot_button"
                 ),
                 intro = c("The sample data can be download here",
                           "Click this button will show the pedigree plot of the sample data",
                           "Upload the pedigree data. The first three columns must be individual, sire, and dam IDs. 
                           Missing data can be denoted as \"0\",\"*\",\"NA\",or NA, but rows with missing individual
                           will be deleted from the data set.
                            More detailed can be found in the information section.",
                           "Input the interested individual and only pedigree information of these individuals
                           will be kept in the pedigree plot. All individuals' pedigree information will be shown if the input is null",
                           "Click this button to generate the pedigree plot."
                 )))
               )
             } else if (str_contains(input$tabs,"Venn Diagram")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c("#fourth_sample_data",
                                                "#fourth_example_button",
                                                "#fourth_file_pheno_input",
                                                "#fourth_file_geno_input",
                                                "#fourth_file_pedigree_input",
                                                "#fourth_plot_button"
                 ),
                 intro = c("The sample data can be download here",
                           "Click this button will show the venn diagram of the sample data",
                           "Upload the ID file for individuals included in the phenotype data.",
                           "Upload the ID file for individuals included in the genotype data.",
                           "Upload the ID file for individuals included in the pedigree data.",
                           "Click this button to generate the venn diagram plot."
                 )))
               )
             } else if (str_contains(input$tabs,"Julia Script")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c(
                                                "#fifth_sample_data",
                                                "#fifth_step_1",
                                                "#fifth_step1_example_single",
                                                "#fifth_step1_example_multiple",
                                                "#number_of_traits_input",
                                                "#fifth_model_traits",
                                                "#fifth_number_genotype",
                                                "#fifth_genotype_data",

                                                "#fifth_step_2",
                                                "#fifth_step2_example",
                                                "#fifth_chain_length",
                                                "#fifth_burn_in",
                                                "#fifth_output_frequency",
                                                "#fifth_other_parameters"
                 ),
                 intro = c(
                           "The sample data can be download here",
                           "The user needs to input all the variables included in the model. The name of the traits and variables 
                           need to match the header of the uploading file. Also, the variance-covariance matrix for residual and 
                           random effect is also needed.",
                           "Click this button to show the step 1 example input for multiple trait model.",
                           "Click this button to show the step 1 example input for single trait model.",
                           "Input the number of traits you want to inlcude in the model, minimum is 1, the maximum is 100.",
                           "Input the name of traits, fixed effects, random effects, and genotype included in the model.",
                           "Input the number of genotype used in the model",
                           "Please write the genotype variable name, the method you want to use,
                           and the local path for the genotype data",
                           
                           "Once the model is built, the user needs to set the parameter for running the model, like the MCMC chain length, 
                           output frequency. The details can be found in the following help or Information section.",
                           "Click this button to show the example input for step 2.",
                           "Input the length of MCMC chain here.",
                           "Input the number of burnin here, the first burnin iterations are discarded at the beginning of a MCMC chain of length chain_length.",
                           "Input the output sampling frequency here, the JWAS will save MCMC samples every 
                           output_samples_frequency iterations, defaulting to chain_length/1000",
                           "These parameters required for advanced model setting, the detail explanation for these parameters can be found in the
                           JWAS package documation."
                          
                           
                 )))
               )
             }  else if (str_contains(input$tabs,"Manhattan Plot")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c(
                                                "#sixth_sample_data",
                                                "#sixth_example_button",
                                                "#sixth_file_input",
                                                "#sixth_map_file",
                                                "#sixth_file_header",
                                               
                                                "#sixth_y_axis",
                                                "#sixth_graph_title",
                                                "#sixth_y_label",
                                                "#sixth_WPPA_threshold",
                                                "#sixth_plot_button"
                 ),
                 intro = c(
                           "The sample data for Manhattan plot can be download here.",
                           "Click this button to show the Manhattan plot of sample data.",
                           "Upload the data file for Manhattan plot here, the default format will be the same as the GWAS output
                           file from JWAS package.",
                           "Since the default manhattan plot data file does not contain the chromosome and posision information, uploading the map file 
                           is required. For window based data, the chromosome and position for the genomic window will be decided by its first SNP.",
                           "Click this button if the header in the file are parameter name.",
                           "Pick the column used for the y axis in the Manhattan plot, the default will be \"WPPA\" ",
                           "Input the title of the graph.",
                           "Input the label of the y axis for the graph",
                           "Choose the threshold for the manhattan plot.",
                           "Click this button to generate the Manhattan plot."
                          
                 )))
               )
             } else if (str_contains(input$tabs,"Convergence Diagnosis")) {
               rintrojs::introjs(session, options = list(
                 steps = data.frame(element = c(
                                                "#seventh_sample_data",
                                                "#seventh_example_button",
                                                "#seventh_number_of_chain",
                                                "#seventh_file_input",
                                                "#seventh_plot_button"
                 ),
                 intro = c(
                           "The sample data for convergence diagnosis can be download here.",
                           "Click this button to run the convergence diagnosis on the sample data.",
                           "Input number of chains you want to use to do the convergence diagnosis.",
                           "Upload the files for each MCMC chain.",
                           "Click this button to do the convergence diagnosis."
                          
                           
                 )))
               )
             }
             
             
             })






##########################################################################################
#                                    Phenotype 
##########################################################################################

output$phenotype_Data <- downloadHandler(
  filename = function() { 
    "sample_phenotype_data.csv"
  },
  content = function(file) {
    file.copy(paste0(volumes["Example"],"/sample_phenotype_data.csv"), file)
  }
)

phenotype_graph_data = reactive({
  input$phenotype_graph_file
})

observeEvent(input$phenotype_graph_indicate, {
  output$phenotype_graph <- renderPlot({
    pheno_data = read.csv(phenotype_graph_data()$datapath) # read the inpute data
    
  
    if (!input$phenotype_index == ""){
    choosed_pheno = unlist(strsplit(input$phenotype_index, ","))
    pheno_data = pheno_data[choosed_pheno]
    }else{pheno_data = pheno_data}
    trait_name = colnames(pheno_data)  # vector of parameter

    num_trait = dim(pheno_data)[2]

    
    # remove extra columns if the number of phenotype is larger than 4
    if(num_trait > 4){
      num_trait = 4
      pheno_data = pheno_data[,1:num_trait]
      }  

    plot_vec = list()        # list of plot
    count = 1
    for (i in 1:num_trait ){
      for (j in 1:num_trait){
        current_df = data.frame(x = pheno_data[,i], y =pheno_data[,j])
        plot_vec[[count]] = ggplot(current_df, aes(x = x, y =y) ) + geom_point()+
          xlab(trait_name[i])+ylab(trait_name[j])
        count = count + 1
        
      }
    }
    
    grid.arrange(grobs = plot_vec)
    
    
  })
  
  
  
})


observeEvent(input$phenotype_graph_example, {
  
  output$phenotype_graph <- renderPlot({
    pheno_data = read.csv(paste0(volumes["Example"],"/sample_phenotype_data.csv"))
    trait_name = colnames(pheno_data)  # vector of parameter
    
    
    num_graph = dim(pheno_data)[2]
    if(num_graph > 4){num_graph = 4}  
    pheno_data = pheno_data[,1:num_graph]
    
    
    plot_vec = list()        # list of plot
    count = 1
    for (i in 1:num_graph ){
      for (j in 1:num_graph){
        current_df = data.frame(x = pheno_data[,i], y =pheno_data[,j])
        plot_vec[[count]] = ggplot(current_df, aes(x = x, y =y) ) + geom_point()+
          xlab(trait_name[i])+ylab(trait_name[j])
        count = count + 1
        
      }
    }
    
    grid.arrange(grobs = plot_vec)
    
    
  })
  
})






##########################################################################################
#                             Posterior Distribution Visualization
##########################################################################################

output$jwas_output_Data <- downloadHandler(
  filename = function() { 
    paste("MCMC_samples_residual_variance", ".txt", sep="")
  },
  content = function(file) {
    file.copy(paste0(volumes["Example"],"/MCMC_samples_residual_variance.txt"), file)
  }
)



JWAS_output_data = reactive({
  input$jwas_output_file
})



observeEvent(input$jwas_output_plot_indicate, {
  output$jwas_output_plot <- renderPlot({
  cov_data = read.csv(JWAS_output_data()$datapath) # read the inpute data
  paramter_name = colnames(cov_data)  # vector of parameter
  
  
  num_graph = dim(cov_data)[2]
  if(num_graph > 16){num_graph = 16}  
  cov_data = cov_data[,1:num_graph]
  
  max_res = max(cov_data)  # get the max data
  min_res = min(cov_data)  # get the min data

  plot_vec = list()        # list of plot
  for (i in 1:num_graph ){
    plot_vec[[i]] = ggplot(cov_data, aes(x=cov_data[,i])) + 
    geom_histogram(binwidth=abs(max_res-min_res)/50, fill="#69b3a2", color="#e9ecef") +
    xlab(paramter_name[i] ) +
    ylab("frequency")
  }
  
  grid.arrange(grobs = plot_vec)
    

  })
  
  
  
})


observeEvent(input$jwas_output_example, {
  
  output$jwas_output_plot<- renderPlot({
    cov_data <- read.csv(paste0(volumes["Example"],"/output_plot_sample.txt"))
    paramter_name = colnames(cov_data)  # vector of parameter
    
    
    num_graph = dim(cov_data)[2]
    if(num_graph > 16){num_graph = 16}  
    cov_data = cov_data[,1:num_graph]
    
    max_res = max(cov_data)  # get the max data
    min_res = min(cov_data)  # get the min data
    
    plot_vec = list()        # list of plot
    for (i in 1:num_graph ){
      plot_vec[[i]] = ggplot(cov_data, aes(x=cov_data[,i])) + 
        geom_histogram(binwidth=abs(max_res-min_res)/50, fill="#69b3a2", color="#e9ecef") +
        xlab(paramter_name[i] ) +
        ylab("frequency")
    }
    
    grid.arrange(grobs = plot_vec)
    
  })
  
})

##########################################################################################
#                             Pedigree Visualization
##########################################################################################


#cand = c("Y","Z1","Z2")

output$pedigree_Data <- downloadHandler(
  filename = function() { 
    "sample_pedigree.csv"
  },
  content = function(file) {
    file.copy(paste0(volumes["Example"],"/sample_pedigree.csv"), file)
  }
)


vidpedigree_data = reactive({
  input$pedigree_plot_file
})
candidate_data = reactive({
  input$pedigree_candidate
})

observeEvent(input$pedigree_plot_indicate, {
  output$vidpedigree_plot <-renderPlot({
    cand_vec = unlist(strsplit(candidate_data(), ","))
    if(length(cand_vec) ==0){cand_vec = NULL}
    ped_data = read.csv(vidpedigree_data()$datapath)
    
    tidy_sample_ped <-
      tidyped(ped = ped_data,cand = cand_vec )
    visped(tidy_sample_ped)
  })
})


observeEvent(input$pedigree_plot_example, {
  
  output$vidpedigree_plot <-renderPlot({
    
    ped_data = read.csv(paste0(volumes["Example"],"/sample_pedigree.csv"))
    
    tidy_sample_ped <-
      tidyped(ped = ped_data,
              cand = c("Y","Z1","Z2"))
    visped(tidy_sample_ped, compact = TRUE)
  })
})
##########################################################################################
#                                      Venn Diagram
##########################################################################################
  

  # Sample Data   
output$Venn_Data <- downloadHandler(
    filename = function() { 
      paste("ID_", "sample.zip", sep="")
    },
    content = function(file) {
      file.copy(paste0(volumes["Example"],"/ID.zip"), file)
    }
  )
 
  
  
  phen_id_data<-reactive({
    input$pen_id_file
  })
  
  geno_id_data<-reactive({
    input$geno_id_file
  })
  
  pedi_id_data<-reactive({
    input$pedigree_id_file
  })
  
  observeEvent(input$id_plot, {
    output$Venn_Diagram<- renderPlot({
      
      id_list = list()
      id1 = phen_id_data()
     
      
      id2 = geno_id_data()
      
      
      id3 = pedi_id_data()
      
      
      #Read uploading id file
      
      id1_indicator = FALSE
      id2_indicator = FALSE
      id3_indicator = FALSE
      
      if (!is.null(id1)){
        data_id1 <- read.csv(id1$datapath)
        data_id1 = t(as.matrix(data_id1))
        id1_indicator = TRUE
        }
      if (!is.null(id2)){
        data_id2 <- read.csv(id2$datapath)
        data_id2 = t(as.matrix(data_id2))
        id2_indicator = TRUE
        }
      if (!is.null(id3)){
        data_id3 <- read.csv(id3$datapath)
        data_id3 = t(as.matrix(data_id3))
        id3_indicator = TRUE
        }
      
      
      myCol <- brewer.pal(3, "Pastel2")
      x1 = list()
      category_names = c()
      if(id1_indicator){
        x1[[1]] = data_id1
        category_names[1] = "Phenotype ID"
      }
      
      if(id2_indicator){
        x1[[2]] = data_id2
        category_names[2] = "Genotype ID"
      }else{
        x1[[2]] =data_id1
        category_names[2] = ""
        }
      if(id3_indicator){
        x1[[3]] = data_id3
        category_names[3] = "Pedigree ID"
      }else{
        x1[[3]] = data_id1
        category_names[3] = ""
      }
    
      temp = venn.diagram(
        x = x1,
        category.names = category_names,
        
        filename = NULL,
        output = TRUE ,
        imagetype="png" ,
        height = 600 , 
        width = 480 , 
        resolution = 300,
        compression = "lzw",
        lwd = 3,
        col=c("#440154ff", '#21908dff', '#E94826'),
        fill = c(alpha("#440154ff",1), alpha('#21908dff',1), alpha('#E94826',1)),
        cex = 1,
        fontfamily = "sans",
        cat.cex = 1,
        cat.default.pos = "outer",
        cat.pos = c(-27, 45, 135),
        cat.dist = c(0.055, 0.055, 0.085),
        cat.fontfamily = "sans",
        cat.col = c("#440154ff", '#21908dff', '#E94826'),
        rotation = 1
      )
      
      grid.draw(temp)
      
    })
    
  })
  
  
  #Example
  
  
  observeEvent(input$Venn_example, {
 
    output$Venn_Diagram<- renderPlot({
      
      example_id1 <- read.csv(paste0(volumes["Example"],"/IDs_for_individuals_with_phenotypes.txt"))
      example_id2 <- read.csv(paste0(volumes["Example"],"/IDs_for_individuals_with_genotypes.txt"))
      example_id3 <- read.csv(paste0(volumes["Example"],"/IDs_for_individuals_with_pedigree.txt"))
      
      example_id1 = t(as.matrix(example_id1))
      example_id2 = t(as.matrix(example_id2))
      example_id3 = t(as.matrix(example_id3))      
      
      myCol <- brewer.pal(3, "Pastel2")
      temp = venn.diagram(
        x = list(example_id1, example_id2, example_id3),
        category.names = c("Phenotype ID" , "Genotype ID " , "Pedigree ID"),
        filename = NULL,
        output = TRUE ,
        imagetype="png" ,
        height = 600 , 
        width = 480 , 
        resolution = 300,
        compression = "lzw",
        lwd = 3,
        col=c("#440154ff", '#21908dff', '#E94826'),
        fill = c(alpha("#440154ff",1), alpha('#21908dff',1), alpha('#E94826',1)),
        cex = 1,
        fontfamily = "sans",
        cat.cex = 1,
        cat.default.pos = "outer",
        cat.pos = c(-27, 45, 135),
        cat.dist = c(0.055, 0.055, 0.085),
        cat.fontfamily = "sans",
        cat.col = c("#440154ff", '#21908dff', '#E94826'),
        rotation = 1
      )
      
      grid.draw(temp)
      
    })
      
  
  })
  
  
  
##########################################################################################
#                                      Julia Script
##########################################################################################
  
  #sample data for phenotype, genotype and pedigree
  output$Bayesian_Data <- downloadHandler(
    filename = function() { 
      paste("Bayesian", "_sample", ".zip", sep="")
    },
    content = function(file) {
      file.copy(paste0(volumes["Example"],"/Bayesian.zip"), file)
    })
  
  
  #define the working directory and necessary function
  volumes <- c(Home = getwd(),Local = fs::path_home(),Example = paste0(getwd(),"/Example"))
  bool_juila <- function(x) {
    if (x) {
      return("true")
    } else {
      return("false")
    }
  }

  
  #Build model section (input phenotype, fixed and random effect included in the model)
  output$model_traits = renderUI({
    trait_list = c()
    
  if (is.na(input$n_trait) || identical(input$n_trait,numeric(0))){number_trait = 0}
  else{number_trait = input$n_trait} #avoid the crash caused by the emtpy input
   
  for (i in 1:number_trait){chain_list = pushR(trait_list,paste0("y",i))} #define the set of traits
    
  if (number_trait>0 ){
    Traits <- lapply(1:number_trait, function(i) {
      inputName <- trait_list[i]
       fluidRow(
         column(2,
                textInput(paste0("jw_id_traits",i), label = HTML("Trait name"))),
         column(3, 
                textInput(paste0("jw_fixed_effects_number",i), label = HTML("Continous Fixed effects"))),
         column(3,
                textInput(paste0("jw_fixed_effects_factor",i), label = HTML("Catogorical Fixed effects"))),
         column(2,
                textInput(paste0("jw_random_effects",i), label = HTML("Random effects"))),
         column(2,
                textInput(paste0("jw_model_genotype",i), label = HTML("Genotype")))
       )
      
    })
  }
  })
  
  
  
  
  
  
  #Build model section (input covariance matrix for each random effect included in the model)
  
  
  output$genotype_data= renderUI({
    genotype_list = c()
    
    if (is.na(input$n_genotype) || identical(input$n_genotype,numeric(0))){number_gentype = 0}
    else{number_gentype = input$n_genotype} #avoid the crash caused by the emtpy input
    
    for (i in 1:number_gentype){genotype_list = pushR(genotype_list,paste0("geno",i))} 
    
    if (number_gentype!=0 ){
      Traits <- lapply(1:number_gentype, function(i) {
        inputName <- genotype_list[i]
        fluidRow(
          column(4,
                 textInput(paste0("jw_genotype",i), label = HTML("genotype"))
          ),
          column(4,
                 textInput(paste0("jw_genotype_method",i), label = HTML("Method"))
          ),
          column(4,
                 textInput(paste0("jw_genotype_path",i), label = HTML("Genotype Data Path"))
          )
        )
      })
    }
  })
  
  #Example section for Julia Script
  observeEvent(input$jw_effects_example_single, { # JWAS effects example ----
    updateNumericInput(session,"n_trait",value = 1)
    updateTextInput(session, "jw_id_traits1", value = "y1")
    updateTextInput(session, "jw_fixed_effects_number1", value = "x1")
    updateTextInput(session, "jw_fixed_effects_factor1", value = "x2,x3")
    updateTextInput(session, "jw_random_effects1", value = "dam")
    
    
    updateTextInput(session, "jw_model_genotype1", value = "geno")
    updateNumericInput(session, "n_genotype", value = 1)
    updateTextInput(session, "jw_genotype1", value = "geno")
    updateTextInput(session, "jw_genotype_method1", value = "BayesC")
    updateTextInput(session, "jw_genotype_path1", value = "path1")
  })
  
  observeEvent(input$jw_effects_example_multiple, { # JWAS effects example ----
    updateNumericInput(session,"n_trait",value = 2)
    updateTextInput(session, "jw_id_traits1", value = "y1")
    updateTextInput(session, "jw_id_traits2", value = "y2")
    updateTextInput(session, "jw_fixed_effects_number1", value = "x1")
    updateTextInput(session, "jw_fixed_effects_number2", value = "x1")
    updateTextInput(session, "jw_fixed_effects_factor1", value = "x2,x3")
    updateTextInput(session, "jw_fixed_effects_factor2", value = "x2")
    updateTextInput(session, "jw_random_effects1", value = "dam")
    updateTextInput(session, "jw_random_effects2", value = "dam")
    
    
    updateTextInput(session, "jw_model_genotype1", value = "geno1")
    updateTextInput(session, "jw_model_genotype2", value = "geno2")
    updateNumericInput(session, "n_genotype", value = 2)
    updateTextInput(session, "jw_genotype1", value = "geno1")
    updateTextInput(session, "jw_genotype2", value = "geno2")
    updateTextInput(session, "jw_genotype_method1", value = "BayesC")
    updateTextInput(session, "jw_genotype_method2", value = "BayesA")
    updateTextInput(session, "jw_genotype_path1", value = "path1")
    updateTextInput(session, "jw_genotype_path2", value = "path2")
  })
  
 
  observeEvent(input$jw_config_example, { # JWAS config example ----
    updateRadioButtons(session, "jw_methods", selected = "BayesC")
    updateNumericInput(session, "jw_chain_length", value = 5000)
    updateNumericInput(session, "jw_output_samples_frequency", value = 100)
    updateNumericInput(session, "jw_burnin", value = 1000)
    updateNumericInput(session, "jw_pi", value = 0.99)
    updateCheckboxInput(session, "jw_outputebv", value = TRUE)
    #updateCheckboxInput(session, "jw_estimatepi", value = FALSE)
    updateCheckboxInput(session, "jw_estimate_variance", value = TRUE)
    updateCheckboxInput(session, "jw_single_step_analysis", value = FALSE)
    updateCheckboxInput(session, "jw_categorical_trait", value = FALSE)
    updateCheckboxInput(session, "jw_missing_phenotypes", value = TRUE)
    updateCheckboxInput(session, "jw_causal_structure", value = FALSE)
    updateCheckboxInput(session, "jw_constraint", value = FALSE)
    updateCheckboxInput(session, "jw_pedigree", value = FALSE)
    updateCheckboxInput(session, "jw_output_heritability", value = TRUE)
    updateCheckboxInput(session, "jw_printout_model_info", value = TRUE)
  })
  
  
  
  # Script Generator
  #Script for package using and data path definition
  observe({  #JWAS Script ----
    jw_phe_file <- parseFilePaths(volumes, input$jw_phe_file)
    jw_ped_file <- parseFilePaths(volumes, input$jw_ped_file)
    jw_geno_file <- parseFilePaths(volumes, input$jw_geno_file)
    
    script <- c("using JWAS,DataFrames,CSV", "")
    script <- c(script ,"Phenotype_data_path =  # the local path to the phenotype data")
    script <- c(script ,"Pedigree_data_path =   # the local path to the pedigree data")
    #script <- c(script ,"Genotype_data_path =   # the local path to the pedigree data","")
    
    
    
    # JWAS Read Phenotype Script ----
      script <- c(script,
                  paste0('phenotypes = CSV.read(', "Phenotype_data_path",
                         ', header=', bool_juila(TRUE),
                         ', delim="', "," ,'", missingstrings=["NA"])'))
    
    
    # JWAS Read Pedigree Script----
    
      script <- c(script,
                  paste0('pedigree = get_pedigree(', "Pedigree_data_path",
                         ', header=', bool_juila(TRUE),
                         ', separator="', ",",'")'),
                  "")
    
    
    # JWAS effects Script ----
    jw_fixed_effects_number = list()
    jw_fixed_effects_factor = list()
    jw_random_effects = list()
    jw_id = list()
    jw_traits = list()
    jw_random_var= list()
    
    jw_geno_list   = list()
    geno_list      = list()
    geno_path_list = list()
    geno_method_list = list()
    
    if (is.na(input$n_trait) || identical(input$n_trait,numeric(0))){number_trait = 1}
    else{number_trait = input$n_trait}
    
    #if(!is.na(input$n_trait)) {for (i in 1:number_trait){
       
    if(number_trait>0) {for (i in 1:number_trait){   
    #Accept the user input effects
    a = unlist(strsplit(as.character(input[[paste0("jw_fixed_effects_factor",i) ]]), ","))
    
    if(!is.null(a) && !identical(a,character(0)))
    {
      jw_fixed_effects_factor[[i]] = a}
    else{jw_fixed_effects_factor[[i]] = ""}      
    
    a = unlist(strsplit(as.character(input[[paste0("jw_fixed_effects_number",i) ]]), ","))
    if(!is.null(a) && !identical(a,character(0))){jw_fixed_effects_number[[i]] = a}
    else{jw_fixed_effects_number[[i]] = ""}
    
      
    a = unlist(strsplit(as.character(input[[paste0("jw_random_effects",i)]]), ","))
    if(!is.null(a) && !identical(a,character(0))){jw_random_effects[[i]] = a}
    else{jw_random_effects[[i]] = ""} 
    
    
    a = unlist(strsplit(as.character(input[[paste0("jw_id_traits",i)]]), ","))
    if(!is.null(a) && !identical(a,character(0))){jw_traits[[i]] = a}
    else{jw_traits[[i]] = ""}  
    
    a = unlist(strsplit(as.character(input[[paste0("jw_model_genotype",i)]]), ","))
    if(!is.null(a) && !identical(a,character(0))){jw_geno_list[[i]] = a}
    else{jw_geno_list[[i]] = ""}  
    
    }}
    
    if (is.na(input$n_genotype) || identical(input$n_genotype,numeric(0))){number_genotype = 0}
    else{number_genotype = input$n_genotype}
    if(number_genotype > 0){
    for (i in 1:number_genotype){
      a = unlist(strsplit(as.character(input[[paste0("jw_genotype",i) ]]), ","))
      if(!is.null(a) && !identical(a,character(0))){geno_list[[i]] = a}
      else{geno_list[[i]] = ""}
      
      a = unlist(strsplit(as.character(input[[paste0("jw_genotype_path",i) ]]), ","))
      if(!is.null(a) && !identical(a,character(0))){geno_path_list[[i]] = a}
      else{geno_path_list[[i]] = ""}
      
      a = unlist(strsplit(as.character(input[[paste0("jw_genotype_method",i) ]]), ","))
      if(!is.null(a) && !identical(a,character(0))){geno_method_list[[i]] = a}
      else{geno_method_list[[i]] = ""}
    }
    }
    
    #JWAS reading genotype
    
    # JWAS Read Genotype Script ----

    if(number_genotype > 0){
      for (i in 1:number_genotype){
        script <- c(script,
                    paste0(geno_list[[i]],' = get_genotypes(',geno_path_list[[i]] ,',method = "',
                           geno_method_list[[i]],
                           '", header=', bool_juila(TRUE),
                           ', estimatePi = ',"true" ,', separator=\',\')   '  )  ) 
        
      }
    }

    
    # JWAS Build Model (model equation)
    eq1 = c(unlist(jw_fixed_effects_factor[1]), 
            unlist(jw_fixed_effects_number[1]), 
            unlist(jw_random_effects[1]), 
            unlist(jw_geno_list[1]), 
            "ID" )
    
    eq1 = eq1[eq1!=""]
    
    if (is.na(input$n_trait) || identical(input$n_trait,numeric(0))){number_trait = 1}
    else{number_trait = input$n_trait}
    
    if (number_trait != 1){
      script <- c(script,
                  paste0('model_equation = "', jw_traits[1], ' = intercept + ', 
                         paste0(eq1, collapse = " + "))
      ) 
    }else{
      script <- c(script,
                  paste0('model_equation = "', jw_traits[1], ' = intercept + ', 
                         paste0(eq1, collapse = " + "),'"')
      )
    }
    
    # adding the equation for trait 2 or more
    if(!is.na(number_trait) & number_trait>1){

      for (i in 2:number_trait){
        eq2 = c(unlist(jw_fixed_effects_factor[i]), 
                unlist(jw_fixed_effects_number[i]), 
                unlist(jw_random_effects[i]),
                unlist(jw_geno_list[i]),
                "ID" )
        eq2 = eq2[eq2!=""]
        
        script <- c(script,
                    paste0('                  ', jw_traits[i], ' = intercept + ', 
                           paste0(eq2,
                                  collapse = " + "),'"'))
      }
    }
  
    
    
    # adding the building model script
    script <- c(script,
                "model = build_model(model_equation);",
                "\n")
    
    
    
    # JWAS Covariates ----
    #print(number_trait)
    if(number_trait>0){
      for (i in 1:number_trait){
        dim_v = dim(jw_fixed_effects_number[i])
        
        if(!(is.null(dim_v) &unlist(jw_fixed_effects_number[i])==""  ) ){
          
        script <- c(script,
                    paste0('set_covariate(model,"', paste(unlist(jw_fixed_effects_number[i]), collapse = " "),'");')
        )
        }
      }
    }
    
    # JWAS Random ----
    all_random_Effect = c()
    
    if(number_trait>0){
      for (i in 1:number_trait){
        dim_v = dim(jw_random_effects[i])
        
        if( !( is.null(dim_v) & unlist(jw_random_effects[i])==""  ) ){
          
          all_random_Effect = pushR(all_random_Effect,paste(unlist(jw_random_effects[i]) ) )
        }
      }
    }
    
    all_random_Effect = unique(unlist(all_random_Effect))
    if(length(all_random_Effect)>0){
    for (i in 1:length(all_random_Effect)){
      script <- c(script,
                  paste0('set_random(model,"', all_random_Effect[i],'"', ' );')
      )      
    }
    }
    
    
    
    # JWAS ID effect with pedigree as covariance matraix
    script <- c(script,
                paste0('set_random(model,"', "ID", '", pedigree);'),
                "")
    
    #if (length(jw_ped_file$datapath) > 0) {
    #  script <- c(script,
    #              paste0('set_random(model,"', "ID", '", pedigree);'),
    #              "")
    #}
    
    # JWAS genotype
  #  if (input$jw_methods != "conventional (no markers)") {
  #    script <- c(script,
  #                #paste0('genofile = "', "Genotype_data_path", '";'),
  #                paste0("add_genotypes(model, Genotype_data_path,",
  #                       "separator='", ",",
  #                       "', header=", bool_juila(TRUE), ");"),
  #                "")
  #  }
    
    # Call JWAS ----
    
    script <- c(script,
                paste0('out=runMCMC(model, phenotypes, ',
                       
                       #'Pi=',format(round(input$jw_pi, 2), nsmall = 2) ,', ',
                       'Pi=0.99' ,', ',
                      # 'estimatePi=', bool_juila(input$jw_estimatepi), ', ',
                       'outputEBV=', bool_juila(input$jw_outputebv), ', ',
                       'chain_length=', input$jw_chain_length, ', ',
                       'burnin=',input$jw_burnin,', ',
                       'output_samples_frequency=', input$jw_output_samples_frequency, ', ',
                       'estimate_variance=',bool_juila(input$jw_estimate_variance),', ',
                       'single_step_analysis=',bool_juila(input$jw_single_step_analysis),', ',
                       'categorical_trait=',bool_juila(input$jw_categorical_trait),', ',
                       'missing_phenotypes=',bool_juila(input$jw_missing_phenotypes),', ',
                       #'causal_structure=',input$jw_causal_structure,', ',
                       'constraint=',bool_juila(input$jw_constraint),', ',
                       'pedigree=',bool_juila(input$jw_pedigree),', ',
                       'output_heritability=',bool_juila(input$jw_output_heritability),', ',
                       'printout_model_info=',bool_juila(input$jw_printout_model_info),
                       ');')
    )
    output$jw_run_script <- renderText(paste(script, collapse = "\n"))
    
    # JWAS script download & copy ----
    output$jw_download_script <- downloadHandler(
      filename = "run_JWAS.R",
      content = function(file) {
        writeLines(script, file)
      }
    )
    output$jw_copy_script <- renderUI({
      rclipButton("jw_copy_script_btn", "Copy",
                  clipText = paste(script, collapse = "\n"),
                  icon = icon("clipboard"))
    })
  })

  
####################################################################################################
#                                     Manhattan Plot
#################################################################################################### 
  
  #sample data for manhattan plot (window based)
  output$manhattan_Data <- downloadHandler(
    filename = function() { 
      paste("Manhattan", "_sample", ".csv", sep="")
    },
    content = function(file) {
      file.copy(paste0(volumes["Example"],"/Manhattan_sample.csv"), file)
    })
  
  
  #Manhattan plot Example
  observeEvent(input$manhattan_example, {
  
    updateCheckboxInput(session, "man_header", value = TRUE)
    #updateRadioButtons(session, "man_sep", selected = ",")
     
    output$man_data_table <- DT::renderDataTable({
      DT::datatable(read.table(paste0(volumes["Example"],"/Manhattan_sample.csv"), header = input$man_header,sep = ",", stringsAsFactors = FALSE),
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = 5)),
                      pageLength = 3,
                      lengthMenu = c(3, 10, 15)
                    ))
    })
    
    
    output$manhattan_plot<- renderPlotly({
      data <- read.csv(paste0(volumes["Example"],"/Manhattan_sample2.csv"))
      data$CHR = data$chr
      data$BP  = data$pos
      data$P  = data[[input$man_p_value]]
      
      manhattanly(data,suggestiveline = input$WPPA_threshold,
                  genomewideline = FALSE,logp = FALSE,GENE = "wStart",
                  title = input$man_title, xlab = "chromosome",ylab = "WPPA"
      )
    })
    
    
  })
  
  #Regular Plot
  Man_Data<-reactive({
    input$man_file
  })
  Man_map_data<-reactive({
    input$man_map_file
  })

  observeEvent(input$man_plot,{
    output$manhattan_plot<- renderPlotly({
      inFile = Man_Data()
      mapFile = Man_map_data()
      if(is.null(inFile)){
        return(NULL)}
      
      data <- read.csv(inFile$datapath, header = input$man_header,sep = ",")
      map_data<-read.csv(mapFile$datapath)
      
      data = prepare_GWAS(data,map_data)
      
      
      data$CHR = data$chr
      data$BP  = data$pos
      data$P  = data[[input$man_p_value]]
      
      manhattanly(data,suggestiveline = input$WPPA_threshold,
                  genomewideline = FALSE,logp = FALSE,GENE = "wStart",
                  title = input$man_title, xlab = "chromosome",ylab = input$man_ylab
      )
    })
    
    output$man_data_table <- DT::renderDataTable({
      inFile = Man_Data()
      if(is.null(inFile)){
        return(NULL)}
      DT::datatable(read.table(inFile$datapath, header = input$man_header,sep = ",", stringsAsFactors = FALSE),
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = 5)),
                      pageLength = 3,
                      lengthMenu = c(3, 10, 15)
                    ))
    }) 
  })
  
  
  
 

####################################################################################################
#                                 Convergence Diagnosis
####################################################################################################
  
  #sample data for convergence diagnosis 
  output$converge_Data <- downloadHandler(
    filename = function() { 
      paste("converge_", "diagnosis.zip", sep="")
    },
    content = function(file) {
      
      file.copy(paste0(volumes["Example"],"/convergence.zip"), file)
    }
    )
  
  
  #Convergence Diagnosis Example
  observeEvent(input$convergence_example, {
    updateNumericInput(session, "n_file", value = 2)
    
    output$converge_plot2<- renderPlot({

      data1 <- read.csv(paste0(volumes["Example"],"/sample_chain1.csv"))
      data2 <- read.csv(paste0(volumes["Example"],"/sample_chain2.csv"))
      
      combinedchains = mcmc.list(mcmc(data1), mcmc(data2))
      gelman.diag(combinedchains)
      gelman.plot(combinedchains)
      output$converge_psrf <- renderText(
        paste("multivariate potential scale reduction factor:", 
              format(round(gelman.diag(combinedchains)$mpsrf, 2), nsmall = 2))
      )
      
    })
    
    
  })
  
  

  
  #Regular Convergence Diagnosis
  
  pick_unique<-function(chain){
    first_row = as.numeric(chain[1,])
    keep_col = duplicated(first_row)
    return(chain[,!keep_col])
  }

  #dynamic user input
 output$data_upload = renderUI({
    chain_list = c()
    for (i in 1:input$n_file){chain_list = pushR(chain_list,paste0("chain",i))}
    Files <- lapply(1:input$n_file, function(i) {
      inputName <- chain_list[[i]]

      #sliderInput(inputName, inputName, min=0, max=100, value=0, post="%")
      fileInput(inputName, inputName, accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                              )
      
   })
    do.call(tagList, Files)
  })
 

 
 
  
 

  #Convergence Diagnosis based on user input
  observeEvent(input$CD_plot,{
  
  output$converge_plot2<- renderPlot({
    data_list = list()
    chain_list = list()
    for (i in 1:input$n_file){
      current_name = paste0("chain",i)
      
      current_data = input[[current_name]]
      if(is.null(current_data)){
        return(NULL)}
      current <- read.csv(current_data$datapath)
      current = pick_unique(current)
      data_list = pushR(data_list,mcmc(current))
    }
    

    L = lapply(data_list,mcmc)
    combinedchains = mcmc.list(L)
    gelman.plot(combinedchains)
    output$converge_psrf <- renderText(
      paste("multivariate potential scale reduction factor:", 
            format(round(gelman.diag(combinedchains)$mpsrf, 2), nsmall = 2))
    )
    
  })
  
  })
  
})






