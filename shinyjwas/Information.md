### General Guide
This application is implemented entirely in R and consists of one information panel and seven functional panels: (1) Julia Scripts, (2) Phenotype, (3) Pedigree, (4) Venn Diagram, (5) Manhattan Plot, (6) Convergence Diagnosis, and (7) Posterior Distribution.

The application accepts comma-separated values (CSV) files. For each panel, a step-by-step introduction is provided by clicking the _Help_ button on the upper right of web page. Users can also check the example output based on the example data that can be downloaded by clicking the _Download Sample Data_ button on the upper right corner of the sidebar panel. All source code and example data can also be downloaded from the link: https://github.com/QTL-rocks/ShinyJWAS. The general introduction to all seven functional panels is given below.

---------------
#### Julia Scripts Section
This section helps to generate a Julia script to run Bayesian analysis using the package JWAS. Users can define their models and several parameters in JWAS using the interactive interface.

---------------
#### Phenotype Section
This section aims to visualize the relationship among phenotypic data described by scatter plots. The default input file format is CSV. Each column, up to 4 traits, in the input file will be plotted.

---------------
#### Pedigree Section
This section aims to visualize the pedigree structure. The R package "visPedigree" is used to make the pedigree plot. The format of pedigree file follows the same requirement as "visPedigree" package. The first three columns must be ID of the individual, sire, and dam. Names of the three columns can be changed, but their orders must maintain the same. Missing individuals or parents can be denoted as "0", "NA", or asterisk. More columns, such as sex or generation, can be included in the pedigree file, but will be ignored.

---------------
#### Venn Diagram Section:
This section generates a Venn diagram of the sets A = {phenotyped individuals}, B={genotyped individuals}, and C={individuals in the pedigree}. It helps the user visually organize the individual information and see the relationship among them, such as commonalities and differences. Users need to upload at least one ID file of A, B, or C to generate the Venn diagram. 

---------------
#### Manhattan Plot Section
This section helps the user to visualize the GWAS result. Once uploading the file containing GWAS results, a preview will be shown on the right top. And the interactive manhattan plot provided by R package "manhattanly" will also be shown. Users can use slider inputs to change the title and thresholds. The information of the genomic marker/window can be shown by clicking the point. it is straightforward to download, zoom in, or zoom out the plot. 

---------------
#### Convergence Diagnosis Section
This section helps the user to test the convergence of the MCMC chains. This section uses the Gelman-Rubin method, so multiple MCMC chains are required. Users need to upload multiple files, where each file includes MCMC samples from one chain. In this section, the default number of uploading files is two. Once these files are uploaded, the diagnosis results from the R package "coda" will be provided.

---------------
#### Posterior Distribution Section
This section aims to visualize the posterior distribution of model parameters (e.g., heritability) described by histograms. The input file is the MCMC samples file generated from the JWAS package. Each column, up to 16 variables, in the input file will be plotted.

---------------
#### Usage of this application
The application is hosted on Shinyapps.io here: https://qtl-rocks.shinyapps.io/shinyjwas/ . Users can also run ShinyJWAS locally by running _shiny::runGitHub("ShinyJWAS", "QTL-rocks",subdir = "shiny_app")_ in R studio 

---------------
#### Contact
Authors: [Zigui Wang](zigwang97@gmail.com), [Tianjing Zhao](tjzhao@ucdavis.edu), [Hao Cheng](qtlcheng@ucdavis.edu).

---------------
#### JWAS package
You can get more information about JWAS package here: https://reworkhow.github.io/JWAS.jl/latest/

