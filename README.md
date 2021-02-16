# R code for an exposome-wide association study of chronic disease mortality in the UK Biobank

February 16, 2021  
M. Austin Argentieri

R code for each stage of our data preparation and analysis are contained in these files. This includes:  
* Importing raw data
* Recoding data
* Coding mortality outcome data
* Performing participant exclusions and multiple imputation
* Running the EWAS pipeline
* Creating figures and plots
* Conducting correlation analyses
* Conducting elastic net analyses


Files are meant to be run sequentially. Code in file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables avaialble. 
