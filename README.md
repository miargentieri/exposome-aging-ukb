# R code for an exposome-wide association study of chronic disease mortality in the UK Biobank

February 16, 2021  
M. Austin Argentieri

R code for each stage of our data preparation and analysis are contained in these files. This includes:  
* Importing raw data
* Recoding data
* Coding the Charlson comorbidity index
* Coding mortality outcome data
* Performing participant exclusions and multiple imputation
* Running the EWAS pipeline
* Creating figures and plots
* Conducting correlation analyses
* Conducting elastic net analyses


Files are meant to be run sequentially. Code in file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables avaialble.   
  
Mortality data used for analysis were current as of April 30, 2020. Using mortality data that is more current will change analysis results.

Author
------

Please contact Austin Argentieri (austin.argentieri@anthro.ox.ac.uk) with any questions, comments, or concerns.


Commitment to reproducibility
-----------------------------

The documents in this repository represent the authors' commitment to reproducible research.  Every part of this published analysis can be reproduced by another investigator easily with the code documented here.  If you find an irreproducible element, please contact the author immediately to inform them of the issue.
