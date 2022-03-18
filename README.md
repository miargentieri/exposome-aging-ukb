# R code for an exposome-wide association study of human aging in the UK Biobank

This directory contains the code used for data preparation, analysis, and figure creation for the publication XXX, published in XXX. This repository was created on Sat Mar 18 15:15:17 GMT 2022.

R code for each stage of our data preparation and analysis are contained in these files. This includes:  
* Importing raw data
* Recoding data
* Coding all-cause mortality outcome data
* Performing participant exclusions and multiple imputation
* Running the XWAS pipeline
* Creating figures and plots
* Conducting correlation analyses
* Plot, table, and figure generation

Manifest
--------

The following is a description of the various files and directories found within this project.

|File         |Description                                                                                         |
|:------------|:---------------------------------------------------------------------------------------------------|
|`dictionary/`|Data dictionary describing the entire dataset downloaded from UK Biobank and used in analyses       |
|`figures/`   |HTML interactive versions of plots and figures in published paper                                   |
|`results/`   |The raw analysis output from each analysis stage                                                    |
|`scripts/`   |Code to help process data at various stages of the analysis.                                        

Files are meant to be run sequentially. Code in file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables collected in the UK Biobank cohort.   
  
All-cause mortality data used for analysis were  accessed from the UK Biobank data portal on June 5th, 2021. Censoring date used was February 28th, 2021. Using mortality data that is more current will change analysis results.

Author
------

Please contact Austin Argentieri (austin.argentieri@anthro.ox.ac.uk) with any questions, comments, or concerns.


Commitment to reproducibility
-----------------------------

The documents in this repository represent the authors' commitment to reproducible research.  Every part of this published analysis can be reproduced by another investigator easily with the code documented here.  If you find an irreproducible element, please contact the author immediately to inform them of the issue.
