# R code for an exposome-wide association study of chronic disease mortality in the UK Biobank

This directory contains the code used for data preparation, analysis, and figure creation for the publication XXX, published in XXX. This repository was created on Sat Sep 20 16:39:16 BST 2014.

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

Manifest
--------

The following is a description of the various files and directories found within this project.

|File         |Description                                                                                         |
|:------------|:---------------------------------------------------------------------------------------------------|
|`go`         |High-level script that will reproduce latest results from the raw data.                             |
|`bin/`       |Contains additional programs necessary for analyzing the data.                                      |
|`data/`      |Raw data files (or links to those files if they come from different projects).                      |
|`lists/`     |Text lists used during the processing (e.g. file lists, gene lists, etc.).                          |
|`results/`   |The analysis product for the raw data.  Many runs may exist in this directory.                      |
|`reports/`   |Analyses on various results sets.                                                                   |
|`resources/` |Files that may be useful during analysis (e.g. gene models, sample metadata, etc.).                 |
|`scratch/`   |A space to keep temporary files that do not have a critical role in the analysis.  Safe to discard. |
|`scripts/`   |Code to help process data at various stages of the analysis.                                        


Files are meant to be run sequentially. Code in file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables avaialble.   
  
Mortality data used for analysis were current as of April 30, 2020. Using mortality data that is more current will change analysis results.

Author
------

Please contact Austin Argentieri (austin.argentieri@anthro.ox.ac.uk) with any questions, comments, or concerns.


Commitment to reproducibility
-----------------------------

The documents in this repository represent the authors' commitment to reproducible research.  Every part of this published analysis can be reproduced by another investigator easily with the code documented here.  If you find an irreproducible element, please contact the author immediately to inform them of the issue.
