# Exposome-wide analysis reveals the environmental architecture of human aging among 436,965 UK Biobank participants

This directory contains the code used for data preparation, analysis, and figure creation for the publication "Exposome-wide analysis reveals the environmental architecture of human aging among 436,965 UK Biobank participants." Published in XXX. 

This repository was created on Fri Mar 18 15:15:17 GMT 2022.

R code for each stage of our data preparation and analysis are contained in these files. This includes:  
* Importing raw data
* Recoding data
* Coding all-cause mortality outcome data
* Performing participant exclusions and multiple imputation
* Running the XWAS pipeline
* Conducting correlation analyses
* Creating figures and plots

Manifest
--------

The following is a description of the various files and directories found within this project.

|File              |Description                                                                                         |
|:-----------------|:---------------------------------------------------------------------------------------------------|
|`dictionaries/`   |Data dictionaries describing the UK Biobank datasets used in imputation and XWAS analysis           |
|`figures/`        |HTML interactive versions of plots and figures from the published paper                             |
|`results/`        |The raw analysis output from each analysis stage                                                    |
|`scripts/`        |Code to help process data at various stages of the analysis.                                        |

Files are in R Markdown format so that chunks can be run individually without running the entire script. However, scripts are not currently written or optimized to be knit and published via R Markdown without additional configuration in the code. Code in file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables collected in the UK Biobank cohort.   
  
All-cause mortality data used for analysis were  accessed from the UK Biobank data portal on June 5th, 2021. Censoring date used was February 28th, 2021. Using mortality data that is more current will change analysis results.

Interactive HTML versions of figures in manuscript
--------------------------------------------------
 
<a target="_blank" rel="noopener noreferrer" href="miargentieri.github.io/figures/ACM_XWAS_HR_comparison_feb_21_2022.html"> Comparison plot between female- and male-specific XWAS hazard ratios </a> 
<br>
<a target="_blank" rel="noopener noreferrer" href="miargentieri.github.io/figures/ACM_XWAS_loghr_volc_exposome_feb_22_2022_all_sexes.html"> Pooled XWAS volcano plot </a>

Author
------

Please contact Austin Argentieri (austin.argentieri@anthro.ox.ac.uk) with any questions, comments, or concerns.


Commitment to reproducibility
-----------------------------

The documents in this repository represent the authors' commitment to reproducible research.  Every part of this published analysis can be reproduced by another investigator easily with the code documented here.  If you find an irreproducible element, please contact the author immediately to inform them of the issue.
