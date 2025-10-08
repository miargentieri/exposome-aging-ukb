# Integrating the environmental and genetic architectures of mortality and aging

This directory contains the code used for data preparation, analysis, tables, and figure creation for the publication: Argentieri et al. "Integrating the environmental and genetic architectures of mortality and aging." *Nature Medicine* (2025). [https://www.nature.com/articles/s41591-024-03483-9](https://www.nature.com/articles/s41591-024-03483-9).

R code for each stage of our data preparation and analysis are contained in these files. This includes:  
* Importing raw data
* Recoding exposure data
* Coding all-cause mortality and incident chronic disease outcome data
* Performing participant exclusions and multiple imputation
* Running the XWAS pipeline
* Conducting correlation and multivariable exposome analyses
* Creating figures, tables, and plots

Manifest
--------

The following is a description of the various files and directories found within this project.

|Directory            |Description                                                                                         |
|:--------------------|:---------------------------------------------------------------------------------------------------|
|`code/`           |Code used for all analyses and for creating figures/tables.                                         |
|`code/functions/` |Custom function files that are called in scripts.                                                   |
|`dictionaries/`      |Data dictionaries describing the UK Biobank data used in imputation, XWAS, and PheWAS analyses.     |
|`files/`             |Files and coding tables that are called in scripts.                                                 |
|`supplementary_files/`             |Single excel document with Supplementary Files 1-178 referenced in the paper. |


Files are in R Markdown format so that chunks can be run individually without running the entire script. However, scripts are not currently written or optimized to be knit and published via R Markdown without additional configuration in the code. Code in script file #1 (data import) requires a complete raw dataset from the UK Biobank with ALL baseline variables collected in the UK Biobank cohort.   
  
All-cause mortality data used for analysis were accessed from the UK Biobank data portal on May 4, 2022. Censoring dates used were September 30, 2021 or October 31, 2021 for participants recruited in England/Scotland or Wales, respectively. Hospital inpatient data were accessed from the UK Biobank data portal on May 30, 2022, at which time the UK Biobank recommended a censoring date of September 30, 2021; July 31, 2021; or February 28, 2018 for participants recruited in England, Scotland, or Wales, respectively. Using mortality and hospital inpatient data from an earlier or later window of data release will change analysis results.

Author
------

Please contact Austin Argentieri (aargentieri@mgh.harvard.edu) with any questions, comments, or concerns.
