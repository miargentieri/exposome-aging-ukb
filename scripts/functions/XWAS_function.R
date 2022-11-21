
## XWAS function ---------------------------


### Packages -------------------------------------------------------------------

list.of.packages <- c("Hmisc", "pbapply", "survival", "mice", "data.table", "broom", "parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))))

### XWAS function -------------------------------------------------------------------

XWAS <- function(
    disc_data = NULL, 
    rep_data = NULL, 
    data = NULL,
    multi_imputation = FALSE, 
    model_type, 
    exposures, 
    outcome, 
    covariates,
    extra.var = NULL, 
    extra.covar = NULL,
    replication = TRUE,
    p_adjust = "BH",
    categories = NULL,
    strata = NULL,
    glm_family = NULL,
    scale = FALSE,
    scale_exclude = NULL,
    cores = NULL,
    interaction = FALSE,
    int.type = NULL,
    int.var = NULL,
    int.option = NULL,
    ...
) {
    
    ### Startup errors ----------------------------------------------------------
    
    `%nin%` <- Negate(`%in%`)
    
    if (replication == TRUE & (missing(disc_data) | missing(rep_data))) {
        stop("both 'disc_data' and 'rep_data' must be supplied")
    } else if (replication == FALSE & missing(data)) {
        stop("'data' must be supplied")
    } else if (multi_imputation == TRUE & replication == TRUE & (inherits(disc_data, "data.frame") | inherits(rep_data, "data.frame"))) {
        stop("must supply a list of dataframes if multi_imputation = TRUE")
    } else if (multi_imputation == TRUE & replication == FALSE & inherits(data, "data.frame")) {
        stop("must supply a list of dataframes if multi_imputation = TRUE")
    } else if (multi_imputation == FALSE & replication == TRUE & (!is.data.frame(disc_data) | !is.data.frame(rep_data))) {
        stop("must supply 'data' as a dataframe")
    } else if (multi_imputation == FALSE & replication == FALSE & !is.data.frame(data)) {
        stop("must supply 'data' as a dataframe")
    } else if (model_type == "coxph" & !grepl("Surv", outcome)) {
        stop("'outcome' must be a Surv object for coxph")
    } else if (!is.null(categories) & (sum(c("Variable", "Category") %nin% colnames(categories)) > 0)) {
        stop("'categories' df must include column headings 'Variable' and 'Category'")
    } else if (p_adjust %nin% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) {
        stop("'p_adjust' must be a valid method from p.adjust.methods")
    } else if (!is.null(int.type) & int.type %nin% c(":", "*")) {
        stop("'int.type' must be one of c(':', '*')")
    } else if (!is.null(int.option) & int.option %nin% c("both", "int.only")) {
        stop("'int.option' must be one of c('both', 'int.only')")
    } 
    
    ### Formulas -------------------------------------------------------------------
    
    # message("Initializing XWAS...")
    cat("Initializing XWAS.............")
    flush.console()
    Sys.sleep(1)
    
    if (!is.null(int.var)) {
        covariates <- c(covariates, int.var)
    }
    
    if (model_type == "coxph" & !is.null(strata)) {
        
        covariate_terms <- paste0(covariates, collapse = " + ")
        stratas <- paste0(strata, collapse = ",")
        strata_term <- paste0(paste0("strata(", stratas), ")")
        covariate_terms <- paste(strata_term, covariate_terms, sep = " + ")
        
    } else {
        
        covariate_terms <- paste0(covariates, collapse = " + ")
        
    }
    
    # merge all components together into single formula
    formula <- paste(outcome, covariate_terms, sep = " ~ ")
    
    # set interaction or not
    if (interaction == TRUE) {
        if (int.type == ":") {
            join <- ":"
        } else if (int.type == "*" | is.null(int.type)) {
            join <- "*"
        }
        if (is.null(int.var)) {
            # if no int.var is supplied, use the last covariate listed
            int.var <- covariates[length(covariates)]
        }
    } 
    
    if (interaction == FALSE) {
        join <- " + "
    }
    
    # * coxph ====
    
    # ** Replication ====
    
    if (model_type == "coxph" & replication == TRUE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                if (!is.null(extra.covar) & x == extra.var) {
                    models <- lapply(disc_data, function(y)
                        survival::coxph(as.formula(
                            paste(
                                paste(formula, extra.covar, sep = " + "),
                                x, sep = join
                            )
                        ), data = y, ...)
                    )
                    
                    pool <- mice::pool(models)
                    rm(models)
                    return(pool)
                    
                } else {
                    models <- lapply(disc_data, function(y)
                        survival::coxph(as.formula(
                            paste(
                                formula,
                                x, sep = join
                            )
                        ), data = y, ...)
                    )
                    
                    pool <- mice::pool(models)
                    rm(models)
                    return(pool)
                }
            }
            
            # replication
            model_rep <- function(x) {
                if (!is.null(extra.covar) & x == extra.var) {
                    models <- lapply(rep_data, function(y)
                        survival::coxph(as.formula(
                            paste(
                                paste(formula, extra.covar, sep = " + "),
                                x, sep = join
                            )
                        ), data = y, ...)
                    )
                    
                    pool <- mice::pool(models)
                    rm(models)
                    return(pool)
                    
                } else {
                    models <- lapply(rep_data, function(y)
                        survival::coxph(as.formula(
                            paste(
                                formula,
                                x, sep = join
                            )
                        ), data = y, ...)
                    )
                    
                    pool <- mice::pool(models)
                    rm(models)
                    return(pool)
                }                    
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- survival::coxph(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = disc_data, ...)
                
                return(model)
            }
            
            # replication
            model_rep <- function(x) {
                model <- survival::coxph(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = rep_data, ...)
                
                return(model)
            }
        }
    } 
    
    #** Non-replication ====
    
    if (model_type == "coxph" & replication == FALSE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                models <- lapply(data, function(y)
                    survival::coxph(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- survival::coxph(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = data, ...)
                
                return(model)
            }
        }
    }
    
    # * lm ====
    
    # ** Replication ====
    
    if (model_type == "lm" & replication == TRUE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                models <- lapply(disc_data, function(y)
                    lm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
            # replication
            model_rep <- function(x) {
                models <- lapply(rep_data, function(y)
                    lm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- lm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = disc_data, ...)
                
                return(model)
            }
            
            # replication
            model_rep <- function(x) {
                model <- lm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = rep_data, ...)
                
                return(model)
            }
        }
    }
    
    # ** Non-replication ====
    
    if (model_type == "lm" & replication == FALSE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                models <- lapply(data, function(y)
                    lm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- lm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = data, ...)
                
                return(model)
            }
        }
    }
    
    # * glm ====
    
    # ** Replication ====
    
    if (model_type == "glm" & replication == TRUE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                models <- lapply(disc_data, function(y)
                    glm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, family = glm_family, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
            # replication
            model_rep <- function(x) {
                models <- lapply(rep_data, function(y)
                    glm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, family = glm_family, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- glm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = disc_data, family = glm_family, ...)
                
                return(model)
            }
            
            # replication
            model_rep <- function(x) {
                model <- glm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = rep_data, family = glm_family, ...)
                
                return(model)
            }
        }
    }
    
    # ** Non-replication ====
    
    if (model_type == "glm" & replication == FALSE) {
        
        if (multi_imputation == TRUE) {
            
            # discovery
            model_disc <- function(x) {
                models <- lapply(data, function(y)
                    glm(as.formula(
                        paste(
                            formula,
                            x, sep = join
                        )
                    ), data = y, family = glm_family, ...)
                )
                
                pool <- mice::pool(models)
                rm(models)
                return(pool)
            }
            
        }  else if (multi_imputation == FALSE) {
            
            # discovery
            model_disc <- function(x) {
                model <- glm(as.formula(
                    paste(
                        formula,
                        x, sep = join
                    )
                ), data = data, family = glm_family, ...)
                
                return(model)
            }
        }
    }
    
    cat("done", "\n")
    flush.console()
    Sys.sleep(1)
    
    # Scale ----------------------------------------------------------------
    
    if (scale == TRUE) {
        
        cat("Scaling.......................")
        flush.console()
        
        if (!is.null(scale_exclude)) {
            scale_vars <- exposures[which(exposures %nin% scale_exclude)]
        } else {
            scale_vars <- exposures
        }
        
        # scale function
        scale_numeric <- function(x) { 
            # create logical vector of numeric columns
            nums <- sapply(x, is.numeric)
            # only scale exposures
            nums[names(nums) %nin% scale_vars] <- FALSE 
            # perform scale
            x[nums] <- scale(x[nums], scale = TRUE)
            return(x)
        }
        
        if (multi_imputation == TRUE) {
            
            if (replication == TRUE) {
                # scale desired numeric columns
                disc_data <- lapply(disc_data, scale_numeric)
                rep_data <- lapply(rep_data, scale_numeric)
            } else if (replication == FALSE) {
                # scale desired numeric columns
                data <- lapply(data, scale_numeric)
            }
            
        } else if (multi_imputation == FALSE) {
            
            if (replication == TRUE) {
                # scale desired numeric columns
                disc_data <- scale_numeric(disc_data)
                rep_data <- scale_numeric(rep_data)
            } else if (replication == FALSE) {
                # scale desired numeric columns
                data <- scale_numeric(data)
            }
        }
    }
    
    cat("done", "\n")
    flush.console()
    
    # Run XWAS ----------------------------------------------------------------
    
    options(scipen = 999) # turn off scientific notation
    pbapply::pboptions(type = "timer", char = "=") # initialize progress bar
    
    if (replication == TRUE) {
        
        cat("Performing discovery XWAS.....", "\n")
        flush.console()
        
        ## discovery XWAS
        pool_disc <- as.list(seq(1,length(exposures))) # create list to store model results
        pool_disc <- pbapply::pblapply(exposures, model_disc, cl = cores)
        
        cat("Performing replication XWAS...", "\n")
        flush.console()
        
        ## replication XWAS
        pool_rep <- as.list(seq(1,length(exposures))) # create list to store model results
        pool_rep <- pbapply::pblapply(exposures, model_rep, cl = cores)
        
    } else {
        
        cat("Performing XWAS...............", "\n")
        flush.console()
        
        ## XWAS
        pool_disc <- as.list(seq(1,length(exposures))) # create list to store model results
        pool_disc <- pbapply::pblapply(exposures, model_disc, cl = cores)
        
    }
    
    ### Model Summaries -------------------------------------------------------------------
    
    cat("Processing XWAS results.......")
    flush.console()
    
    if (replication == TRUE) {
        
        if (multi_imputation == TRUE) {
            
            # get list of summaries for all pooled models (with beta)
            smry_disc <- lapply(pool_disc,
                                summary,
                                conf.int = TRUE,
                                conf.level = 0.95)
            
            smry_rep <- lapply(pool_rep,
                               summary,
                               conf.int = TRUE,
                               conf.level = 0.95)
            
            # extract all model summaries from the lists and convert to single data frame of estimates
            allvars_disc <- as.data.frame(data.table::rbindlist(smry_disc))
            allvars_rep <- as.data.frame(data.table::rbindlist(smry_rep))
            
        } else if (multi_imputation == FALSE){
            
            # get list of summaries for all pooled models (with beta)
            smry_disc <- lapply(pool_disc,
                                broom::tidy,
                                conf.int = TRUE,
                                conf.level = 0.95)
            
            smry_rep <- lapply(pool_rep,
                               broom::tidy,
                               conf.int = TRUE,
                               conf.level = 0.95)
            
            # extract all model summaries from the lists and convert to single data frame of estimates
            allvars_disc <- as.data.frame(data.table::rbindlist(smry_disc))
            allvars_rep <- as.data.frame(data.table::rbindlist(smry_rep))
        }
        
        # rename column for exposure
        names(allvars_disc)[names(allvars_disc) == "term"] <- "Exposure"
        names(allvars_rep)[names(allvars_rep) == "term"] <- "Exposure"
        
    }
    
    if (replication == FALSE) {
        
        if (multi_imputation == TRUE) {
            
            # get list of summaries for all pooled models (with beta)
            smry_disc <- lapply(pool_disc,
                                summary,
                                conf.int = TRUE,
                                conf.level = 0.95)
            
            # extract all model summaries from the lists and convert to single data frame of estimates
            allvars_disc <- as.data.frame(data.table::rbindlist(smry_disc))
            
        } else if (multi_imputation == FALSE) {
            
            # get list of summaries for all pooled models (with beta)
            smry_disc <- lapply(pool_disc,
                                broom::tidy,
                                conf.int = TRUE,
                                conf.level = 0.95)
            
            # extract all model summaries from the lists and convert to single data frame of estimates
            allvars_disc <- as.data.frame(data.table::rbindlist(smry_disc))
        }
        
        # rename column for exposure
        names(allvars_disc)[names(allvars_disc) == "term"] <- "Exposure"
        
    }
    
    # Exponentiate ------------------------------------------------------------
    
    ### * coxph ====
    if (model_type == "coxph") {
        
        if (replication == TRUE) {
            
            # calculate Hazard Ratio
            allvars_disc$Hazard.Ratio <- exp(allvars_disc$estimate)
            allvars_rep$Hazard.Ratio <- exp(allvars_rep$estimate)
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            names(allvars_rep)[names(allvars_rep) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"HR_2.5%" <- exp(allvars_disc$`2.5 %`)
                allvars_disc$"HR_97.5%" <- exp(allvars_disc$`97.5 %`)
                
                allvars_rep$"HR_2.5%" <- exp(allvars_rep$`2.5 %`)
                allvars_rep$"HR_97.5" <- exp(allvars_rep$`97.5 %`)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE){
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"HR_2.5%" <- exp(allvars_disc$conf.low)
                allvars_disc$"HR_97.5%" <- exp(allvars_disc$conf.high)
                
                allvars_rep$"HR_2.5%" <- exp(allvars_rep$conf.low)
                allvars_rep$"HR_97.5" <- exp(allvars_rep$conf.high)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "conf.low"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "conf.high"] <- "Beta_97.5%"
                
            }
        }
        
        if (replication == FALSE) {
            
            # calculate Hazard Ratio
            allvars_disc$Hazard.Ratio <- exp(allvars_disc$estimate)
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"HR_2.5%" <- exp(allvars_disc$`2.5 %`)
                allvars_disc$"HR_97.5%" <- exp(allvars_disc$`97.5 %`)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"HR_2.5%" <- exp(allvars_disc$conf.low)
                allvars_disc$"HR_97.5%" <- exp(allvars_disc$conf.high)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
            }
        }
    } 
    
    ### * glm ====
    if (model_type == "glm") {
        
        if (replication == TRUE) {
            
            # calculate Odds Ratio
            allvars_disc$Odds.Ratio <- exp(allvars_disc$estimate)
            allvars_rep$Odds.Ratio <- exp(allvars_rep$estimate)
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            names(allvars_rep)[names(allvars_rep) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"OR_2.5%" <- exp(allvars_disc$`2.5 %`)
                allvars_disc$"OR_97.5%" <- exp(allvars_disc$`97.5 %`)
                
                allvars_rep$"OR_2.5%" <- exp(allvars_rep$`2.5 %`)
                allvars_rep$"OR_97.5" <- exp(allvars_rep$`97.5 %`)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"OR_2.5%" <- exp(allvars_disc$conf.low)
                allvars_disc$"OR_97.5%" <- exp(allvars_disc$conf.high)
                
                allvars_rep$"OR_2.5%" <- exp(allvars_rep$conf.low)
                allvars_rep$"OR_97.5" <- exp(allvars_rep$conf.high)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "conf.low"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "conf.high"] <- "Beta_97.5%"
                
            }
        } 
        
        if (replication == FALSE) {
            
            # calculate Odds Ratio
            allvars_disc$Odds.Ratio <- exp(allvars_disc$estimate)
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"OR_2.5%" <- exp(allvars_disc$`2.5 %`)
                allvars_disc$"OR_97.5%" <- exp(allvars_disc$`97.5 %`)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE) {
                
                # exponentiate beta 95% CIs for HR 95% CIs
                allvars_disc$"OR_2.5%" <- exp(allvars_disc$conf.low)
                allvars_disc$"OR_97.5%" <- exp(allvars_disc$conf.high)
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
            }
        }
    }
    
    ### * lm ====
    
    if (model_type == "lm") {
        
        if (replication == TRUE) {
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            names(allvars_rep)[names(allvars_rep) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE) {
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
                names(allvars_rep)[names(allvars_rep) == "conf.low"] <- "Beta_2.5%"
                names(allvars_rep)[names(allvars_rep) == "conf.high"] <- "Beta_97.5%"
                
            }
        }
        
        if (replication == FALSE) {
            
            # rename column for betas
            names(allvars_disc)[names(allvars_disc) == "estimate"] <- "Beta"
            
            if (multi_imputation == TRUE) {
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "2.5 %"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "97.5 %"] <- "Beta_97.5%"
                
            } else if (multi_imputation == FALSE) {
                
                # rename column for beta 95% CIs
                names(allvars_disc)[names(allvars_disc) == "conf.low"] <- "Beta_2.5%"
                names(allvars_disc)[names(allvars_disc) == "conf.high"] <- "Beta_97.5%"
                
            }
        }
    }
    
    ### Getting accurate p-values  -------------------------------------------------------------------
    
    if (multi_imputation == TRUE) {
        
        ### using summary on a mipo object (output from the pool function) rounds very small p-values to 0
        ### even when we set the # of digits to the max in R (22)
        ### so we have to hard code the math to get the p-values manually from the mipo object
        
        ## discovery 
        tstat <- as.list(seq(1,length(pool_disc))) # create list to store t stats
        p.val <- as.list(seq(1,length(pool_disc))) # create list to store p-values
        p_table <- as.list(seq(1,length(pool_disc))) # create list to p table dfs
        
        # loop to calculate p-values and create table of results
        for (j in seq_along(pool_disc)) {
            # t statistic test using pooled estimate and pooled variance (t)
            tstat[[j]] <- pool_disc[[j]]$pooled$estimate / sqrt(pool_disc[[j]]$pooled$t)
            # calculate p-values
            p.val[[j]] <- as.data.frame(2 * pt(-abs(tstat[[j]]), df = pool_disc[[j]]$pooled$df))
            # create df in p table
            p_table[[j]] <- data.frame(matrix(NA, nrow = nrow(p.val[[j]]), ncol = 2))
            # fill in p table
            p_table[[j]]$X2 <- p.val[[j]][[1]]
            # add in variable names
            p_table[[j]]$X1 <- pool_disc[[j]]$pooled$term
        }
        
        # extract list into a single df
        p_table <- data.table::rbindlist(p_table)
        
        # change column names
        colnames(p_table) <- c("Exposure.p.table",
                               "p.value.full")
        
        # merge p table with XWAS results
        allvars_disc <- cbind(allvars_disc, p_table)
        
        # test to make sure the rows didn't get mixed up
        all.equal(allvars_disc$Exposure, allvars_disc$Exposure.p.table)
        
        # replace all 0 p-values from pooling with actual value
        allvars_disc$p.value <- ifelse(allvars_disc$p.value == 0, 
                                       allvars_disc$p.value.full,
                                       allvars_disc$p.value)
        
        allvars_disc <- subset(allvars_disc, select = -c(Exposure.p.table, p.value.full))
        
    } 
    
    if (multi_imputation == TRUE & replication == TRUE) {
        
        ## replication 
        tstat <- as.list(seq(1,length(pool_rep))) # create list to store t stats
        p.val <- as.list(seq(1,length(pool_rep))) # create list to store p-values
        p_table <- as.list(seq(1,length(pool_rep))) # create list to p table dfs
        
        # loop to calculate p-values and create table of results
        for (j in seq_along(pool_rep)) {
            # t statistic test using pooled estimate and pooled variance (t)
            tstat[[j]] <- pool_rep[[j]]$pooled$estimate / sqrt(pool_rep[[j]]$pooled$t)
            # calculate p-values
            p.val[[j]] <- as.data.frame(2 * pt(-abs(tstat[[j]]), df = pool_rep[[j]]$pooled$df))
            # create df in p table
            p_table[[j]] <- data.frame(matrix(NA, nrow = nrow(p.val[[j]]), ncol = 2))
            # fill in p table
            p_table[[j]]$X2 <- p.val[[j]][[1]]
            # add in variable names
            p_table[[j]]$X1 <- pool_rep[[j]]$pooled$term
        }
        
        # extract list into a single df
        p_table <- data.table::rbindlist(p_table)
        
        # change column names
        colnames(p_table) <- c("Exposure.p.table",
                               "p.value.full")
        
        # merge p table with XWAS results
        allvars_rep <- cbind(allvars_rep, p_table)
        
        # test to make sure the rows didn't get mixed up
        all.equal(allvars_rep$Exposure, allvars_rep$Exposure.p.table)
        
        # replace all 0 p-values with actual value
        allvars_rep$p.value <- ifelse(allvars_rep$p.value == 0, 
                                      allvars_rep$p.value.full,
                                      allvars_rep$p.value)
        
        allvars_rep <- subset(allvars_rep, select = -c(Exposure.p.table, p.value.full))
        
    }
    
    ### Remove covariate estimates ------------------------------------------------
    
    # remove the unwanted rows with coefficients for the covariates from each model
    # strata terms are not listed in output and don't need removing
    
    # add intercept to list of covariates to remove
    # remove_list <- c("intercept", "Intercept", covariates)
    # remove_list <- paste(remove_list, collapse = "|")
    exposures_list <- paste(exposures, collapse = "|")
    index_disc <- grepl(exposures_list, allvars_disc$Exposure)
    
    if (replication == TRUE) {
        index_rep <- grepl(exposures_list, allvars_rep$Exposure)
        if (interaction == TRUE) {
            if (int.option == "both" | is.null(int.option)) {
                allvars_disc <- allvars_disc[index_disc, ]
                allvars_rep <- allvars_rep[index_rep, ]
            } else if (int.option == "int.only") {
                index2_disc <- which(grepl(int.var, allvars_disc$Exposure) & grepl(exposures_list, allvars_disc$Exposure))
                index2_rep <- which(grepl(int.var, allvars_rep$Exposure) & grepl(exposures_list, allvars_rep$Exposure))
                allvars_disc <- allvars_disc[index2_disc, ]
                allvars_rep <- allvars_rep[index2_rep, ]
            }
        } else if (interaction == FALSE) {
            allvars_disc <- allvars_disc[index_disc, ]
            allvars_rep <- allvars_rep[index_rep, ]
        }
    }
    
    if (replication == FALSE) {
        if (interaction == TRUE) {
            index2_disc <- which(grepl(int.var, allvars_disc$Exposure) & grepl(exposures_list, allvars_disc$Exposure))
            if (int.option == "both" | is.null(int.option)) {
                allvars_disc <- allvars_disc[index_disc, ]
            } else if (int.option == "int.only") {
                allvars_disc <- allvars_disc[index2_disc, ]
            }
        } else if (interaction == FALSE) {
            allvars_disc <- allvars_disc[index_disc, ]
        }
    }
    
    ### P-value replication -------------------------------------------------------------------
    
    if (replication == TRUE) {
        
        # get p-value column from replication set to merge
        rep <- subset(allvars_rep, select = c("Exposure", "p.value")) # subset replication results to just p-values
        names(rep)[names(rep) == "p.value"] <- "p.value.rep" # re-name p-values to differentiate from discovery
        
        ## merge disc and rep, keeping estimates just from discovery
        XWAS_total <- merge(allvars_disc, 
                            rep, 
                            by = "Exposure", 
                            all = FALSE, 
                            sort = FALSE)
        
    } else {
        
        ## merge disc and rep, keeping estimates just from discovery
        XWAS_total <- allvars_disc
        
    }
    
    ### Add Categories -------------------------------------------------------------------
    
    ## map variable names to results
    XWAS_total$Variable <- NA
    # must sort for variable matching to work properly
    variables <- sort(exposures)
    variables_grep <- paste0("\\b", variables)
    
    # (e.g., match "alcohol_freq" to "alcohol_freqDaily or almost daily")
    for(j in seq_along(variables)) {
        XWAS_total$Variable[grepl(variables_grep[[j]], XWAS_total$Exposure)] <- variables[[j]]
    }
    
    if (!is.null(categories)) {
        ## add in categories according to match with variable name
        XWAS_total <- merge(XWAS_total,  
                            subset(categories, select = c("Variable", "Category")), 
                            by = "Variable", 
                            all = FALSE, 
                            sort = FALSE)
        
        # order exposures by category
        XWAS_total <- XWAS_total[order(XWAS_total$Category), ] 
    }
    
    
    ### Multiple testing correction -------------------------------------------------------------------
    
    if (p_adjust != "none") {
        
        if (replication == TRUE) {
            
            ## calculate FDR in the discovery analyses using benjamini-hochberg method
            XWAS_total$FDR.disc <- p.adjust(XWAS_total$p.value, method = p_adjust)
            
            ## calculate FDR in the replication analyses only among those from the discovery with FDR p-value < 0.05
            exposome_rep <- subset(XWAS_total, select = c("Exposure", 
                                                          "FDR.disc",
                                                          "p.value.rep"))
            
            # subset to associations that were significant after FDR in discovery
            exposome_rep <- exposome_rep[which(exposome_rep$FDR.disc < 0.05), ] 
            
            # calculate FDR on replication p-values only for significant variables in discovery
            exposome_rep$FDR.rep <- p.adjust(exposome_rep$p.value.rep, method = p_adjust) 
            
            # subset replication results to just FDR p-values
            sig <- subset(exposome_rep, select = c("Exposure", "FDR.rep")) 
            
            ## recombine FDR p-values from replication with discovery results
            XWAS_total <- merge(XWAS_total, 
                                sig, 
                                by = "Exposure", 
                                all = T, 
                                sort = F)
            
        } else if (replication == FALSE) {
            ## calculate FDR in the discovery analyses using benjamini-hochberg method
            XWAS_total$FDR <- p.adjust(XWAS_total$p.value, method = p_adjust)
        } 
    }
    
    ### Order columns -------------------------------------------------------------------
    
    reorder <- function(x, y){
        vec <- c()
        for (i in 1:length(x)){
            vec <- c(vec, which(grepl(y[i],names(x))))
        }
        return(x[,vec])
    }
    
    cols <- c("Exposure", "Beta", "Hazard.Ratio", "HR", "Odds.Ratio", "OR", "p.value", "FDR", "Variable", "Category", "std.error", "statistic", "df")
    XWAS_total <- reorder(XWAS_total, y = cols)

    # Return ------------------------------------------------------------------
    
    formula <- paste(formula, "x", sep = join)
    
    if (replication == TRUE) {
        
        names(pool_disc) <- exposures
        names(pool_rep) <- exposures
        
        if (multi_imputation == TRUE) {
            
            exposome_results <- list(
                "pooled_models_discovery" = pool_disc,
                "pooled_models_replication" = pool_rep,
                "summary" = XWAS_total,
                "model_formula" = formula,
                "discovery_model_function" = model_disc,
                "replication_model_function" = model_rep,
                "exposures" = exposures
            )
            
            class(exposome_results) <- "XWAS"
            return(exposome_results) 
            
        } else if (multi_imputation == FALSE) {
            
            exposome_results <- list(
                "models_discovery" = pool_disc,
                "models_replication" = pool_rep,
                "summary" = XWAS_total,
                "model_formula" = formula,
                "discovery_model_function" = model_disc,
                "replication_model_function" = model_rep,
                "exposures" = exposures
            )
            
            class(exposome_results) <- "XWAS"
            return(exposome_results) 
        }
        
    }
    
    if (replication == FALSE) {
        
        names(pool_disc) <- exposures
        
        if (multi_imputation == TRUE) {
            
            exposome_results <- list(
                "pooled_models" = pool_disc,
                "summary" = XWAS_total,
                "model_formula" = formula,
                "model_function" = model_disc,
                "exposures" = exposures
            )
            
            class(exposome_results) <- "XWAS"
            return(exposome_results) 
            
        } else if (multi_imputation == FALSE) {
            
            exposome_results <- list(
                "models" = pool_disc,
                "summary" = XWAS_total,
                "model_formula" = formula,
                "model_function" = model_disc,
                "exposures" = exposures
            )
            
            class(exposome_results) <- "XWAS"
            return(exposome_results) 
        }
    }
    
    cat("done")
    flush.console()
}
