
### Packages -------------------------------------------------------------------

list.of.packages <- c("Hmisc", "car", "survival", "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))))


# Cluster reduce function -------------------------------------------------

cluster_reduce <- function(
        data,
        vif.data = NULL,
        clusters = 1,
        cluster.mat = NULL,
        exclude.vars = NULL,
        alias.vars = NULL,
        vars.list = NULL,
        multi.imputation = FALSE,
        check.vif = TRUE,
        model.type,
        outcome,
        covariates,
        vif.limit = 2,
        vif.type = "GVIF^(1/(2*Df))",
        vif.override = NULL,
        vif.only = FALSE,
        strata = NULL,
        ...
) {
    
    ### Startup errors ----------------------------------------------------------
    
    `%nin%` <- Negate(`%in%`)

    if (multi.imputation == TRUE & inherits(data, "data.frame")) {
        stop("must supply a list of dataframes if multi.imputation = TRUE")
    } else if (multi.imputation == FALSE & inherits(data, "list")) {
        stop("cannot supply a list of dataframes if multi.imputation = FALSE")
    } else if (model.type == "coxph" & !grepl("Surv", outcome)) {
        stop("'outcome' must be a Surv object for coxph")
    }

    # Set formula -------------------------------------------------------------
    
    # message("Initializing XWAS...")
    cat("Within-cluster variable selection\n")
    flush.console()
    Sys.sleep(1)
    
    if (model.type == "coxph" & !is.null(strata)) {
        
        covariate_terms <- paste0(covariates, collapse = " + ")
        stratas <- paste0(strata, collapse = ", ")
        strata_term <- paste0(paste0("strata(", stratas), ")")
        covariate_terms <- paste(strata_term, covariate_terms, sep = " + ")
        
    } else if (model.type == "lmer" & !is.null(rand_terms)) {
        
        covariate_terms <- paste0(covariates, collapse = " + ")
        random <- paste0(rand_terms, collapse = " + ")
        covariate_terms <- paste(random, covariate_terms, sep = " + ")
        
    } else {
        
        covariate_terms <- paste0(covariates, collapse = " + ")
        
    }
    
    # merge all components together into single formula
    formula <- paste(outcome, covariate_terms, sep = " ~ ")
    
    # * coxph ====
    
    if (model.type == "coxph") {
        
        # discovery
        model_function <- function(x, y) {
            survival::coxph(as.formula(
                paste(
                    formula,
                    y, sep = " + "
                )
            ), data = x, ...)
        }
    }
        
    # * lm ====
    
    if (model.type == "lm") {
        
        # discovery
        model_function <- function(x, y) {
            lm(as.formula(
                paste(
                    formula,
                    y, sep = " + "
                )
            ), data = x, ...)
        }
    }
    
    # * glm ====
    
    if (model.type == "glm") {
        
        # discovery
        model_function <- function(x, y) {
            glm(as.formula(
                paste(
                    formula,
                    y, sep = " + "
                )
            ), data = x, ...)
        }
    }

    # Check VIF ---------------------------------------------------------------
    
    vars <- as.list(rep(1,length(clusters)))
    vif_vars <- as.list(rep(1,length(clusters)))
    vif_remove <- as.list(rep(1,length(clusters)))
    models <- as.list(rep(1,length(clusters)))
    df <- as.list(rep(1,length(clusters)))
    
    if (!is.null(alias.vars)) {
        exclude.vars <- c(exclude.vars, alias.vars)
    }

    for (k in seq_along(clusters)) {
        
        # extract variables
        if (!is.null(vars.list)) {
            vars[[k]] <- vars.list
        } else {
            vars[[k]] <- as.vector(unlist(cluster.mat[clusters[k]]))
            vars[[k]] <- vars[[k]][!is.na(vars[[k]])]
        }
        
        if (!is.null(exclude.vars)) {
            vars[[k]] <- vars[[k]][vars[[k]] %nin% exclude.vars]
        }
        
        if (length(vars[[k]]) == 0) {
            
            cat(paste(paste("\rNo variables left after exclusion.... cluster", k), length(clusters), sep = "/"), "\n", "\n")
            flush.console()
            models[[k]] <- "empty"
            
        } else {
            
            model_vars <- paste0(vars[[k]], collapse = " + ")
            
            cat(paste(paste("\rChecking collinearity.... cluster", k), length(clusters), sep = "/"), "\n", "\n")
            flush.console()
            # Sys.sleep(1.5)
            
            # initialize empty vector
            remove_set <- c()
            keep_set <- c()
            # initialize i for while loop
            i <- 1
            # check collinearity
            while (i != 0) {
                
                if (multi.imputation == TRUE) {
                    if (!is.null(vif.data)) {
                        fit <- model_function(vif.data[[1]], y = model_vars)
                    } else {
                        fit <- model_function(data[[1]], y = model_vars)
                    }
                    
                } else if (multi.imputation == FALSE) {
                    if (!is.null(vif.data)) {
                        fit <- model_function(vif.data, y = model_vars)
                    } else {
                        fit <- model_function(data, y = model_vars)
                    }
                }
                
                # try to run vif and return model results if error (e.g. aliased coefficients) so that user can check coefficients
                df[[k]] <- tryCatch(as.data.frame(car::vif(fit)),
                                    error = function(e) {print(summary(fit))})
            
                # display
                print.data.frame(df[[k]])
                flush.console()
                rm(fit)
                
                # stop here and return vif results if only vif is desired
                if (vif.only == TRUE) {
                    return(df[[k]])
                }
                
                # make col for variable name
                df[[k]]$var <- rownames(df[[k]])
                
                # subset to exposures
                df[[k]] <- df[[k]][which(df[[k]]$var %nin% c(covariates, strata_term)), ]
                
                # get highest vif value
                if (!is.null(vif.override)) {
                    index <- max(df[[k]][[vif.type]][which(df[[k]]$var != vif.override)])
                } else {
                    index <- max(df[[k]][[vif.type]])
                }
                
                if (index > vif.limit) {
                    remove <- which(df[[k]][[vif.type]] == index)
                    removed <- df[[k]]$var[remove]
                    cat("\nVariable to remove:", removed, "\n", "\n")
                    flush.console()
                    remove_set <- c(remove_set, removed)
                    keep_set <- df[[k]]$var[which(df[[k]]$var %nin% remove_set)]
                    model_vars <- paste0(keep_set, collapse = " + ")
                    i <- 1
                } else if (index <= vif.limit) {
                    cat("\nVariable to remove: none\n", "\n")
                    flush.console()
                    remove_set <- c(remove_set)
                    keep_set <- df[[k]]$var[which(df[[k]]$var %nin% remove_set)]
                    i <- 0
                }
            }
            
            vif_vars[[k]] <- keep_set
            vif_remove[[k]] <- remove_set
            model_vars <- paste0(keep_set, collapse = " + ")
            
            # Run single model ---------------------------------------------------------------
            
            cat("\r",paste(paste("\rRunning single model..... cluster",k), length(clusters), sep = "/"))
            flush.console()
            # Sys.sleep(1.5)
            
            # run in single regression
            if (multi.imputation == TRUE) {
                
                model <- lapply(data, model_function, y = model_vars)
                models[[k]] <- mice::pool(model)
                rm(model)
                
            } else {
                
                models[[k]] <- model_function(data, y = model_vars)
            }
        }
        invisible(gc())
    }

    # Extract significant vars --------------------------------------------------------

    cat("\nExtracting significant variables...")
    flush.console()
    
    # make lists
    pools <- models
    sig_vars <- as.list(seq_along(pools))
    non_sig_vars <- as.list(seq_along(pools))
    covar_remove <- paste(covariates, collapse = "|")
    
    if (!is.null(vars.list)) {
        variables <- vars.list
    } else {
        variables <- keep_set
    }
    
    # run loop to retrieve significant and non-significant variables
    for (k in seq_along(sig_vars)) {
        
        if (is.list(pools[[k]]) == FALSE) {
            
            sig_vars[[k]] <- "remove.this.var"
            non_sig_vars[[k]] <- "remove.this.var"
            
        } else {
            
            # make df of summary of regression results
            pool_df <- summary(pools[[k]])
            # remove covariate rows
            if (!is.null(vars.list)) {
                vars_keep <- paste(vars.list, collapse = "|")
                pool_df <- pool_df[grepl(vars_keep, pool_df$term),]
            } else {
                pool_df <- pool_df[!grepl(covar_remove, pool_df$term),]
            }
            # make new row for variable name
            pool_df$Variable <- NA
            
            # add variable names based on partial match with term
            for(j in seq_along(variables)) {
                pool_df$Variable[grepl(variables[[j]], pool_df$term)] <- variables[[j]]
            }
            
            # get all significant cluster exposures 
            sig_vars[[k]] <- pool_df$Variable[which(pool_df$p.value < 0.05)]
            # get all non-significant cluster exposures 
            non_sig_vars[[k]] <- pool_df$Variable[which(pool_df$p.value >= 0.05)]
        }
    }
    
    # merge significant and non-significant vars across all models
    sig_vars <- unique(unlist(sig_vars))
    sig_vars <- sig_vars[which(sig_vars != "remove.this.var")]
    
    non_sig_vars <- unique(unlist(non_sig_vars))
    non_sig_vars <- non_sig_vars[which(non_sig_vars != "remove.this.var")]
    non_sig_vars <- non_sig_vars[which(non_sig_vars %nin% sig_vars)]

    rm(pools)

    # Return ------------------------------------------------------------------
    
    cat("done", "\n")
    flush.console()
    
    if (multi.imputation == TRUE) {
        
            cluster_results <- list(
                "pooled_models" = models,
                "vif.models" = df,
                "sig.vars" = sig_vars,
                "non.sig.vars" = non_sig_vars,
                "model_formula" = formula
            )
        
        class(cluster_results) <- "rXWAS"
        return(cluster_results) 
        
    } else if (multi.imputation == FALSE) {
        
        cluster_results <- list(
            "models" = models,
            "vif.models" = df,
            "sig.vars" = sig_vars,
            "non.sig.vars" = non_sig_vars,
            "model_formula" = formula
        )
        
        class(cluster_results) <- "rXWAS"
        return(cluster_results) 
    }
    
}

