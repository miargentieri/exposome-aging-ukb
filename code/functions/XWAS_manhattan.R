
list.of.packages <- c("ggplot2", "plotly", "randomcoloR", "Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))))



manhattan <- function(
        data,
        x,
        y,
        interactive = FALSE,
        plotly.font = NULL,
        jitter.width = 0,
        jitter.height = 0,
        point.size = 3,
        legend.point.size = 5,
        estimate.label = NULL,
        estimate.label.col = NULL,
        exposure.label.col = NULL,
        generate.colors = FALSE,
        color.seed = NULL,
        color = NULL,
        title = NULL,
        subtitle = NULL, 
        caption = NULL,
        x.lab = NULL,
        y.lab = NULL,
        sig.line = NULL,
        legend.title = NULL,
        theme = NULL,
        theme.size = NULL,
        theme.family = NULL,
        theme.line.size = NULL,
        theme.rect.size = NULL,
        ...
) {
    
    ### Startup errors ----------------------------------------------------------
    
    `%nin%` <- Negate(`%in%`)
    
    theme_list <- c('gray', 'bw', 'linedraw', 'light', 'dark', 'minimal', 'classic', 'void', 'test')
    
    if (missing(data)) {
        stop("'data' must be supplied")
    } else if (missing(x)) {
        stop("'x' must be supplied")
    } else if (missing(y)) {
        stop("'y' must be supplied")
    } else if (!is.null(theme) & theme %nin% theme_list) {
        stop("'theme' must be one of 'gray', 'bw', 'linedraw', 'light', 'dark', 'minimal', 'classic', 'void', 'test")
    } else if (!is.null(plotly.font) & !is.list(plotly.font)) {
        stop("'plotly.font' must be supplied as a list")
    }
    
    # Labels ------------------------------------------------------------------
    
    if (!is.null(estimate.label)) {
        
        estimate.label <- paste0(estimate.label, ":")
        
    }
    
    # Colors ------------------------------------------------------------------
    
    if (generate.colors == TRUE) {
        
        if (!is.null(color.seed)) {
            set.seed(color.seed)
        } else {
            set.seed(789)
        }
        
        color <- c(randomcoloR::distinctColorPalette(k = length(unique(data[[x]])), altCol = FALSE, runTsne = FALSE))
        color_map <- data.frame(sequence = unique(sort(plots_df$Category)), color = color)
        data <- merge(data, color_map, by.x = "Category", by.y = "sequence", all = TRUE)
        
    }
    

    # gray --------------------------------------------------------------------

    if (theme == "gray") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_gray(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # bw ----------------------------------------------------------------------
    
    if (theme == "bw") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_bw(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
       
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }

    # linedraw ----------------------------------------------------------------

    if (theme == "linedraw") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_linedraw(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # light -------------------------------------------------------------------

    if (theme == "light") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_light(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # dark --------------------------------------------------------------------
    
    if (theme == "dark") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_dark(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # minimal -----------------------------------------------------------------
    
    if (theme == "minimal") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_minimal(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # classic -----------------------------------------------------------------
    
    if (theme == "classic") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_classic(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # void --------------------------------------------------------------------
    
    if (theme == "void") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_void(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
    
    # test --------------------------------------------------------------------
    
    if (theme == "test") {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme_test(
                base_size = theme.size,
                base_family = theme.family,
                base_line_size = theme.line.size,
                base_rect_size = theme.rect.size) +
            theme(...)
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }

        # no theme ----------------------------------------------------------------
        
    if (is.null(theme)) {
        
        manhattan <- ggplot2::ggplot(
            data, 
            aes(x = .data[[x]], 
                y = .data[[y]], 
                label = .data[[exposure.label.col]],
                text = paste(estimate.label, 
                             format(round(.data[[estimate.label.col]], 2), nsmall = 2))
            )) + 
            geom_point(
                aes(color = .data[[x]]), 
                size = point.size, 
                # jitter points so they are spaced horizontally
                position = position_jitter(width = jitter.width, 
                                           height = jitter.height)) + 
            # increase point size in legend
            guides(colour = guide_legend(override.aes = list(size = legend.point.size))) + 
            scale_color_manual(values = unique(color)) +
            # ggtitle(title) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 x = x.lab,
                 y = y.lab,
                 color = legend.title) +
            # FDR line
            geom_hline(yintercept = sig.line, 
                       color = "blue", 
                       linetype = "dashed") + 
            theme(...)
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            # plotly interactive plot
            fig <- plotly::ggplotly(
                manhattan, 
                tooltip = c("x", 
                            "y", 
                            "label", 
                            "text")) %>% 
                layout(title = title, 
                       font = t)
            
            return(fig)
            
        } else {
            
            return(manhattan)
        }
    }
}


