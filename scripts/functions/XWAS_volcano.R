
list.of.packages <- c("ggplot2", "plotly", "randomcoloR", "Hmisc", "dplyr", "magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))))



volcano <- function(
        data,
        x,
        y,
        interactive = FALSE,
        plotly.font = NULL,
        category = NULL,
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
        x.lim = NULL,
        y.lim = NULL,
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
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_gray(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_gray(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # bw ----------------------------------------------------------------------
    
    if (theme == "bw") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_bw(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_bw(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # linedraw ----------------------------------------------------------------
    
    if (theme == "linedraw") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_linedraw(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_linedraw(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # light -------------------------------------------------------------------
    
    if (theme == "light") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_light(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_light(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # dark --------------------------------------------------------------------
    
    if (theme == "dark") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_dark(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_dark(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # minimal -----------------------------------------------------------------
    
    if (theme == "minimal") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_minimal(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_minimal(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # classic -----------------------------------------------------------------
    
    if (theme == "classic") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_classic(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_classic(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # void --------------------------------------------------------------------
    
    if (theme == "void") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_void(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_void(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # test --------------------------------------------------------------------
    
    if (theme == "test") {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_test(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme_test(
                    base_size = theme.size,
                    base_family = theme.family,
                    base_line_size = theme.line.size,
                    base_rect_size = theme.rect.size) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
    
    # no theme ----------------------------------------------------------------
    
    if (is.null(theme)) {
        
        if (!is.null(sig.line)) {
            
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme(...)
            
        } else {
            
            # calling ggplotly with empty geom_hline causes error
            volcano <- ggplot2::ggplot(
                data %>%
                    arrange(color), 
                aes(x = .data[[x]], 
                    y = .data[[y]], 
                    label = .data[[exposure.label.col]],
                    label2 = .data[[category]],
                    text = paste(estimate.label, 
                                 format(round(.data[[estimate.label.col]], 2), nsmall = 2))
                )) + 
                geom_point(
                    aes(color = .data[[category]]), 
                    size = point.size) + 
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
                scale_x_continuous(limits = x.lim) +
                scale_y_continuous(limits = y.lim) +
                theme(...)
            
        }
        
        
        if (interactive == TRUE) {
            
            if (!is.null(plotly.font)) {
                
                t <- plotly.font
                
            } else {
                
                t <- list(family = "sans-serif",
                          size = 18,
                          color = 'black')
                
            }
            
            if (!is.null(category)) {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label", 
                                "label2")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            } else {
                
                # plotly interactive plot
                fig <- plotly::ggplotly(
                    volcano, 
                    tooltip = c("text", 
                                "y", 
                                "label")) %>% 
                    layout(title = title, 
                           font = t)
                
                return(fig)
                
            }
            
        } else {
            
            return(volcano)
        }
    }
}
