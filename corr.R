
create_corr_df <- function(exclude_ids = FALSE) {
  source_script("all")
  
  if(!exists("all", envir = globalenv())) {
    data <- create_all_df(lpa = TRUE)
  } else {
    data <- all
  }
  
  if(exclude_ids == TRUE) data <- remove_excluded_ids(data, cutoff = 0.5)
  
  data <- data %>%
    select(ID, day, age, 
           PHQ, PHQ_change, PHQ_RMSSD, PHQ_T0, PHQ_T1,
           screentime, screentime_RMSSD, lpa_group,
           phoenix, H_total, UEQ, SUTAQ,
           everyday, productivity, games, health, creativity, shopping, 
           social, system, tools, entertainment, education, news
    ) %>%
    group_by(ID) %>% 
    summarize_all(mean, na.rm = TRUE) %>% 
    select(-ID, -day)
  
  return(data)
}

# Matrix
create_corr_matrix <- function(data = NULL, clean = NULL, sig = NULL, save = FALSE, exclude_ids = FALSE) {
  if(is.null(data)) {
    data <- create_corr_df(exclude_ids = exclude_ids)
    }
  
  if(!requireNamespace("Hmisc", quietly = TRUE)) {
    install.packages("Hmisc")
  }
  require(Hmisc)
  
  # calculate matrix
  corr_result <- rcorr(as.matrix(data), type = "pearson")
  r <- corr_result$r
  p <- corr_result$P
  
  # Combine matrices
  n <- nrow(r)
  combined_matrix <- matrix("", n, n)
  rownames(combined_matrix) <- rownames(r)
  colnames(combined_matrix) <- colnames(r)
  
  # Apply formatting for significance
  if(is.null(sig) || sig == "none") {
    # only r-values without significance
    for(i in 1:n) {
      for(j in 1:n) {
        if(!is.na(r[i,j])) {
          combined_matrix[i,j] <- formatC(r[i,j], digits=2, format="f")
        } else {
          combined_matrix[i,j] <- "NA"
        }
      }
    }
  } else if(sig == "short") {
    # r-values with asterisk for significance
    for(i in 1:n) {
      for(j in 1:n) {
        if(!is.na(r[i,j]) && !is.na(p[i,j])) {
          combined_matrix[i,j] <- paste0(
            formatC(r[i,j], digits=2, format="f"),
            ifelse(p[i,j] < 0.05, "*", ""),
            ifelse(p[i,j] < 0.01, "*", ""),
            ifelse(p[i,j] < 0.001, "*", "")
          )
        } else {
          combined_matrix[i,j] <- "NA"
        }
      }
    }
  } else if(sig == "long") {
    # r-values with p-values in brackets
    for(i in 1:n) {
      for(j in 1:n) {
        if(!is.na(r[i,j]) && !is.na(p[i,j])) {
          r_formatted <- sprintf("%.2f", r[i,j])
          
          if(p[i,j] < 0.001) {
            p_formatted <- "<0.001"
          } else {
            p_formatted <- sprintf("%.3f", p[i,j])
          }
          
          combined_matrix[i,j] <- paste0(r_formatted, " (", p_formatted, ")")
        } else {
          combined_matrix[i,j] <- "NA"
        }
      }
    }
  }
  
  for(i in 1:n) {
    combined_matrix[i,i] <- "1.00"
  }
  
  # Remove duplicate values
  result <- combined_matrix
  if(!is.null(clean)) {
    if(clean == "upper") {
      result[upper.tri(result)] <- NA
    } else if(clean == "lower") {
      result[lower.tri(result)] <- NA
    }
  }
  
  # save matrix
  if(save) {
    save_csv()
    }
  
  return(result)
}






# plot
create_corr_heatmap <- function(data = NULL) {
  if(is.null(data)) {
    if(exists("corr_df")) {
      data <- corr_df
    } else {
      data <- create_corr_df()
    }
  }
  
  variable_names <- c(
    "Age",
    "PHQ", "Change", "PHQ RMSSD", "T0", "T1",
    "Screentime", "St RMSSD", "Cluster",
    "Intervention", "Tasks", "UEQ", "SUTAQ",
    "Daily Life", "Productivity", "Games", "Health", "Creativity", "Shopping",
    "Social", "System", "Tools", "Entertainment", "Education", "News"
  )
  
  # group definition
  group_labels <- c(
    rep("General", 1),
    rep("PHQ", 5),
    rep("Screentime", 3),
    rep("Intervention", 4),
    rep("Categories", 12)
  )
  
  corr_result <- rcorr(as.matrix(data), type = "pearson")
  r <- corr_result$r
  p <- corr_result$P
  
  # Create a mapping for the groups
  group_data <- data.frame(
    variable = colnames(r),
    display_name = variable_names,
    group = group_labels,
    stringsAsFactors = FALSE
  )
  
  # Calculate the cumulative number of variables per group
  group_counts <- table(group_labels)
  group_ends <- cumsum(group_counts)
  group_starts <- c(1, group_ends[-length(group_ends)] + 1)
  group_mids <- (group_starts + group_ends) / 2
  
  title <- "Correlation Matrix"
  
  n <- ncol(r)
  
  cor_tri <- r
  cor_tri[lower.tri(cor_tri, diag = TRUE)] <- NA
  
  # create long format for ggplot
  melted_cor <- reshape2::melt(cor_tri, na.rm = TRUE)
  colnames(melted_cor) <- c("Var1", "Var2", "value")
  
  # Display names & groups
  melted_cor <- melted_cor %>%
    left_join(group_data, by = c("Var1" = "variable")) %>%
    rename(display_name1 = display_name, group1 = group) %>%
    left_join(group_data, by = c("Var2" = "variable")) %>%
    rename(display_name2 = display_name, group2 = group)
  
  # Faktoren für die richtige Reihenfolge
  melted_cor$display_name1 <- factor(melted_cor$display_name1, levels = variable_names)
  melted_cor$display_name2 <- factor(melted_cor$display_name2, levels = rev(variable_names))
  
  # correct sequence
  group_unique <- unique(group_labels)
  n_groups <- length(group_unique)
  
  # Füge Gruppenfärbung für jede Zeile/Spalte hinzu
  group_colors <- viridis::viridis(n_groups, alpha = 0.2)
  names(group_colors) <- group_unique
  
  # mapping function for line colors
  melted_cor$row_color <- group_colors[melted_cor$group1]
  melted_cor$col_color <- group_colors[melted_cor$group2]
  
  # Create the plot with integrated group markers
  p <- ggplot(melted_cor, aes(x = display_name1, y = display_name2)) +
    # Gruppenblöcke
    geom_tile(aes(fill = value)) +
    # Color scale for correlations
    scale_fill_gradient2(
      low = "darkblue", 
      mid = "white", 
      high = "darkred", 
      midpoint = 0, 
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 11, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  # grid lines
  for (i in 1:n) {
    # Horizontal lines
    p <- p + annotate("segment", 
                      x = 0.5, xend = n + 0.5, 
                      y = i + 0.5, yend = i + 0.5,
                      linetype = "solid", linewidth = 0.2, color = "gray90")
    
    # Vertikal lines
    p <- p + annotate("segment", 
                      x = i + 0.5, xend = i + 0.5, 
                      y = 0.5, yend = n + 0.5,
                      linetype = "solid", linewidth = 0.2, color = "gray90")
  }
  
  # horizontal groups
  for (i in 1:length(group_unique)) {
    indices <- which(group_labels == group_unique[i])
    if (length(indices) > 0) {
      x_start <- min(indices) - 0.5
      x_end <- max(indices) + 0.5
      y_position <- n + 1
      
      # Bars
      p <- p + annotate("rect", 
                        xmin = x_start, xmax = x_end, 
                        ymin = y_position - 0.4, ymax = y_position + 0.4,
                        fill = group_colors[group_unique[i]], 
                        alpha = 0.8)
      
      # names
      p <- p + annotate("text", 
                        x = (x_start + x_end) / 2, 
                        y = y_position,
                        label = group_unique[i], 
                        fontface = "bold", 
                        size = 2.5)
    }
  }
  
  # vertical groups
  for (i in 1:length(group_unique)) {
    indices <- which(group_labels == group_unique[i])
    if (length(indices) > 0) {
      rev_indices <- n + 1 - indices
      y_start <- min(rev_indices) - 0.5
      y_end <- max(rev_indices) + 0.5
      x_position <- n + 1
      
      # bars
      p <- p + annotate("rect", 
                        xmin = x_position - 0.4, xmax = x_position + 0.4, 
                        ymin = y_start, ymax = y_end,
                        fill = group_colors[group_unique[i]], 
                        alpha = 0.8)
      
      # names
      p <- p + annotate("text", 
                        x = x_position, 
                        y = (y_start + y_end) / 2,
                        label = group_unique[i], 
                        fontface = "bold", 
                        size = 2.5,
                        angle = 270)
    }
  }
  
  # Extend plot boundaries for group labels
  p <- p + coord_cartesian(xlim = c(0, n + 1), ylim = c(0, n + 1)) #c(0.5, n + 2), ylim = c(0.5, n + 2)
  
  # grid lines for group boundaries
  for (i in 1:(length(group_unique) - 1)) {
    current_group = group_unique[i]
    next_group = group_unique[i+1]
    
    last_idx_current = max(which(group_labels == current_group))
    
    # horizontal
    p <- p + annotate("segment", 
                      x = 0.5, xend = n + 0.5,
                      y = n - last_idx_current + 0.5, yend = n - last_idx_current + 0.5,
                      linetype = "dashed", linewidth = 0.3, color = "gray50")
    
    # vertical
    # Main
    p <- p + annotate("segment", 
                      x = last_idx_current + 0.5, xend = last_idx_current + 0.5, 
                      y = 0.5, yend = n + 0.5,
                      linetype = "dashed", linewidth = 0.3, color = "gray50")
    
    # Oblique line
    extension_length = 2
    
    p <- p + annotate("segment", 
                      x = last_idx_current + 0.5, 
                      y = 0.5, 
                      xend = last_idx_current + 0.5 - extension_length, # Versatz nach links wegen 45° Winkel
                      yend = 0.5 - extension_length, # Nach unten versetzt
                      linetype = "dashed", linewidth = 0.3, color = "gray50")
  }
  
  return(p)
}


run_t_test <- function(data, x, y, group_values = NULL) {
  y_variable <- data[[y]]
  
  if(is.numeric(y_variable) && !is.factor(y_variable)) {
    return(run_correlation_analysis(data, x, y))
  } else {
    return(run_categorical_t_test(data, x, y, group_values))
  }
}

run_categorical_t_test <- function(data, x, y, group_values = NULL) {
  if(is.null(group_values)) {
    group_values <- unique(data[[y]])
    group_values <- group_values[!is.na(group_values)]
  }
  
  if(length(group_values) != 2) {
    stop("Der t-Test benötigt genau 2 Gruppen.")
  }
  
  # Prepare data for analysis
  formula <- as.formula(paste(x, "~", "as.factor(", y, ")"))
  analysis_data <- data %>% filter(!is.na(!!sym(y)))
  
  results <- list()
  results$test_type <- "t_test"
  
  results$levene <- car::leveneTest(formula, data = analysis_data)
  var_equal <- results$levene$`Pr(>F)`[1] > 0.05
  
  results$shapiro <- list()
  results$shapiro[[1]] <- shapiro.test(analysis_data[[x]][analysis_data[[y]] == group_values[1]])
  results$shapiro[[2]] <- shapiro.test(analysis_data[[x]][analysis_data[[y]] == group_values[2]])
  
  # QQ-Plots
  par(mfrow = c(1, 2))
  qqnorm(analysis_data[[x]][analysis_data[[y]] == group_values[1]], 
         main = paste("QQ-Plot für Gruppe", group_values[1]))
  qqline(analysis_data[[x]][analysis_data[[y]] == group_values[1]])
  
  qqnorm(analysis_data[[x]][analysis_data[[y]] == group_values[2]], 
         main = paste("QQ-Plot für Gruppe", group_values[2]))
  qqline(analysis_data[[x]][analysis_data[[y]] == group_values[2]])
  par(mfrow = c(1, 1))
  
  # t-Test
  results$t_test <- t.test(formula, data = analysis_data, var.equal = var_equal, alternative = "two.sided")
  
  # calculate sd
  results$sd <- c(
    sd(analysis_data[[x]][analysis_data[[y]] == group_values[1]], na.rm = TRUE),
    sd(analysis_data[[x]][analysis_data[[y]] == group_values[2]], na.rm = TRUE)
  )
  names(results$sd) <- c(paste("Gruppe", group_values[1]), paste("Gruppe", group_values[2]))
  
  # effect size
  results$cohen_d <- effsize::cohen.d(formula, data = analysis_data)
  
  return(results)
}

# function if y is continuous
run_correlation_analysis <- function(data, x, y) {
  # Prepare data for analysis
  analysis_data <- data %>% filter(!is.na(!!sym(x)) & !is.na(!!sym(y)))
  
  results <- list()
  results$test_type <- "correlation"
  
  results$shapiro <- list()
  results$shapiro$x <- shapiro.test(analysis_data[[x]])
  results$shapiro$y <- shapiro.test(analysis_data[[y]])
  
  # QQ-Plots
  par(mfrow = c(1, 2))
  qqnorm(analysis_data[[x]], main = paste("QQ-Plot für", x))
  qqline(analysis_data[[x]])
  
  qqnorm(analysis_data[[y]], main = paste("QQ-Plot für", y))
  qqline(analysis_data[[y]])
  par(mfrow = c(1, 1))
  
  # scatter diagram
  plot(analysis_data[[x]], analysis_data[[y]], 
       main = paste("Streudiagramm", x, "vs.", y),
       xlab = x, ylab = y)
  abline(lm(analysis_data[[y]] ~ analysis_data[[x]]), col = "red")
  
  # Calculate correlations (Pearson and Spearman)
  # Pearson (normal distributed data)
  results$pearson <- cor.test(analysis_data[[x]], analysis_data[[y]], 
                              method = "pearson")
  
  # Spearman (not-normal distributed data)
  results$spearman <- cor.test(analysis_data[[x]], analysis_data[[y]], 
                               method = "spearman")
  
  # linear Regression
  results$linear_model <- lm(formula = as.formula(paste(y, "~", x)), 
                             data = analysis_data)
  results$model_summary <- summary(results$linear_model)
  
  return(results)
}