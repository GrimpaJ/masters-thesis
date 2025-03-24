library(dplyr)
library(rlang)
library(knitr)
library(kableExtra)

reorder_columns <- function(data, desired_order = NULL) {
  if (is.null(desired_order)) {
    desired_order <- c("ID", "sex", "age_group", "age", "app_data", "day", 
                       "marital_status", "residence", "nationality", "ed_qual", "occupation",
                       "cur_therapy", "clin", "mult_diag", "dmhi_exp", 
                       "PHQ", "RMSSD", "PHQ_change", "PHQ_T0", "PHQ_T1",
                       paste0("PHQ_", 1:9),
                       "SUTAQ", "UEQ",
                       "screentime", "screentime_RMSSD",
                       "creativity", "education", "entertainment", "everyday", "games", "health", "news", "productivity", "shopping", "social", "system", "tools"
                       )
  }
  
  # Create a list with all existing columns in the desired order
  existing_cols <- desired_order[desired_order %in% names(data)]
  
  # Find all columns that are not listed in desired_order
  other_cols <- setdiff(names(data), desired_order)
  
  # Select the columns in the desired order, followed by all other columns
  data %>% select(all_of(existing_cols), all_of(other_cols))
}

rm_dup_col <- function(data) {
  dup_cols <- duplicated(names(data)) | duplicated(names(data), fromLast = TRUE)
  if (any(dup_cols)) {
    warning("Doppelte Spaltennamen gefunden und bereinigt: ", 
            paste(unique(names(data)[dup_cols]), collapse = ", "))
    
    # only keep first column
    data <- data[, !duplicated(names(data))]
  }
  
  return(data)
}

shorten_by_id <- function(data, all_vars) {
  
  all_vars_to_keep <- unique(all_vars)

  # Identify numeric and categorical variables
  numeric_vars <- all_vars_to_keep[sapply(data[all_vars_to_keep], is.numeric)]
  categorical_vars <- setdiff(all_vars_to_keep, numeric_vars)
  
  if (length(numeric_vars) > 0) {
    numeric_summary <- data %>%
      group_by(ID) %>%
      summarise(across(all_of(numeric_vars), 
                       ~mean(.x, na.rm = TRUE)),
                .groups = "drop")
  } else {
    numeric_summary <- data %>% select(ID) %>% distinct()
  }
  
  # Handle categorical variables
  if (length(categorical_vars) > 0) {
    cat_summary <- data.frame(ID = unique(data$ID))
    
    for (var in categorical_vars) {
      
      # Get most frequent value for each ID
      mode_per_id <- data %>%
        filter(!is.na(!!sym(var))) %>% 
        group_by(ID, !!sym(var)) %>% 
        summarise(count = n(), .groups = "drop") %>%
        group_by(ID) %>%
        slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
        select(ID, !!sym(var)) %>% 
        distinct(ID, .keep_all = TRUE)
      
      cat_summary <- left_join(cat_summary, mode_per_id, by = "ID")
    }
    
    summarized_data <- left_join(numeric_summary, cat_summary, by = "ID")
  } else {
    summarized_data <- numeric_summary
  }
  
  if("day" %in% names(summarized_data)) {
    summarized_data <- summarized_data %>% 
      select(-day)
  }

  return(summarized_data)
}



#' Create the base dataframe with all variables
#' 
#' @return A dataframe with all variables processed and ready for analysis
create_stats_df <- function() {
  # import data
  data <- process_data("ema")
  
  screenstate <- process_data("screenstate") %>% 
    select(ID, day, time_on) %>% 
    rename(screentime = time_on) 
  
  screentime_RMSSD <- screenstate %>%   
    group_by(ID) %>% 
    arrange(ID, day) %>% 
    mutate(diff_succ = c(NA, diff(screentime)),
           diff_sq = diff_succ^2
           ) %>% 
    summarise(screentime_RMSSD = sqrt(mean(diff_sq, na.rm = TRUE)))
  
  apps <- process_data("app_use") %>% 
    arrange(ID, app_name, day) %>% 
    group_by(ID, category, day) %>% 
    summarise(time = sum(fg_time_ms), .groups = "drop") %>% 
    pivot_wider(
      id_cols = c(ID, day),
      names_from = category,
      values_from = time,
      values_fill = 0
      )
  
  ue <- process_data("ue") %>% 
    select(ID, SUTAQ, UEQ)
  
  app_info <- process_data("codebook") %>%
    select(ID, app_usage)
  
  sd_info <- process_data("demography")
  
  sosci_t1 <- process_data("sosci") %>%
    filter(Time == 'T1') %>%
    select(ID, PHQ) %>%
    rename(PHQ_T1 = PHQ)

  sosci_t0 <- process_data("sosci") %>%
    filter(Time == 'T0') %>%
    select(ID, PHQ) %>%
    rename(PHQ_T0 = PHQ)
  
  # add info to main df
  data <- left_join(data, sd_info, by = "ID")
  data <- left_join(data, app_info, by = "ID")
  data <- left_join(data, sosci_t1, by = "ID")
  data <- left_join(data, sosci_t0, by = "ID")
  data <- left_join(data, screenstate, by = c("ID", "day"))
  data <- left_join(data, screentime_RMSSD, by = "ID")
  data <- left_join(data, apps, by = c("ID", "day"))
  data <- left_join(data, ue, by = "ID")
  
  # change stuff
  data <- data %>%
    rename(app_data = app_usage)
  
  data <- reorder_columns(data)
  
  return(data)
}


#' Summarize statistics by group
#' 
#' @param data The input dataframe
#' @param groups A character vector of grouping variables
#' @param vars A character vector of variables to summarize
#' @return A dataframe with summarized statistics for each group
group_stats <- function(data, groups, vars = NULL, stats_mode = "auto", stats_override_vars = NULL) {
  # Check if data has multiple rows per ID (e.g., day-level data)
  has_multiple_rows_per_id <- FALSE
  if ("ID" %in% names(data)) {
    has_multiple_rows_per_id <- any(table(data$ID) > 1)
  }
  
  if (has_multiple_rows_per_id) {
    all_vars <- c(unlist(vars), unlist(groups))
    data <- shorten_by_id(data, all_vars)
  }
  
  # Filter vars if they conflict with groups
  if (!is.null(vars)) {
    vars <- setdiff(vars, groups)
  }
  
  # Prepare numeric vars for summarizing
  if (is.null(vars)) {
    # Use all numeric columns if none specified
    var_cols <- names(data)[sapply(data, is.numeric)]
    var_cols <- setdiff(var_cols, groups)
  } else {
    var_cols <- vars[sapply(vars, function(v) is.numeric(data[[v]]))]
  }
  
  # Prepare factor vars for summarizing
  if (is.null(vars)) {
    factor_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    factor_cols <- setdiff(factor_cols, groups)
  } else {
    factor_cols <- vars[sapply(vars, function(v) is.factor(data[[v]]) || is.character(data[[v]]))]
    factor_cols <- setdiff(factor_cols, groups)
  }
  
  # Create group expression
  if (length(groups) == 0) {
    group_expr <- NULL
    result <- data
  } else {
    group_expr <- rlang::syms(groups)
    result <- data %>%
      group_by(!!!group_expr)
  }
  
  # Compute summary statistics for numeric variables
  if (length(var_cols) > 0) {
    numeric_summary <- result %>%
      summarise(across(all_of(var_cols), 
                       list(
                         n = ~sum(!is.na(.x)),
                         mean = ~mean(.x, na.rm = TRUE),
                         median = ~median(.x, na.rm = TRUE),
                         sd = ~sd(.x, na.rm = TRUE),
                         min = ~min(.x, na.rm = TRUE),
                         max = ~max(.x, na.rm = TRUE)
                       )),
                .groups = "drop") %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
  } else {
    numeric_summary <- result %>% 
      summarise(.groups = "drop")
  }

  # Compute statistics for factor variables
  factor_summaries <- list()
  for (col in factor_cols) {
    if (length(groups) == 0) {
      factor_sum <- data %>%
        count(!!sym(col)) %>%
        mutate(
          variable = col,
          value = !!sym(col),
          percent = n / sum(n) * 100
        ) %>%
        select(variable, value, n, percent)
    } else {
      factor_sum <- data %>%
        group_by(!!!group_expr, !!sym(col)) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(!!!group_expr) %>%
        mutate(
          variable = col,
          value = !!sym(col),
          percent = n / sum(n) * 100
        ) %>%
        select(!!!group_expr, variable, value, n, percent) %>%
        ungroup()
    }
    factor_summaries[[col]] <- factor_sum
  }
  
  # Combine factor summaries if any exist
  if (length(factor_summaries) > 0) {
    factor_summary <- bind_rows(factor_summaries)
  } else {
    factor_summary <- NULL
  }
  
  # Add group info to result
  if (length(groups) > 0) {
    group_name <- paste(groups, collapse = ":")
    numeric_summary$group <- group_name
    if (!is.null(factor_summary)) {
      factor_summary$group <- group_name
    }
  } else {
    numeric_summary$group <- "total"
    if (!is.null(factor_summary)) {
      factor_summary$group <- "total"
    }
  }
  z_num <<- numeric_summary
  z_fac <<- factor_summary
  return(list(numeric = numeric_summary, factor = factor_summary))
}

#' Create a table with grouped statistics
#' 
#' @param group A character vector of grouping variables
#' @param vars A character vector of variables to include
#' @param data The input dataframe
#' @param total Whether to include a total column
#' @param norm_test The normality test to use ("shapiro" or "none")
#' @param alpha The significance level for tests
#' @return A dataframe formatted as a table
create_table <- function(group, vars, data, total = TRUE, norm_test = "shapiro", alpha = 0.05, stats_mode = "auto", stats_override_vars = NULL) {
  # Ensure group is a character
  if (!is.character(group)) {
    group <- as.character(substitute(group))
  }

  # Sicherheitsmaßnahme, falls Gruppe nicht existiert
  if (!group %in% names(data)) {
    warning(paste("Group variable", group, "not found in the data. Creating a default group."))
    data[[group]] <- "Default"
  }

  # Check if data has multiple rows per ID (e.g., day-level data)
  has_multiple_rows_per_id <- FALSE
  if ("ID" %in% names(data)) {
    has_multiple_rows_per_id <- any(table(data$ID) > 1)
  }
  
  if(has_multiple_rows_per_id) {
    data <- shorten_by_id(data, vars) ### oder all_vars
    }

  # Workaround for total-column
  if (identical(group, "alibi_group")) {
    data[[group]] <- "Total"
    group_levels <- levels(data[[group]])
  } else {
    if (length(group) > 1) {
      # Create a new column for combined grouping variables
      combined_group_name <- paste(group, collapse = "_")
      
      data[[combined_group_name]] <- apply(data[, group, drop = FALSE], 1, function(x) {
        paste(x, collapse = ":")
      })
      
      # Use the combined column as a grouping variable
      data[[combined_group_name]] <- factor(data[[combined_group_name]])
      group_levels <- levels(data[[combined_group_name]])
      
      # Update the grouping variable
      group <- combined_group_name
    } else {
      if (!group %in% names(data) || length(data[[group]]) == 0) {
        stop(paste("Group variable", group, "not found in the data or has no values"))
        }
      
      # Normal grouping with one variable
      data[[group]] <- factor(data[[group]])
      group_levels <- levels(data[[group]])
    }
  }
  
  # Extract variable names
  var_names <- vars
  
  # Prepare the result table
  result <- data.frame(
    Variable = character(),
    Type = character(),
    stringsAsFactors = FALSE
  )
  
  # Add columns for each group level
  for (level in group_levels) {
    result[[level]] <- character()
  }
  
  # Add Total column if requested
  if (total) {
    result[["Total"]] <- character()
  }
  
  # Calculate statistics for each variable
  for (var_name in var_names) {
    if (!var_name %in% colnames(data)) {
      warning(paste("Variable", var_name, "not found in dataset."))
      next
    }
    
    # Skip the grouping variable itself
    if (var_name == group) {
      next
    }
    
    # Identify variable type
    x <- data[[var_name]]
    is_numeric <- is.numeric(x)
    is_factor <- is.factor(x) || is.character(x)
    
    # Add variable name row
    row_idx <- nrow(result) + 1
    result[row_idx, "Variable"] <- var_name
    result[row_idx, "Type"] <- ""
    
    # Fill values with empty strings
    for (level in group_levels) {
      result[row_idx, level] <- ""
    }
    if (total) {
      result[row_idx, "Total"] <- ""
    }
    
    # Count non-NA values
    row_idx <- nrow(result) + 1
    result[row_idx, "Variable"] <- ""
    result[row_idx, "Type"] <- "N"
    
    for (level in group_levels) {
      group_data <- x[data[[group]] == level]
      result[row_idx, level] <- sum(!is.na(group_data))
    }
    
    if (total) {
      result[row_idx, "Total"] <- sum(!is.na(x))
    }
    
    # For numeric variables
    if (is_numeric) {
      if (stats_mode == "auto") {
        if (!is.null(stats_override_vars) && var_name %in% names(stats_override_vars)) {
          is_normal <- stats_override_vars[[var_name]]
        } else {
          # Normality test
          if (norm_test == "shapiro") {
            is_normal <- tryCatch({
              if (length(na.omit(x)) < 5000) {
                p_val <- shapiro.test(na.omit(x))$p.value
                p_val > alpha
              } else {
                p_val <- ks.test(scale(na.omit(x)), "pnorm")$p.value
                p_val > alpha
              }
            }, error = function(e) {
              FALSE
            })
          }
        }
      } else if (stats_mode == "all_normal") {
        is_normal <- TRUE
      } else if (stats_mode == "all_nonnormal") {
        is_normal <- FALSE
      }
      
      # Statistics based on normality
      if (is_normal) {
        # Mean
        row_idx <- nrow(result) + 1
        result[row_idx, "Variable"] <- ""
        result[row_idx, "Type"] <- "M"
        
        for (level in group_levels) {
          group_data <- x[data[[group]] == level]
          result[row_idx, level] <- round(mean(group_data, na.rm = TRUE), 2)
        }
        
        if (total) {
          result[row_idx, "Total"] <- round(mean(x, na.rm = TRUE), 2)
        }
        
        # Standard deviation
        row_idx <- nrow(result) + 1
        result[row_idx, "Variable"] <- ""
        result[row_idx, "Type"] <- "SD"
        
        for (level in group_levels) {
          group_data <- x[data[[group]] == level]
          result[row_idx, level] <- round(sd(group_data, na.rm = TRUE), 2)
        }
        
        if (total) {
          result[row_idx, "Total"] <- round(sd(x, na.rm = TRUE), 2)
        }
      } else {
        # Median
        row_idx <- nrow(result) + 1
        result[row_idx, "Variable"] <- ""
        result[row_idx, "Type"] <- "Mdn"
        
        for (level in group_levels) {
          group_data <- x[data[[group]] == level]
          result[row_idx, level] <- round(median(group_data, na.rm = TRUE), 2)
        }
        
        if (total) {
          result[row_idx, "Total"] <- round(median(x, na.rm = TRUE), 2)
        }
        
        # IQR
        row_idx <- nrow(result) + 1
        result[row_idx, "Variable"] <- ""
        result[row_idx, "Type"] <- "IQR"
        
        for (level in group_levels) {
          group_data <- x[data[[group]] == level]
          result[row_idx, level] <- round(IQR(group_data, na.rm = TRUE), 2)
        }
        
        if (total) {
          result[row_idx, "Total"] <- round(IQR(x, na.rm = TRUE), 2)
        }
      }
      
      # Min and Max
      row_idx <- nrow(result) + 1
      result[row_idx, "Variable"] <- ""
      result[row_idx, "Type"] <- "Min"
      
      for (level in group_levels) {
        group_data <- x[data[[group]] == level]
        result[row_idx, level] <- round(min(group_data, na.rm = TRUE), 2)
      }
      
      if (total) {
        result[row_idx, "Total"] <- round(min(x, na.rm = TRUE), 2)
      }
      
      row_idx <- nrow(result) + 1
      result[row_idx, "Variable"] <- ""
      result[row_idx, "Type"] <- "Max"
      
      for (level in group_levels) {
        group_data <- x[data[[group]] == level]
        result[row_idx, level] <- round(max(group_data, na.rm = TRUE), 2)
      }
      
      if (total) {
        result[row_idx, "Total"] <- round(max(x, na.rm = TRUE), 2)
      }
    }
    
    # For categorical variables
    else if (is_factor) {
      if (!is.factor(x)) {
        x <- as.factor(x)
      }
      
      for (cat in levels(x)) {
        row_idx <- nrow(result) + 1
        result[row_idx, "Variable"] <- ""
        result[row_idx, "Type"] <- cat
        
        for (level in group_levels) {
          group_data <- x[data[[group]] == level]
          count <- sum(group_data == cat, na.rm = TRUE)
          total_count <- sum(!is.na(group_data))
          percentage <- ifelse(total_count > 0, round(count / total_count * 100, 1), 0)
          
          result[row_idx, level] <- paste0(count, " (", percentage, "%)")
        }
        
        if (total) {
          count <- sum(x == cat, na.rm = TRUE)
          total_count <- sum(!is.na(x))
          percentage <- ifelse(total_count > 0, round(count / total_count * 100, 1), 0)
          
          result[row_idx, "Total"] <- paste0(count, " (", percentage, "%)")
        }
      }
    }
  }
  
  return(result)
}

#' Create a table with multiple groups
#' 
#' @param groups A list of character vectors, where each vector contains grouping variables
#' @param vars A character vector of variables to include
#' @param data The input dataframe
#' @param total Whether to include columns for totals
#' @return A dataframe with statistics for multiple groups
create_multi_group_table <- function(groups, vars, data, total = TRUE, stats_mode = "auto", stats_override_vars = NULL) {
  result_list <- list()
  
  for (g in names(groups)) {
    group_vars <- groups[[g]]
    
    # Skip empty group names (for total)
    if (length(group_vars) == 0) {
      if (total) {
        result_list[[g]] <- group_stats(data, character(0), vars)
      }
      next
    }
    result_list[[g]] <- group_stats(data, group_vars, vars, 
                                    stats_mode = stats_mode, 
                                    stats_override_vars = stats_override_vars)
  }
  
  # Combine all results
  numeric_results <- lapply(result_list, function(x) x$numeric)
  factor_results <- lapply(result_list, function(x) x$factor)
  
  # Filter out NULL results
  numeric_results <- numeric_results[!sapply(numeric_results, is.null)]
  factor_results <- factor_results[!sapply(factor_results, is.null)]
  
  result <- list(
    numeric = if (length(numeric_results) > 0) bind_rows(numeric_results) else NULL,
    factor = if (length(factor_results) > 0) bind_rows(factor_results) else NULL
  )
  
  return(result)
}

#' Format a table for display
#' 
#' @param table_data The table data to format
#' @param format The output format (html, latex, etc.)
#' @param caption The table caption
#' @return A formatted table for display
print_grouped_table <- function(table_data, format = "html", caption = NULL) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    warning("Package 'knitr' is needed for formatted output.")
    return(table_data)
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    warning("Package 'kableExtra' is needed for improved formatting.")
    return(knitr::kable(table_data, format = format, caption = caption))
  }
  
  # Create kable table with grouping lines
  k_table <- knitr::kable(table_data, format = format, caption = caption)
  
  # Find rows with variable names (non-empty in the Variable column)
  var_rows <- which(table_data$Variable != "")
  
  # Add grouping lines
  if (format == "html") {
    k_table <- kableExtra::kable_styling(k_table, bootstrap_options = c("striped", "hover"))
    
    # Group by variables
    for (i in var_rows) {
      k_table <- kableExtra::row_spec(k_table, i, bold = TRUE, background = "#f0f0f0")
    }
    
    # Group variable rows
    if (length(var_rows) > 1) {
      for (i in 1:(length(var_rows) - 1)) {
        start_row <- var_rows[i]
        end_row <- var_rows[i + 1] - 1
        k_table <- kableExtra::group_rows(
          k_table, 
          group_label = "", 
          start_row = start_row, 
          end_row = end_row
        )
      }
      
      # Last group
      last_start <- var_rows[length(var_rows)]
      last_end <- nrow(table_data)
      if (last_start < last_end) {
        k_table <- kableExtra::group_rows(
          k_table, 
          group_label = "", 
          start_row = last_start, 
          end_row = last_end
        )
      }
    }
  }
  
  return(k_table)
}

#' Test for differences between groups
#' 
#' @param data The input dataframe
#' @param group The grouping variable
#' @param vars The variables to test
#' @return A dataframe with test results
test_group_differences <- function(data, group, vars, stats_mode = "auto", stats_override_vars = NULL) {
  if (!is.character(group)) {
    group <- as.character(substitute(group))
  }
  
  # Check if data has multiple rows per ID (e.g., day-level data)
  has_multiple_rows_per_id <- FALSE
  if ("ID" %in% names(data)) {
    has_multiple_rows_per_id <- any(table(data$ID) > 1)
  }
  
  # If we have day-level data, summarize by ID first to avoid multiple measurements per person
  if (has_multiple_rows_per_id) {
    # Create a list of variables to summarize
    all_vars_to_keep <- unique(c(groups, vars))
    
    # Identify numeric and categorical variables
    numeric_vars <- all_vars_to_keep[sapply(data[all_vars_to_keep], is.numeric)]
    categorical_vars <- setdiff(all_vars_to_keep, numeric_vars)
    
    # Summarize numeric variables by ID (mean)
    if (length(numeric_vars) > 0) {
      numeric_summary <- data %>%
        group_by(ID) %>%
        summarise(across(all_of(numeric_vars), ~mean(.x, na.rm = TRUE)),
                  .groups = "drop")
    } else {
      numeric_summary <- data %>% select(ID) %>% distinct()
    }
    
    # Handle categorical variables (most frequent value)
    if (length(categorical_vars) > 0) {
      cat_summary <- data.frame(ID = unique(data$ID))
      
      for (var in categorical_vars) {
        if (var %in% names(data)) {
          #data <- rm_dup_col(data)
          
          # Get most frequent value for each ID
          mode_per_id <- data %>%
            filter(!is.na(!!sym(var))) %>%
            group_by(ID, !!sym(var)) %>%
            summarise(count = n(), .groups = "drop") %>%
            group_by(ID) %>%
            slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
            select(ID, !!sym(var)) %>% 
            distinct(ID, .keep_all = TRUE)
          
          # Merge with summary
          cat_summary <- left_join(cat_summary, mode_per_id, by = "ID")
        }
      }
      
      # Combine summaries
      summarized_data <- left_join(numeric_summary, cat_summary, by = "ID")
    } else {
      summarized_data <- numeric_summary
    }
    
    # Use the summarized data for testing
    data <- summarized_data
  }
  
  # Prepare results dataframe
  results <- data.frame(
    Variable = character(),
    Test = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    Significance = character(),
    stringsAsFactors = FALSE
  )
  
  for (var_name in vars) {
    if (!var_name %in% colnames(data)) {
      warning(paste("Variable", var_name, "not found in dataset."))
      next
    }
    
    # Skip if this variable is the same as the grouping variable
    if (var_name == group) {
      next
    }
    
    x <- data[[var_name]]
    is_numeric <- is.numeric(x)
    
    # For numeric variables
    if (is_numeric) {
      # Check for normality
      is_normal <- tryCatch({
        shapiro_result <- tapply(x, data[[group]], function(x) {
          if (length(na.omit(x)) < 3) return(FALSE)
          if (length(na.omit(x)) < 5000) {
            shapiro.test(na.omit(x))$p.value > 0.05
          } else {
            ks.test(scale(na.omit(x)), "pnorm")$p.value > 0.05
          }
        })
        all(shapiro_result, na.rm = TRUE)
      }, error = function(e) {
        FALSE
      })
      
      # Check for homogeneity of variance
      homogeneity <- tryCatch({
        if (length(levels(factor(data[[group]]))) > 1) {
          levene_result <- car::leveneTest(x ~ factor(data[[group]]))
          levene_result[["Pr(>F)"]][1] > 0.05
        } else {
          TRUE
        }
      }, error = function(e) {
        FALSE
      })
      
      # Select appropriate test
      if (is_normal && homogeneity && length(levels(factor(data[[group]]))) > 1) {
        # ANOVA
        test_result <- tryCatch({
          aov_result <- aov(x ~ factor(data[[group]]))
          anova_table <- summary(aov_result)[[1]]
          test_name <- "ANOVA"
          statistic <- anova_table[["F value"]][1]
          p_value <- anova_table[["Pr(>F)"]][1]
          list(test = test_name, statistic = statistic, p_value = p_value)
        }, error = function(e) {
          list(test = "Error", statistic = NA, p_value = NA)
        })
      } else if (length(levels(factor(data[[group]]))) == 2) {
        # t-test or Wilcoxon
        test_result <- tryCatch({
          if (is_normal && homogeneity) {
            t_result <- t.test(x ~ factor(data[[group]]), var.equal = TRUE)
            test_name <- "t-test"
            statistic <- t_result$statistic
            p_value <- t_result$p.value
          } else {
            wilcox_result <- wilcox.test(x ~ factor(data[[group]]))
            test_name <- "Wilcoxon"
            statistic <- wilcox_result$statistic
            p_value <- wilcox_result$p.value
          }
          list(test = test_name, statistic = statistic, p_value = p_value)
        }, error = function(e) {
          list(test = "Error", statistic = NA, p_value = NA)
        })
      } else if (length(levels(factor(data[[group]]))) > 2) {
        # Kruskal-Wallis
        test_result <- tryCatch({
          kw_result <- kruskal.test(x ~ factor(data[[group]]))
          test_name <- "Kruskal-Wallis"
          statistic <- kw_result$statistic
          p_value <- kw_result$p.value
          list(test = test_name, statistic = statistic, p_value = p_value)
        }, error = function(e) {
          list(test = "Error", statistic = NA, p_value = NA)
        })
      } else {
        test_result <- list(test = "N/A", statistic = NA, p_value = NA)
      }
    } else { # For categorical variables
      # Chi-square test
      test_result <- tryCatch({
        # Create contingency table
        cont_table <- table(data[[var_name]], data[[group]])
        if (min(cont_table) >= 5 && nrow(cont_table) > 1 && ncol(cont_table) > 1) {
          chi_result <- chisq.test(cont_table)
          test_name <- "Chi-square"
          statistic <- chi_result$statistic
          p_value <- chi_result$p.value
        } else if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
          fisher_result <- fisher.test(cont_table)
          test_name <- "Fisher's Exact"
          statistic <- NA
          p_value <- fisher_result$p.value
        } else {
          test_name <- "N/A"
          statistic <- NA
          p_value <- NA
        }
        list(test = test_name, statistic = statistic, p_value = p_value)
      }, error = function(e) {
        list(test = "Error", statistic = NA, p_value = NA)
      })
    }
    
    # Add to results
    results <- rbind(results, data.frame(
      Variable = var_name,
      Test = test_result$test,
      Statistic = round(as.numeric(test_result$statistic), 3),
      P_Value = round(as.numeric(test_result$p_value), 4),
      Significance = ifelse(is.na(test_result$p_value), "", 
                            ifelse(test_result$p_value < 0.001, "***",
                                   ifelse(test_result$p_value < 0.01, "**",
                                          ifelse(test_result$p_value < 0.05, "*", "n.s.")))),
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' The main function to create descriptive statistics
#' 
#' @param type The type of output ("table", "table_grouped", "df", "df_raw")
#' @param vars Variables to include in the analysis
#' @param var_groups Groups of variables to include
#' @param group Grouping variables
#' @param group_mult Whether to use multiplicative grouping
#' @param total Whether to include totals
#' @param sig_test Whether to perform significance tests
#' @param data The input dataframe (optional)
#' @param format The output format
#' @param caption The table caption
#' @return The requested statistics output
descriptive_stats <- function(type = "table", 
                              vars = NULL, 
                              var_groups = NULL, 
                              group = NULL, 
                              group_mult = TRUE, 
                              total = TRUE, 
                              sig_test = FALSE, 
                              data = NULL,
                              format = "html",
                              caption = NULL,
                              stats_mode = "auto",
                              stats_override_vars = NULL) {
  
  get_var_group <- function(group_name) {
    var_groups <- list(
      basic = c("app_data", "age", "age_group", "sex"),
      sd = c("age", "age_group", "sex", "marital_status", "residence", "nationality", "ed_qual", "occupation"),
      clin = c("dmhi_exp", "cur_therapy", "clin", "mult_diag"),
      phq = c("PHQ", "PHQ_change", "RMSSD", "PHQ_T0", "PHQ_T1"),
      phq_items = c(paste0("PHQ_", 1:9)),
      apps = c("screentime", "screentime_RMSSD", "creativity", "entertainment", "everyday", "games", 
               "health", "productivity", "shopping", "social", "system", "tools", "education", "news"),
      ue = c("SUTAQ", "UEQ")
    )
    
    if (!group_name %in% names(var_groups)) {
      stop(paste("Variable group", group_name, "not found. Available groups are:", paste(names(var_groups), collapse = ", ")))
    }
    
    return(var_groups[[group_name]])
  }
  
  # Default variables if none specified
  var_groups <- if(is.null(vars) && is.null(var_groups)) c("basic", "phq") else var_groups
  
  # Expand variable groups to individual variables
  expanded_vars <- unlist(lapply(var_groups, get_var_group))
  
  # Combine with individually specified variables
  all_vars <- unique(c(expanded_vars, vars %||% character(0)))
  
  # set data
  stats_data <- if(is.null(data)) create_stats_df() else data
  
  if ("day" %in% names(stats_data) && 
      !("day" %in% all_vars) && 
      !identical(group, "day")) {
    stats_data <- stats_data %>% select(-day)
  }
  
  if(type == "df_raw") return(stats_data)
  
  if(type == "df_id") return(shorten_by_id(data = stats_data, all_vars = all_vars))

  # Create appropriate output based on type
  if (type == "table") {
    # Simple table without grouping
    if (is.null(group)) {
      # Expliziter Ansatz zur Erstellung der Gruppierungsvariable
      group <- "alibi_group"
      stats_data <- stats_data %>% 
        mutate(alibi_group = "gleich")
      
      result_table <- create_table(group = group,
                                   vars = all_vars, 
                                   data = stats_data, 
                                   total = TRUE,
                                   stats_mode = stats_mode,
                                   stats_override_vars = stats_override_vars
                                   )
      
      # Keep only the Total column to essentially create an ungrouped table
      result_table <- result_table[, c("Variable", "Type", "Total")]
      colnames(result_table)[3] <- "Value"
      
    } else {
      # Table with single grouping
      result_table <- create_table(group = group, 
                                   vars = all_vars, 
                                   data = stats_data, 
                                   total = total,
                                   stats_mode = stats_mode,
                                   stats_override_vars = stats_override_vars
                                   )
    }
    
    if (format != "raw") {
      return(print_grouped_table(result_table, format = format, caption = caption))
    } else {
      return(result_table)
    }
  } else if (type == "table_grouped") {
    # Table with grouping
    if (is.null(group)) {
      stop("For 'table_grouped' type, you must specify a grouping variable.")
    }
    
    # Handle multiple grouping variables
    if (length(group) > 1 && group_mult) {
      # For multiplicative grouping
      groups <- list()
      
      if (total) {
        groups["total"] <- list(character(0))
      }
      
      for (g in group) {
        groups[g] <- list(g)
      }
      
      # ist das if hier überhaupt nötig... ist doch immer so
      if (length(group) > 1) {
        groups[paste(group, collapse = ":")] <- list(group)
      }
      
      # Filter out variables that are used for grouping
      analysis_vars <- setdiff(all_vars, group)
      
      result_table <- create_multi_group_table(groups, analysis_vars, 
                                               stats_data, total, 
                                               stats_mode = stats_mode, 
                                               stats_override_vars = stats_override_vars
                                               )
      
      # Format the results for presentation
      if (is.list(result_table) && !is.null(result_table$numeric)) {
        #Reshape numeric results for better presentation
        numeric_wide <- result_table$numeric %>%
          select(-sex, -age_group) %>% 
          pivot_longer(
            cols = -matches("_n$") & !starts_with("group"),
            names_to = c("variable", "stat"),
            names_pattern = "(.+)_(.+)$"
          ) %>%
          pivot_wider(
            id_cols = c("variable"),
            names_from = c("stat", "group"),
            values_from = "value"
          )
        
        
        # Handle factor variables separately if they exist
        if (!is.null(result_table$factor)) {
          factor_table <- result_table$factor %>%
            pivot_wider(
              id_cols = c("variable", "value"),
              names_from = "group",
              values_from = c("n", "percent")
            )
          
          result_list <- list(numeric = numeric_wide, factor = factor_table)
        } else {
          result_list <- list(numeric = numeric_wide)
        }
        
        result_table <- result_list
      }
      
    } else {
      # For simple grouping (one grouping variable)
      for (g in group) {
        # Filter out the current grouping variable from analysis variables
        analysis_vars <- setdiff(all_vars, g)
        result_table <- create_table(group = g, 
                                     vars = analysis_vars, 
                                     data = stats_data, 
                                     total = total,
                                     stats_mode = stats_mode,
                                     stats_override_vars = stats_override_vars
                                     )
        
        # Add significance tests if requested
        if (sig_test) {
          test_results <- test_group_differences(stats_data, g, analysis_vars, 
                                                 stats_mode = stats_mode, 
                                                 stats_override_vars = stats_override_vars
                                                 )
          result_table <- list(table = result_table, tests = test_results)
        }
      }
    }
    
    if (format != "raw") {
      if (is.list(result_table) && !is.data.frame(result_table)) {
        # If we have test results, format both
        if (!is.null(result_table$table) && !is.null(result_table$tests)) {
          formatted_table <- print_grouped_table(result_table$table, format = format, 
                                                 caption = paste0(caption, " - Statistics"))
          formatted_tests <- knitr::kable(result_table$tests, format = format, 
                                          caption = paste0(caption, " - Group Differences"))
          return(list(table = formatted_table, tests = formatted_tests))
        } else if (!is.null(result_table$numeric) && !is.null(result_table$factor)) {
          # Handle multi-group results with both numeric and factor data
          formatted_numeric <- knitr::kable(result_table$numeric, format = format, 
                                            caption = paste0(caption, " - Numeric Variables"))
          formatted_factor <- knitr::kable(result_table$factor, format = format, 
                                           caption = paste0(caption, " - Categorical Variables"))
          return(list(numeric = formatted_numeric, factor = formatted_factor))
        } else if (!is.null(result_table$numeric)) {
          # Only numeric data
          return(knitr::kable(result_table$numeric, format = format, 
                              caption = paste0(caption, " - Variables")))
        }
      } else {
        # Just format the table
        return(print_grouped_table(result_table, format = format, caption = caption))
      }
    } else {
      return(result_table)
    }
  }
  
  # Default fallback
  return(stats_data)
}