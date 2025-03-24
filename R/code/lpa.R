run_lpa <- function(data = NULL,
                              groups = 1:6,
                              type = "sum",
                              n_bootstrap = 0,
                              confidence_level = 0.95,
                              seed = 123,
                              excluded_ids = FALSE,
                              remove_irrelevant_apps = FALSE,
                              summary = TRUE,
                              verbose = FALSE,
                              parallel = TRUE,
                              cores = NULL) {
  require(mclust)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  result <- list()
  result$call <- match.call()
  
  # data preparation
  if(is.null(data)) {
    if(!exists("app_use", envir = globalenv())) {
      data <- process_data("app_use")
    } else {
      data <- app_use
    }
    
    if (remove_irrelevant_apps == TRUE) {
      data <- data %>% 
        filter(relevance != "no")
    }
    
    if (type == "sum") {
      data <- data %>% 
        group_by(ID, category, day) %>% 
        summarise(time = sum(fg_time_ms, na.rm = TRUE), 
                  .groups = "drop")
    } else if (type == "mean") {
      data <- data %>% 
        group_by(ID, category, day) %>% 
        summarise(time = mean(fg_time_ms, na.rm = TRUE), 
                  .groups = "drop")
    } else {
      stop("Type muss 'sum' oder 'mean' sein")
    }
    
    data <- data %>% 
      group_by(ID, category) %>% 
      summarise(mean_time = mean(time, na.rm = TRUE), 
                .groups = "drop") %>% 
      pivot_wider(names_from = category, 
                  values_from = mean_time, 
                  values_fill = 0)
  }
  
  if (excluded_ids == TRUE) {
    data <- remove_excluded_ids(data, cutoff = 0.5)
  }
  
  result$data <- data
  
  # prepare matrix for LPA
  matrix <- data %>% select(-ID) %>% as.matrix()
  categories <- colnames(matrix)
  n_categories <- length(categories)
  
  # run original LPA
  if(verbose) {
    original_result <- Mclust(matrix, G = groups)
  } else {
    invisible(capture.output(
      original_result <- Mclust(matrix, G = groups)
    ))
  }
  
  # save result
  result$original <- list(
    model = original_result,
    model_type = original_result$modelName,
    n_profiles = original_result$G,
    bic = original_result$bic,
    loglik = original_result$loglik,
    n_parameters = original_result$df
  )
  
  original_sizes <- table(original_result$classification)
  original_props <- prop.table(original_sizes)
  
  result$original$profile_sizes <- as.numeric(original_sizes)
  result$original$profile_proportions <- as.numeric(original_props)
  
  if (is.list(original_result$parameters$mean)) {
    n_profiles <- length(original_result$parameters$pro)
    original_means <- matrix(NA, nrow = n_profiles, ncol = n_categories)
    
    for (i in 1:n_profiles) {
      original_means[i,] <- original_result$parameters$mean[[i]]
    }
  } else {
    original_means <- t(original_result$parameters$mean)
  }
  colnames(original_means) <- categories
  
  result$original$means_matrix <- original_means
  
  original_means_df <- as.data.frame(original_means)
  original_means_df$profile <- 1:nrow(original_means_df)
  
  original_means_long <- original_means_df %>%
    pivot_longer(cols = -profile, 
                 names_to = "category", 
                 values_to = "mean_time")
  
  result$original$means_long <- original_means_long
  
  # profile plor for original lpa
  profile_plot <- ggplot(original_means_long, 
                         aes(x = category, y = mean_time, fill = factor(profile))) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "App-Nutzung nach Profil",
         x = "Kategorie", y = "Nutzungszeit",
         fill = "Profile")
  
  result$original$profile_plot <- profile_plot
  
  # add profile classification to original data
  result$data$profile <- original_result$classification
  
  # Bootstrap-analysis if n_bootstrap > 0
  if(n_bootstrap > 0) {
    result$bootstrap <- list(
      n_bootstrap = n_bootstrap,
      confidence_level = confidence_level
    )
    
    n_profiles_original <- original_result$G
    bootstrap_means <- array(NA, 
                             dim = c(n_bootstrap, n_profiles_original, n_categories),
                             dimnames = list(NULL, 
                                             paste0("profile", 1:n_profiles_original), 
                                             categories))
    
    bootstrap_props <- matrix(NA, 
                              nrow = n_bootstrap, 
                              ncol = n_profiles_original,
                              dimnames = list(NULL, 
                                              paste0("profile", 1:n_profiles_original)))
    
    model_types <- character(n_bootstrap)
    n_profiles <- integer(n_bootstrap)
    bic_values <- numeric(n_bootstrap)
    success_indicator <- logical(n_bootstrap)
    
    if(parallel) {
      require(foreach)
      require(doParallel)
      if(is.null(cores)) {
        cores <- min(parallel::detectCores() - 1, 4)
      }
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      
      if(verbose) {
        cat("Bootstrap-Analyse mit", cores, "Kernen gestartet...\n")
      }
      
      bootstrap_results <- foreach(i = 1:n_bootstrap, 
                                   .packages = c("mclust", "dplyr"),
                                   .errorhandling = "pass") %dopar% {
                                     # create boot sample
                                     sample_indices <- sample(1:nrow(matrix), nrow(matrix), replace = TRUE)
                                     boot_matrix <- matrix[sample_indices, ]
                                     
                                     boot_result <- NULL
                                     output <- capture.output({
                                       boot_result <- tryCatch({
                                         Mclust(boot_matrix, G = groups)
                                       }, error = function(e) {
                                         return(NULL)
                                       })
                                     })
                                     
                                     if(is.null(boot_result)) {
                                       return(list(success = FALSE))
                                     }
                                     
                                     # extract bootstrap results
                                     boot_model_type <- boot_result$modelName
                                     boot_n_profiles <- boot_result$G
                                     boot_bic <- boot_result$bic
                                     
                                     if(boot_n_profiles == n_profiles_original) {
                                       if (is.list(boot_result$parameters$mean)) {
                                         boot_means <- matrix(NA, nrow = boot_n_profiles, ncol = n_categories)
                                         for (j in 1:boot_n_profiles) {
                                           boot_means[j,] <- boot_result$parameters$mean[[j]]
                                         }
                                       } else {
                                         boot_means <- t(boot_result$parameters$mean)
                                       }
                                       colnames(boot_means) <- categories
                                       
                                       profile_mapping <- rep(NA, n_profiles_original)
                                       
                                       for(orig_idx in 1:n_profiles_original) {
                                         distances <- apply(boot_means, 1, function(boot_profile) {
                                           sqrt(sum((boot_profile - original_means[orig_idx,])^2))
                                         })
                                         
                                         available_indices <- which(!(1:boot_n_profiles %in% profile_mapping))
                                         if(length(available_indices) > 0) {
                                           available_distances <- distances[available_indices]
                                           best_match <- available_indices[which.min(available_distances)]
                                           profile_mapping[orig_idx] <- best_match
                                         }
                                       }
                                       
                                       means_matched <- matrix(NA, nrow = n_profiles_original, ncol = n_categories)
                                       colnames(means_matched) <- categories
                                       
                                       for(j in 1:n_profiles_original) {
                                         if(!is.na(profile_mapping[j])) {
                                           means_matched[j,] <- boot_means[profile_mapping[j],]
                                         }
                                       }
                                       
                                       boot_sizes <- table(boot_result$classification)
                                       boot_props <- prop.table(boot_sizes)
                                       props_matched <- rep(NA, n_profiles_original)
                                       
                                       for(j in 1:n_profiles_original) {
                                         if(!is.na(profile_mapping[j]) && profile_mapping[j] <= length(boot_props)) {
                                           props_matched[j] <- boot_props[profile_mapping[j]]
                                         }
                                       }
                                       
                                       return(list(
                                         success = TRUE,
                                         model_type = boot_model_type,
                                         n_profiles = boot_n_profiles,
                                         bic = boot_bic,
                                         means = means_matched,
                                         proportions = props_matched
                                       ))
                                     } else {
                                       return(list(
                                         success = TRUE,
                                         model_type = boot_model_type,
                                         n_profiles = boot_n_profiles,
                                         bic = boot_bic,
                                         means = NULL,
                                         proportions = NULL
                                       ))
                                     }
                                   }
      
      stopCluster(cl)
      
      # process results
      for(i in 1:n_bootstrap) {
        if(is.list(bootstrap_results[[i]]) && !is.null(bootstrap_results[[i]]$success)) {
          success_indicator[i] <- bootstrap_results[[i]]$success
          
          if(bootstrap_results[[i]]$success) {
            model_types[i] <- bootstrap_results[[i]]$model_type
            n_profiles[i] <- bootstrap_results[[i]]$n_profiles
            bic_values[i] <- bootstrap_results[[i]]$bic
            
            if(!is.null(bootstrap_results[[i]]$means)) {
              bootstrap_means[i,,] <- bootstrap_results[[i]]$means
            }
            
            if(!is.null(bootstrap_results[[i]]$proportions)) {
              bootstrap_props[i,] <- bootstrap_results[[i]]$proportions
            }
          }
        }
      }
      
    } else {
      # Non-parallel bootstrap execution
      if(verbose) {
        pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
      }
      
      for(i in 1:n_bootstrap) {
        sample_indices <- sample(1:nrow(matrix), nrow(matrix), replace = TRUE)
        boot_matrix <- matrix[sample_indices, ]
        
        boot_result <- NULL
        if(!verbose) {
          output <- capture.output({
            boot_result <- tryCatch({
              Mclust(boot_matrix, G = groups)
            }, error = function(e) {
              return(NULL)
            })
          })
        } else {
          boot_result <- tryCatch({
            Mclust(boot_matrix, G = groups)
          }, error = function(e) {
            return(NULL)
          })
        }
        
        if(!is.null(boot_result)) {
          success_indicator[i] <- TRUE
          model_types[i] <- boot_result$modelName
          n_profiles[i] <- boot_result$G
          bic_values[i] <- boot_result$bic
          
          if(boot_result$G == n_profiles_original) {
            if (is.list(boot_result$parameters$mean)) {
              boot_means <- matrix(NA, nrow = boot_result$G, ncol = n_categories)
              for (j in 1:boot_result$G) {
                boot_means[j,] <- boot_result$parameters$mean[[j]]
              }
            } else {
              boot_means <- t(boot_result$parameters$mean)
            }
            colnames(boot_means) <- categories
            
            profile_mapping <- rep(NA, n_profiles_original)
            
            for(orig_idx in 1:n_profiles_original) {
              distances <- apply(boot_means, 1, function(boot_profile) {
                sqrt(sum((boot_profile - original_means[orig_idx,])^2))
              })
              
              available_indices <- which(!(1:boot_result$G %in% profile_mapping))
              if(length(available_indices) > 0) {
                available_distances <- distances[available_indices]
                best_match <- available_indices[which.min(available_distances)]
                profile_mapping[orig_idx] <- best_match
              }
            }
            
            for(j in 1:n_profiles_original) {
              if(!is.na(profile_mapping[j])) {
                bootstrap_means[i, j, ] <- boot_means[profile_mapping[j], ]
              }
            }
            
            boot_props <- prop.table(table(boot_result$classification))
            for(j in 1:n_profiles_original) {
              if(!is.na(profile_mapping[j]) && profile_mapping[j] <= length(boot_props)) {
                bootstrap_props[i, j] <- boot_props[profile_mapping[j]]
              }
            }
          }
        } else {
          success_indicator[i] <- FALSE
        }
        
        if(verbose) {
          setTxtProgressBar(pb, i)
        }
      }
      
      if(verbose) {
        close(pb)
      }
    }
    
    n_success <- sum(success_indicator)
    result$bootstrap$n_success <- n_success
    result$bootstrap$success_rate <- n_success / n_bootstrap
    
    if(n_success > 0) {
      result$bootstrap$raw <- list(
        model_types = model_types[success_indicator],
        n_profiles = n_profiles[success_indicator],
        bic_values = bic_values[success_indicator],
        means = bootstrap_means[success_indicator,,],
        proportions = bootstrap_props[success_indicator,]
      )
      
      model_counts <- table(model_types[success_indicator])
      model_props <- prop.table(model_counts) * 100
      result$bootstrap$model_distribution <- data.frame(
        model = names(model_counts),
        frequency = as.numeric(model_counts),
        percentage = round(as.numeric(model_props), 1)
      ) %>% arrange(desc(frequency))
      
      profile_counts <- table(n_profiles[success_indicator])
      profile_props <- prop.table(profile_counts) * 100
      result$bootstrap$profile_distribution <- data.frame(
        n_profiles = as.numeric(names(profile_counts)),
        frequency = as.numeric(profile_counts),
        percentage = round(as.numeric(profile_props), 1)
      ) %>% arrange(desc(frequency))
      
      model_match <- sum(model_types[success_indicator] == original_result$modelName & 
                           n_profiles[success_indicator] == original_result$G, 
                         na.rm = TRUE)
      result$bootstrap$stability_index <- model_match / n_success
      
      means_summary <- data.frame()
      
      for(j in 1:n_profiles_original) {
        for(k in 1:n_categories) {
          values <- bootstrap_means[success_indicator, j, k]
          values <- values[!is.na(values)]
          
          if(length(values) >= 5) {
            mean_val <- mean(values, na.rm = TRUE)
            sd_val <- sd(values, na.rm = TRUE)
            ci_lower <- quantile(values, (1 - confidence_level) / 2, na.rm = TRUE)
            ci_upper <- quantile(values, 1 - (1 - confidence_level) / 2, na.rm = TRUE)
            
            means_summary <- rbind(means_summary, data.frame(
              category = categories[k],
              profile = j,
              original_mean = original_means[j, k],
              bootstrap_mean = mean_val,
              sd = sd_val,
              ci_lower = ci_lower,
              ci_upper = ci_upper,
              n_samples = length(values),
              relative_error = abs(original_means[j, k] - mean_val) / max(0.0001, original_means[j, k]) * 100
            ))
          }
        }
      }
      
      if(nrow(means_summary) > 0) {
        means_summary <- means_summary %>% 
          mutate(
            stability = case_when(
              relative_error <= 10 ~ "Hoch",
              relative_error <= 25 ~ "Mittel",
              TRUE ~ "Niedrig"
            )
          ) %>%
          arrange(profile, category)
        
        result$bootstrap$means_summary <- means_summary
        
        # Plot of CIs
        ci_plot <- ggplot(means_summary, 
                          aes(x = category, y = bootstrap_mean, color = factor(profile))) +
          geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                          position = position_dodge(width = 0.5)) +
          geom_point(aes(y = original_mean), shape = 4, size = 3, stroke = 1.5,
                     position = position_dodge(width = 0.5)) +
          labs(title = "Bootstrap-CI für Profile-Mittelwerte",
               subtitle = "Kreuze zeigen Originalwerte",
               x = "Kategorie", y = "Mittlere Nutzungszeit", color = "Profile") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        result$bootstrap$ci_plot <- ci_plot
      }
      
      props_summary <- data.frame()
      
      for(j in 1:n_profiles_original) {
        values <- bootstrap_props[success_indicator, j]
        values <- values[!is.na(values)]
        
        if(length(values) >= 5) {
          mean_val <- mean(values, na.rm = TRUE)
          sd_val <- sd(values, na.rm = TRUE)
          ci_lower <- quantile(values, (1 - confidence_level) / 2, na.rm = TRUE)
          ci_upper <- quantile(values, 1 - (1 - confidence_level) / 2, na.rm = TRUE)
          
          props_summary <- rbind(props_summary, data.frame(
            profile = j,
            original_prop = as.numeric(original_props[j]),
            bootstrap_prop = mean_val,
            sd = sd_val,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            n_samples = length(values),
            relative_error = abs(as.numeric(original_props[j]) - mean_val) / max(0.0001, as.numeric(original_props[j])) * 100
          ))
        }
      }
      
      if(nrow(props_summary) > 0) {
        props_summary <- props_summary %>% 
          mutate(
            stability = case_when(
              relative_error <= 10 ~ "Hoch",
              relative_error <= 25 ~ "Mittel",
              TRUE ~ "Niedrig"
            )
          ) %>%
          arrange(profile)
        
        result$bootstrap$proportions_summary <- props_summary
        
        props_plot <- ggplot(props_summary, aes(x = factor(profile), y = bootstrap_prop)) +
          geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                          color = "steelblue") +
          geom_point(aes(y = original_prop), color = "red", shape = 4, size = 3, stroke = 1.5) +
          labs(title = "Bootstrap-CI für Profil-Anteile",
               subtitle = "Rote Kreuze zeigen Originalwerte",
               x = "Profile", y = "Anteil") +
          theme_minimal() +
          ylim(0, max(c(props_summary$ci_upper, props_summary$original_prop), na.rm = TRUE) * 1.1)
        
        result$bootstrap$props_plot <- props_plot
      }
    }
  }
  
  class(result) <- "run_lpa"
  
  if(summary) {
    create_lpa_report(result)
  }
  
  return(result)
}

# function to print summary
print_lpa_summary <- function(result, verbose = FALSE) {
  if(!inherits(result, "run_lpa")) {
    stop("Das Objekt ist kein 'run_lpa' Ergebnis")
  }
  
  cat("\n================ LPA-ERGEBNISSE ================\n")
  cat("\nModell:", result$original$model_type)
  cat("\nAnzahl Profile:", result$original$n_profiles)
  cat("\nBIC:", round(result$original$bic, 2))
  cat("\nLoglikelihood:", round(result$original$loglik, 2))
  cat("\nParameter:", result$original$n_parameters)
  
  # profile sizes
  cat("\n\nProfilgrößen:\n")
  profile_sizes_df <- data.frame(
    Profile = 1:length(result$original$profile_sizes),
    N = result$original$profile_sizes,
    Prozent = round(result$original$profile_proportions * 100, 1)
  )
  print(profile_sizes_df)
  
  # profile means
  if(verbose) {
    cat("\nProfilmittelwerte:\n")
    category_means <- result$original$means_long %>%
      pivot_wider(names_from = profile, 
                  values_from = mean_time, 
                  names_prefix = "Profil ")
    print(category_means)
  }
  
  # profile plot
  print(result$original$profile_plot)
  

  if(!is.null(result$bootstrap)) {
    cat("\n\n======== BOOTSTRAP-STABILITÄTSANALYSE ========\n")
    cat("\nBootstrap-Iterationen:", result$bootstrap$n_bootstrap)
    cat("\nErfolgreiche Durchläufe:", result$bootstrap$n_success, "(", 
        round(result$bootstrap$success_rate * 100, 1), "%)", sep="")
    
    if(result$bootstrap$n_success > 0) {
      # model distribution
      if(!is.null(result$bootstrap$model_distribution)) {
        cat("\n\nVerteilung der Modelltypen:\n")
        print(result$bootstrap$model_distribution)
        
        # model stability
        most_frequent_model <- result$bootstrap$model_distribution$model[1]
        most_frequent_model_perc <- result$bootstrap$model_distribution$percentage[1]
        cat("\nHäufigstes Modell:", most_frequent_model, "(", most_frequent_model_perc, "%)", sep="")
        
        original_model_match <- result$bootstrap$model_distribution %>%
          filter(model == result$original$model_type)
        
        if(nrow(original_model_match) > 0) {
          cat("\nÜbereinstimmung mit Original-Modelltyp:", original_model_match$percentage, "%", sep="")
        } else {
          cat("\nDas Originalmodell", result$original$model_type, "kam in keiner Bootstrap-Iteration vor.")
        }
      }
      
      # profile distribution
      if(!is.null(result$bootstrap$profile_distribution)) {
        cat("\n\nVerteilung der Profilanzahl:\n")
        print(result$bootstrap$profile_distribution)
        
        # profile stability
        most_frequent_profiles <- result$bootstrap$profile_distribution$n_profiles[1]
        most_frequent_profiles_perc <- result$bootstrap$profile_distribution$percentage[1]
        cat("\nHäufigste Profilanzahl:", most_frequent_profiles, "(", most_frequent_profiles_perc, "%)", sep="")
        
        original_profiles_match <- result$bootstrap$profile_distribution %>%
          filter(n_profiles == result$original$n_profiles)
        
        if(nrow(original_profiles_match) > 0) {
          cat("\nÜbereinstimmung mit Original-Profilanzahl:", original_profiles_match$percentage, "%", sep="")
        } else {
          cat("\nDie originale Profilanzahl", result$original$n_profiles, "kam in keiner Bootstrap-Iteration vor.")
        }
      }
      
      # stability index
      cat("\n\nStabilitätsindex (Modell & Profilanzahl identisch):", 
          round(result$bootstrap$stability_index * 100, 1), "%", sep="")
      
      if(!is.null(result$bootstrap$means_summary)) {
        cat("\n\nStabilität der Profilmittelwerte:\n")
        print(result$bootstrap$means_summary %>% 
                select(category, profile, original_mean, bootstrap_mean, 
                       ci_lower, ci_upper, stability))
        
        if(!is.null(result$bootstrap$ci_plot)) {
          print(result$bootstrap$ci_plot)
        }
      }
      
      if(!is.null(result$bootstrap$proportions_summary)) {
        cat("\n\nStabilität der Profilanteile:\n")
        print(result$bootstrap$proportions_summary %>% 
                select(profile, original_prop, bootstrap_prop, 
                       ci_lower, ci_upper, stability))
        
        if(!is.null(result$bootstrap$props_plot)) {
          print(result$bootstrap$props_plot)
        }
      }
    }
  }
}

extract_publication_results <- function(result) {
  if(!inherits(result, "run_lpa")) {
    stop("Das Objekt ist kein 'run_lpa' Ergebnis")
  }
  
  cat("======== STATISTISCHE KENNWERTE FÜR PUBLIKATION ========\n\n")
  
  cat("LPA-Modell:\n")
  cat("- Modell:", result$original$model_type, "\n")
  cat("- Anzahl Profile:", result$original$n_profiles, "\n")
  cat("- BIC:", round(result$original$bic, 2), "\n")
  cat("- Loglikelihood:", round(result$original$loglik, 2), "\n\n")
  
  cat("Profilgrößen:\n")
  for(i in 1:length(result$original$profile_sizes)) {
    cat("- Profil", i, ":", result$original$profile_sizes[i], "Teilnehmer (",
        round(result$original$profile_proportions[i] * 100, 1), "%)\n", sep="")
  }
  cat("\n")
  
  if(!is.null(result$bootstrap)) {
    cat("Bootstrap-Stabilitätskennwerte:\n")
    cat("- Erfolgreiche Bootstrap-Samples:", result$bootstrap$n_success, "/", 
        result$bootstrap$n_bootstrap, " (", 
        round(result$bootstrap$success_rate * 100, 1), "%)\n", sep="")
    cat("- Stabilität des Modells:", round(result$bootstrap$stability_index * 100, 1), "%\n\n")
    
    if(!is.null(result$bootstrap$proportions_summary)) {
      cat("Profilgrößen mit Bootstrap-CI:\n")
      props <- result$bootstrap$proportions_summary
      for(i in 1:nrow(props)) {
        cat("- Profil", props$profile[i], ": ", 
            round(props$original_prop[i] * 100, 1), "% [", 
            round(props$ci_lower[i] * 100, 1), "% - ", 
            round(props$ci_upper[i] * 100, 1), "%], ",
            props$stability[i], "\n", sep="")
      }
    }
  }
  
  return(list(
    profile_plot = result$original$profile_plot,
    ci_plot = if(!is.null(result$bootstrap)) result$bootstrap$ci_plot else NULL,
    props_plot = if(!is.null(result$bootstrap)) result$bootstrap$props_plot else NULL
  ))
}

create_lpa_report <- function(lpa_result, output_file = "lpa_report.html") {
  require(htmltools)
  require(ggplot2)
  
  # Convert a dataframe to HTML table
  df_to_html <- function(df) {
    header <- paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row) {
      paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
    })
    paste0("<table class='table'>", header, paste(rows, collapse = ""), "</table>")
  }
  
  # Save plot as base64 encoded image
  plot_to_base64 <- function(plot) {
    temp_file <- tempfile(fileext = ".png")
    png(temp_file, width = 800, height = 600)
    print(plot)
    dev.off()
    
    # Read the file and encode as base64
    img_data <- base64enc::dataURI(file = temp_file, mime = "image/png")
    paste0("<img src='", img_data, "' style='max-width: 100%;'>")
  }
  
  # Start building HTML content
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>LPA Analysis Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; max-width: 1000px; margin: 0 auto; padding: 20px; }",
    "h1, h2, h3 { color: #2c3e50; }",
    ".table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }",
    ".table th, .table td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }",
    ".table th { background-color: #f2f2f2; }",
    ".container { margin-bottom: 30px; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>LPA Analysis Report</h1>"
  )
  
  # LPA Results section
  html_content <- c(html_content,
                    "<div class='container'>",
                    "<h2>LPA Results</h2>",
                    paste0("<p><strong>Model:</strong> ", lpa_result$original$model_type, "</p>"),
                    paste0("<p><strong>Number of profiles:</strong> ", lpa_result$original$n_profiles, "</p>"),
                    paste0("<p><strong>BIC:</strong> ", round(lpa_result$original$bic, 2), "</p>"),
                    paste0("<p><strong>Loglikelihood:</strong> ", round(lpa_result$original$loglik, 2), "</p>"),
                    paste0("<p><strong>Parameters:</strong> ", lpa_result$original$n_parameters, "</p>"),
                    
                    "<h3>Profile Sizes</h3>",
                    df_to_html(data.frame(
                      Profile = 1:length(lpa_result$original$profile_sizes),
                      N = lpa_result$original$profile_sizes,
                      Prozent = paste0(round(lpa_result$original$profile_proportions * 100, 1), "%")
                    )),
                    
                    "<h3>Profile Plot</h3>",
                    plot_to_base64(lpa_result$original$profile_plot),
                    "</div>"
  )
  
  # Bootstrap section if available
  if (!is.null(lpa_result$bootstrap)) {
    html_content <- c(html_content,
                      "<div class='container'>",
                      "<h2>Bootstrap Stability Analysis</h2>",
                      paste0("<p><strong>Bootstrap iterations:</strong> ", lpa_result$bootstrap$n_bootstrap, "</p>"),
                      paste0("<p><strong>Successful runs:</strong> ", lpa_result$bootstrap$n_success, 
                             " (", round(lpa_result$bootstrap$success_rate * 100, 1), "%)</p>")
    )
    
    if (lpa_result$bootstrap$n_success > 0) {
      # Model distribution
      if (!is.null(lpa_result$bootstrap$model_distribution)) {
        html_content <- c(html_content,
                          "<h3>Model Type Distribution</h3>",
                          df_to_html(lpa_result$bootstrap$model_distribution),
                          paste0("<p><strong>Most frequent model:</strong> ", 
                                 lpa_result$bootstrap$model_distribution$model[1], 
                                 " (", lpa_result$bootstrap$model_distribution$percentage[1], "%)</p>")
        )
      }
      
      # Profile count distribution
      if (!is.null(lpa_result$bootstrap$profile_distribution)) {
        html_content <- c(html_content,
                          "<h3>Profile Count Distribution</h3>",
                          df_to_html(lpa_result$bootstrap$profile_distribution)
        )
      }
      
      # Stability index
      html_content <- c(html_content,
                        paste0("<p><strong>Stability index:</strong> ", 
                               round(lpa_result$bootstrap$stability_index * 100, 1), "%</p>")
      )
      
      # Means confidence intervals
      if (!is.null(lpa_result$bootstrap$means_summary)) {
        mean_summary_df <- lpa_result$bootstrap$means_summary %>% 
          select(category, profile, original_mean, bootstrap_mean, ci_lower, ci_upper, stability)
        
        html_content <- c(html_content,
                          "<h3>Profile Means Stability</h3>",
                          df_to_html(mean_summary_df),
                          "<h4>Confidence Intervals Plot</h4>",
                          plot_to_base64(lpa_result$bootstrap$ci_plot)
        )
      }
      
      # Proportions confidence intervals
      if (!is.null(lpa_result$bootstrap$proportions_summary)) {
        props_summary_df <- lpa_result$bootstrap$proportions_summary %>% 
          select(profile, original_prop, bootstrap_prop, ci_lower, ci_upper, stability)
        
        html_content <- c(html_content,
                          "<h3>Profile Proportions Stability</h3>",
                          df_to_html(props_summary_df),
                          "<h4>Proportions Confidence Intervals Plot</h4>",
                          plot_to_base64(lpa_result$bootstrap$props_plot)
        )
      }
    }
    
    html_content <- c(html_content, "</div>")
  }
  
  # Close HTML document
  html_content <- c(html_content,
                    "</body>",
                    "</html>"
  )
  
  # Write to file
  writeLines(html_content, output_file)
  
  # Open the file in browser
  if (interactive()) {
    utils::browseURL(output_file)
  }
  
  cat("HTML report generated at:", output_file, "\n")
}

add_lpa_groups <- function(data, lpa_result = NULL, replace = FALSE, var_name = "profile") {
  if(!"ID" %in% names(data)) {
    stop("Der Datensatz muss eine Spalte namens 'ID' enthalten.")
  }
  
  if(var_name %in% names(data) && !replace) {
    cat(paste0("Spalte '", var_name, "' existiert bereits. Keine Änderungen vorgenommen.\n"))
    return(data)
  }
  
  if (is.null(lpa_result)) {
    if (exists("lpa_result", envir = globalenv())) {
      global_lpa_result <- get("lpa_result", envir = globalenv())
      if (inherits(global_lpa_result, "run_lpa")) {
        lpa_result <- global_lpa_result
        cat("Existierendes lpa_result wird verwendet.\n")
      } else {
        cat("'lpa_result' ist kein gültiges LPA-Ergebnis.\n")
        lpa_result <- NULL
      }
    }
    
    if (is.null(lpa_result)) {
      cat("Führe neue LPA-Analyse durch...\n")
      lpa_result <- run_lpa(
        groups = 1:6,
        type = "sum", 
        excluded_ids = TRUE, 
        remove_irrelevant_apps = FALSE,
        n_bootstrap = 0,
        summary = FALSE,
        verbose = FALSE)
      cat("\nLPA wurde mit folgenden Parametern durchgeführt:")
      cat("\ngroups =", paste(lpa_result$call$groups, collapse = ", "))
      cat("\ntype =", lpa_result$call$type)
      cat("\nexcluded_ids =", lpa_result$call$excluded_ids)
      cat("\nremove_irrelevant_apps =", lpa_result$call$remove_irrelevant_apps)
      cat("\nbootstrapping = disabled")
      cat("\nsummary =", lpa_result$call$summary)
      cat("\n")
    }
  }
  
  profiles_df <- data.frame(
    ID = lpa_result$data$ID,
    profile = lpa_result$data$profile
  )
  
  if (var_name != "profile") {
    names(profiles_df)[names(profiles_df) == "profile"] <- var_name
  }
  
  # Führe die Datensätze zusammen
  if(var_name %in% names(data) && replace) {
    data <- data[, !names(data) %in% var_name, drop = FALSE]
  }
  
  result <- merge(data, profiles_df, by = "ID", all.x = TRUE)
  
  return(result)
}
