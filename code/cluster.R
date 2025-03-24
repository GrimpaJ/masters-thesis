cluster_fuck <- function(data, method,
                         imputation = FALSE, centering = FALSE,
                         plot = TRUE, k = 5, seed = "123") {
  
  df <- data %>%
    select(ID, day, PHQ) %>% 
    filter(day >= 1, day <= 14)
  
  df_wide <- df %>%
    pivot_wider(names_from = day, values_from = PHQ, names_prefix = "day_") %>%
    arrange(ID)
  
  phq_cols <- grep("^day_", names(df_wide), value = TRUE)
  
  if(imputation == TRUE) {
    df_wide_imputed <- df_wide
    df_wide_imputed[, phq_cols] <- 
      t(apply(as.matrix(df_wide[, phq_cols]), 1,
              function(x) na.approx(x, na.rm = FALSE, rule = 2)
      ))
    df_wide <- df_wide_imputed
  }
  
  df_wide <- df_wide %>% drop_na(all_of(phq_cols))
  
  if(centering == TRUE) {
    df_wide_centered <- df_wide
    df_wide_centered[, phq_cols] <-
      t(apply(as.matrix(df_wide[, phq_cols]), 1,
              function(x) x - x[1]
      ))
    df_wide <- df_wide_centered
  }
  
  # Matrix
  phq_matrix <- as.matrix(df_wide[, phq_cols])
  
  cluster_result <- NULL
  cluster_column <- NULL
  
  if(method == "dtw") {
    # Partitional Clustering with DTW-distance and DBA
    library(dtwclust)
    set.seed(seed)
    cluster_result <- tsclust(phq_matrix,
                              type = "partitional",
                              k = k,
                              distance = "dtw_basic",
                              centroid = "dba",
                              trace = TRUE)
    
    # Append profile assignment to df_wide
    df_wide$cluster_value <- cluster_result@cluster
    cluster_column <- "dtw_cluster"
    names(df_wide)[names(df_wide) == "cluster_value"] <- cluster_column
    print(table(df_wide[[cluster_column]]))
  }
  
  else if(method == "mclust") {
    library(mclust)
    
    set.seed(seed)
    cluster_result <- Mclust(phq_matrix, G = 1:k)
    
    df_wide$cluster_value <- cluster_result$classification
    cluster_column <- "mclust_cluster"
    names(df_wide)[names(df_wide) == "cluster_value"] <- cluster_column
    
    print(table(df_wide[[cluster_column]]))
  }
  else {
    stop("invalid method: use 'dtw' or 'mclust'")
  }
  
  data <- merge(data, df_wide[, c("ID", cluster_column)], by = "ID", all.x = TRUE)
  
  p <- NULL
  if(plot == TRUE) {
    p <- cluster_plot(df_wide, method = cluster_column)
  }
  
  return(list(
    data = data,
    cluster_result = cluster_result,
    plot = p,
    df_wide = df_wide
  ))
}


cluster_plot <- function(data, method, clusters = "all") {
  library(tidyverse)
  
  # Check whether method column exists
  if (!method %in% colnames(data)) {
    stop(paste("Die Spalte", method, "existiert nicht in den Daten"))
  }
  
  # Determine all available clusters
  all_clusters <- sort(unique(data[[method]]))
  
  if (identical(clusters, "all")) {
    clusters_to_show <- all_clusters
  } else {
    # Check whether specified clusters exist
    if (!all(clusters %in% all_clusters)) {
      warning("Einige angegebene Cluster existieren nicht in den Daten.")
      clusters_to_show <- clusters[clusters %in% all_clusters]
    } else {
      clusters_to_show <- clusters
    }
  }
  
  df_long <- data %>%
    pivot_longer(cols = starts_with("day_"),
                 names_to = "day",
                 names_prefix = "day_",
                 values_to = "PHQ") %>%
    mutate(day = as.numeric(day))
  
  # filter data based on selection
  df_long_filtered <- df_long %>%
    filter(!!sym(method) %in% clusters_to_show)
  
  # Calculate average trajectories incl. confidence interval per cluster
  df_avg <- df_long_filtered %>%
    group_by(!!sym(method), day) %>%
    dplyr::summarize(avg_PHQ = mean(PHQ, na.rm = TRUE),
                     sd_PHQ = sd(PHQ, na.rm = TRUE),
                     n = n(),
                     se = sd_PHQ / sqrt(n),
                     ci_lower = avg_PHQ - qt(0.975, df = n - 1) * se,
                     ci_upper = avg_PHQ + qt(0.975, df = n - 1) * se,
                     .groups = "drop")
  
  p <- ggplot() +
    geom_line(data = df_long_filtered,
              aes(x = day, y = PHQ,
                  group = ID,
                  color = factor(!!sym(method))),
              alpha = 0.5) +
    geom_ribbon(data = df_avg,
                aes(x = day,
                    ymin = ci_lower, ymax = ci_upper,
                    fill = factor(!!sym(method))),
                alpha = 0.2) +
    geom_line(data = df_avg,
              aes(x = day, y = avg_PHQ,
                  color = factor(!!sym(method))),
              linewidth = 1) +
    labs(x = "Tag", y = "PHQ",
         color = "Cluster",
         fill = "Cluster") +
    theme_minimal()+
    scale_x_continuous(breaks = 1:14)
  
  print(p)
  return(p)
}