fancy_shit <- function(dens_scale_factor = 8,
                       dens_col = "royalblue",
                       dens_alpha = 0.7,
                       phq_scale_alpha = 0.8,
                       boxplot_color = "grey10",
                       boxplot_fill = "azure",
                       boxplot_width = 0.5,
                       ci_alpha = 0.4,
                       ci_75_color = "grey80",
                       ci_50_color = "grey60",
                       point_size = 0.5,
                       point_jitter = 0.1,
                       line_color = "black",
                       line_size = 1,
                       cutoff_color = "purple3") {
  
  t0_data <- all %>%
    group_by(ID) %>% 
    dplyr::summarize(PHQ_T0 = first(PHQ_T0)) %>% 
    mutate(position = 0)
    
  t1_data <- all %>% 
    dplyr::group_by(ID) %>% 
    dplyr::summarize(PHQ_T1 = first(PHQ_T1)) %>% 
    mutate(position = 15)
  
  densT0 <- density(t0_data$PHQ_T0,
                    from = min(t0_data$PHQ_T0),
                    to = max(t0_data$PHQ_T0),
                    n = 100)
  densT1 <- density(t1_data$PHQ_T1,
                    from = min(t1_data$PHQ_T1),
                    to = max(t1_data$PHQ_T1),
                    n = 100)
  
  densT0_df <- data.frame(
    x = c(0 - densT0$y * dens_scale_factor, rep(0, length(densT0$y))),
    y = c(densT0$x, rev(densT0$x)))
  densT1_df <- data.frame(
    x = c(15 + densT1$y * dens_scale_factor, rep(15, length(densT1$y))),
    y = c(densT1$x, rev(densT1$x)))
  
  # CIs
  quantile_interval_75 <- function(x) {
    data.frame(
      y = mean(x, na.rm = TRUE),
      ymin = quantile(x, 0.125, na.rm = TRUE),
      ymax = quantile(x, 0.875, na.rm = TRUE)
    )
  }
  
  quantile_interval_50 <- function(x) {
    data.frame(
      y = mean(x, na.rm = TRUE),
      ymin = quantile(x, 0.25, na.rm = TRUE),
      ymax = quantile(x, 0.75, na.rm = TRUE)
    )
  }
  
  # Interpret zone prep
  phq_zones <- data.frame(
    ymin = c(0, 4.5, 9.5, 14.5, 19.5),
    ymax = c(4.5, 9.5, 14.5, 19.5, 27),
    zone = factor(1:5, labels = c("Minimal", "Mild", "Moderate", "Moderat-severe", "Severe"))
  )
  
  zone_colors <- c("Minimal" = "#ffffff", 
                   "Mild" = "#fffdd4",         
                   "Moderate" = "#ffe6c1",     
                   "Moderat-severe" = "#ffd7c1",  
                   "Severe" = "#ffcaca")   
  
  # MAIN PLOT
  p <- ggplot() +
    # background
    geom_rect(data = phq_zones,
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = zone),
              alpha = phq_scale_alpha,
              inherit.aes = FALSE
    ) +
    scale_fill_manual(values = zone_colors,
                      guide = "none"
    ) +
    # Density curves
    geom_polygon(data = densT0_df,
                 aes(x = x, y = y),
                 color = dens_col,
                 fill = dens_col,
                 alpha = dens_alpha,
                 inherit.aes = FALSE
    ) +
    geom_polygon(data = densT1_df,
                 aes(x = x, y = y),
                 color = dens_col,
                 fill = dens_col,
                 alpha = dens_alpha,
                 inherit.aes = FALSE
    ) +
    # Boxplots
    geom_boxplot(data = t0_data,
                 aes(x = position, y = PHQ_T0),
                 width = boxplot_width,
                 #outlier.shape = NA,
                 color = boxplot_color,
                 fill = boxplot_fill
    ) +
    geom_boxplot(data = t1_data,
                 aes(x = position, y = PHQ_T1),
                 width = boxplot_width,
                 fatten = 2,
                 outlier.shape = NA,
                 color = boxplot_color,
                 fill = boxplot_fill
    ) +
    # distribution-zones
    stat_summary(data = all, aes(x = day, y = PHQ),
                 fun.data = quantile_interval_75,
                 geom = "ribbon",
                 fill = ci_75_color,
                 color = ci_75_color,
                 alpha = ci_alpha,
                 na.rm = TRUE) +
    stat_summary(data = all, aes(x = day, y = PHQ),
                 fun.data = quantile_interval_50,
                 geom = "ribbon",
                 fill = ci_50_color,
                 color = ci_50_color,
                 alpha = ci_alpha,
                 na.rm = TRUE) +
    # data points
    geom_point(data = all %>% filter(day >= 1 & day <= 14),
               aes(x = day, y = PHQ),
               size = point_size, position = position_jitter(width = point_jitter),
               na.rm = TRUE) +
    # trajectory
    stat_summary(data = all %>% filter(day >= 1 & day <= 14),
                 aes(x = day, y = PHQ),
                 fun = mean, geom = "line", color = line_color, linewidth = line_size,
                 na.rm = TRUE) +
    # inclusion criteria
    geom_hline(yintercept = 8, linetype = "dashed", color = cutoff_color) +
    # axes
    scale_x_continuous(
      breaks = seq(0, 15, by = 1),
      labels = c("T0", as.character(1:14), "T1"),
      expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 27),
                       breaks = seq(0, 27, by = 3),
                       minor_breaks = seq(0, 27, by = 1),
                       expand = c(0, 0)) +
    coord_cartesian(xlim = c(-1, 16), clip = "off") +
    labs(x = "Day", y = "PHQ Score") +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "grey70"),
          panel.grid.minor = element_line(color = "grey85"),
          axis.ticks.y = element_line(linewidth = 1),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line())
  
  # LEGEND (separate object)
  p_legend <- ggplot() +
    # background zones
    geom_rect(data = phq_zones,
              aes(xmin = 1, xmax = 2, ymin = ymin, ymax = ymax, fill = zone),
              alpha = phq_scale_alpha) +
    scale_fill_manual(values = zone_colors, name = "PHQ-9 Severity") +
    
    # Density curves
    geom_polygon(data = data.frame(
      x = c(3, 4, 4, 3),
      y = c(2, 2, 3, 3)
    ),
    aes(x = x, y = y),
    color = dens_col,
    fill = dens_col,
    alpha = dens_alpha) +
    
    # boxplots
    geom_boxplot(data = data.frame(x = 3.5, y = 6),
                 aes(x = x, y = y),
                 width = boxplot_width,
                 color = boxplot_color,
                 fill = boxplot_fill) +
    
    # 75% distribution
    geom_ribbon(data = data.frame(
      x = c(3, 4),
      ymin = rep(9, 2),
      ymax = rep(11, 2)
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = ci_75_color,
    color = ci_75_color,
    alpha = ci_alpha) +
    
    # 50% distribution
    geom_ribbon(data = data.frame(
      x = c(3, 4),
      ymin = rep(9.5, 2),
      ymax = rep(10.5, 2)
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = ci_50_color,
    color = ci_50_color,
    alpha = ci_alpha) +
    
    # data point
    geom_point(data = data.frame(x = rep(3.5, 5), y = 13 + runif(5, -0.5, 0.5)),
               aes(x = x, y = y),
               size = point_size * 3) +
    
    # average trajectory
    geom_line(data = data.frame(
      x = c(3, 4),
      y = c(10, 10)
    ),
    aes(x = x, y = y),
    color = line_color,
    linewidth = line_size) +
    
    # inclusion criteria
    geom_hline(yintercept = 16, linetype = "dashed", color = cutoff_color) +
    
    # Beschriftungen für die Legende
    annotate("text", x = 5, y = (0 + 4.5)/2, label = "Minimal", hjust = 0) +
    annotate("text", x = 5, y = (4.5 + 9.5)/2, label = "Mild", hjust = 0) +
    annotate("text", x = 5, y = (9.5 + 14.5)/2, label = "Moderate", hjust = 0) +
    annotate("text", x = 5, y = (14.5 + 19.5)/2, label = "Moderat-severe", hjust = 0) +
    annotate("text", x = 5, y = (19.5 + 27)/2, label = "Severe", hjust = 0) +
    
    annotate("text", x = 5, y = 2.5, label = "Density distribution", hjust = 0) +
    annotate("text", x = 5, y = 6, label = "Boxplot", hjust = 0) +
    annotate("text", x = 5, y = 10, label = "Mean PHQ score", hjust = 0) +
    annotate("text", x = 5, y = 16, label = "Cutoff (PHQ ≥ 8)", hjust = 0) +
    annotate("text", x = 5, y = 13, label = "Individual measurements", hjust = 0) +
    annotate("text", x = 5, y = 10, label = "Mean PHQ score", hjust = 0) +
    
    # label for distribution zones
    geom_segment(data = data.frame(x = 5, y = 10, xend = 4.2, yend = 10.75),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), color = "black") +
    annotate("text", x = 7, y = 10, label = "75% & 50% quantile intervals", hjust = 0) +
    
    # layout and labels
    labs(title = "PHQ Score Legend") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    coord_cartesian(xlim = c(1, 10), ylim = c(0, 27))
  
  return(list(p, p_legend))
}