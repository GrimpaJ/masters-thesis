source(file.path(getwd(), "code", "utils.R"))
source_script("data_processing")

create_app_ranking <- function(data = NULL, type, save = FALSE) {
  
  if(is.null(data)) {
    if(!exists("app_use", envir = globalenv())) {
      data <- process_data("app_use")
    } else {
      data <- app_use
    }
  }
  
  if(type == "app") {
    stats <- data %>%
      mutate(time = fg_time_ms / 1000 / 60) %>%
      group_by(app_name) %>%
      summarise(
        users = n_distinct(ID),
        total_usage = sum(time, na.rm = TRUE),
        relative_usage = total_usage / users,
        mean_daily_usage = mean(time[time > 0], na.rm = TRUE),
        .groups = "drop"
      )
    
    apps <- process_data("apps")
    
    ranked <- stats %>%
      mutate(
        rank_users = rank(-users, ties.method = "max"),
        rank_total_usage = rank(-total_usage, ties.method = "max"),
        rank_relative_usage = rank(-relative_usage, ties.method = "max"),
        rank_mean_daily = rank(-mean_daily_usage, ties.method = "max")
      ) %>%
      rowwise() %>%
      mutate(total_score = sum(rank_users, rank_total_usage, 
                               rank_relative_usage, rank_mean_daily)) %>%
      arrange(total_score) %>% 
      left_join(apps, by = c("app_name" = "package_name"))
  }
  
  if(type == "category") {
    stats <- data %>%
      mutate(time = fg_time_ms / 1000 / 60) %>%
      group_by(category) %>%
      summarise(
        users = n_distinct(ID),
        total_usage = sum(time, na.rm = TRUE),
        relative_usage = total_usage / users,
        mean_daily_usage = mean(time[time > 0], na.rm = TRUE),
        .groups = "drop"
      )
    
    ranked <- stats %>%
      mutate(
        rank_users = rank(-users, ties.method = "max"),
        rank_total_usage = rank(-total_usage, ties.method = "max"),
        rank_relative_usage = rank(-relative_usage, ties.method = "max"),
        rank_mean_daily = rank(-mean_daily_usage, ties.method = "max")
      ) %>%
      rowwise() %>%
      mutate(total_score = sum(rank_users, rank_total_usage,
                               rank_relative_usage, rank_mean_daily)) %>%
      arrange(total_score)
  }
  
  if(save == TRUE) {
    save_csv(ranked)
  }
  
  return(ranked)
}