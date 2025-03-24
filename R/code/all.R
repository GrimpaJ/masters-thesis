source(file.path(getwd(), "code", "utils.R"))
source_script("data_processing")

create_all_df <- function(lpa = TRUE) {
  codebook <- process_data("codebook")
  ema <- process_data("ema")
  screenstate <- process_data("screenstate")
  app_use <- process_data("app_use")
  ue <- process_data("ue")
  modules <- process_data("modules")
  sosci <- process_data("sosci")
  
  all <- crossing(ID = as.factor(1:40), day = 1:14) %>% 
    left_join(codebook %>% select(ID, sex, age), by = "ID") %>% 
    left_join(ema, by = c("ID", "day")) %>% 
    left_join(screenstate %>% select(-time_total), by = c("ID", "day")) %>% 
    left_join(app_use %>% 
                group_by(ID, category, day) %>% 
                summarise(time = sum(fg_time_ms, na.rm = TRUE), 
                          .groups = "drop") %>%
                pivot_wider(names_from = category, 
                            values_from = time, 
                            values_fill = 0),
              by = c("ID", "day")
    ) %>% 
    left_join(sosci %>% select(ID, Time, PHQ) %>% 
                pivot_wider(names_from = Time,
                            values_from = PHQ,
                            names_prefix = "PHQ_"),
              by = "ID") %>%  
    left_join(ue %>% select(ID, UEQ, SUTAQ), by = "ID") %>%
    left_join(modules %>% select(ID, H_total), by = "ID") %>% 
    mutate(weekday = rename_weekday(ID, day)) %>% 
    rename(screentime = time_on)
  
  if(lpa == TRUE) {
    all <- add_lpa_groups(all, replace = TRUE, var_name = "lpa_group")
  }
  
  selection <- app_use %>%
    filter(app_name == "health.mentalis.android.phoenix") %>%
    select(ID, day, fg_time_ms)
  
  all <- all %>%
    left_join(selection, by = c("ID", "day")) %>%
    rename(phoenix = fg_time_ms)
  
  
  variable_list <- c("PHQ", "screentime", "creativity", "education", 
                     "entertainment", "everyday", "games", "health", 
                     "news", "shopping", "social", "system", "tools", 
                     "phoenix")
    
    all <- all %>% 
      arrange(ID, day) %>% 
      group_by(ID)
    
    for (var in variable_list) {
      lag_name <- paste0(var, "_lag")
      change_name <- paste0(var, "_change")
      
      all <- all %>%
        mutate(
          !!lag_name := dplyr::lag(!!sym(var), order_by = day),
          !!change_name := !!sym(var) - dplyr::lag(!!sym(var), order_by = day)
        )
    }
    
    all <- all %>%
      mutate(
        PHQ_RMSSD = sqrt(mean((PHQ_change)^2, na.rm = TRUE)),
        screentime_RMSSD = sqrt(mean((screentime_change)^2, na.rm = TRUE)),
        day = as.numeric(day)
        ) %>% 
      ungroup()
    

  return(all)
}


