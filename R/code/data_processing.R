Sys.setenv(TZ = "UTC")

process_data <- function(file_name) {
  source(file.path(getwd(), "code", "data_import.R"))
  
  data <- import_data(file_name)
  if (file_name == "ema") {
    relevant_vars <- c("ID",
                       "record_time",
                       paste0("PHQ_", 1:9))
    
    data <- data %>%
      select(all_of(relevant_vars)) %>% 
      mutate(across(c(PHQ_1:PHQ_9), ~ .x - 1))
    
    data <- add_days(data) %>% 
      select(-record_time)
    
    # Score
    data$PHQ <- apply(data[, c(paste0("PHQ_", 1:9))], 1, function(x) {
      if (all(!is.na(x))) {
        sum(x)
      } else {NA}
    })
    
   data <- create_ID_day_layout(data) %>%
     mutate(across(c(PHQ, PHQ_1:PHQ_9), as.integer))
   
   data <- data %>% 
     arrange(ID, day) %>% 
     group_by(ID) %>% 
     mutate(PHQ_change = PHQ - dplyr::lag(PHQ),
            RMSSD = sqrt(mean((PHQ - lag(PHQ))^2, na.rm = TRUE))
            ) %>% 
     ungroup()
    
    return(data)
  }
  if (file_name == "screenstate") {
    data <- replace_ids(data) %>%
      select(-record_time, -study_id, -device_id, -timestamp, -date) %>% 
      rename(start = end_time) %>%
      arrange(ID, start) %>%
      mutate(
        end = if_else(ID == dplyr::lead(ID), dplyr::lead(start), NA_POSIXct_),
        duration = as.numeric(abs(difftime(start, end, units = "hours"))),
        split = duration > 8
      ) %>%
      replace_na(list(split = FALSE)) %>%
      uncount(ifelse(split, 2, 1), .id = "part") %>%
      mutate(
        end = case_when(
          split & part == 1 ~ start + hours(8),
          TRUE ~ end
        ),
        start = case_when(
          split & part == 2 ~ start + hours(8),
          TRUE ~ start
        ),
        state = if_else(split & part == 2, "False", state)
      ) %>%
      select(-duration, -split, -part) %>%
      arrange(ID, start)
    
    temp_df <- data %>%
      mutate(boundary = floor_date(start, unit = "day") + hours(19) + minutes(45)) %>%
      mutate(boundary = if_else(boundary <= start, boundary + days(1), boundary)) %>%
      filter(boundary < end)
    
    temp_df <- temp_df %>%
      mutate(
        x = ifelse(format(end, "%H:%M:%S") < "19:45:00", 0, 1),
        y = ifelse(format(start, "%H:%M:%S") > "19:45:00", 0, 1),
        z = as.integer(as.Date(end) - as.Date(start)),
        z = ifelse(z > 0, z, 0),
        required_rows = x + y + z,
        ident = paste(ID,start, sep="_"))
    
    expanded_df <- temp_df %>%
      rowwise() %>%
      do(data.frame(
        ident = rep(.$ident, .$required_rows),
        ID = rep(.$ID, .$required_rows),
        start = rep(.$start, .$required_rows),
        state = rep(.$state, .$required_rows),
        end = rep(.$end, .$required_rows),
        row_number = seq_len(.$required_rows)
      )) %>%
      ungroup()
    
    expanded_df <- expanded_df %>%
      mutate(
        start_old = start,
        end_old = end)
    
    calculate_end <- function(start_old, end_old, row_number) {
      base_time <- floor_date(start_old, "day") + hours(19) + minutes(45)
      start_diff <- as.numeric(difftime(start_old, base_time, units = "secs"))
      
      case_when(
        row_number == 1 ~ if_else(start_diff > 0, base_time + days(1), base_time),
        row_number == n() ~ end_old,
        TRUE ~ if_else(start_diff > 0, base_time + days(row_number), base_time + days(row_number - 1))
      )
    }
    
    calculate_start <- function(start_old, end_old, row_number, end) {
      base_time <- floor_date(end_old, "day") + hours(19) + minutes(45)
      base_time_start <- floor_date(start_old, "day") + hours(19) + minutes(45)
      start_diff <- as.numeric(difftime(start_old, base_time_start, units = "secs"))
      end_diff <- as.numeric(difftime(end_old, base_time, units = "secs"))
      
      case_when(
        row_number == 1 ~ start_old,
        row_number == n() ~ if_else(end_diff >= 0, base_time, floor_date(end, "day") - days(1) + hours(19) + minutes(45)),
        TRUE ~ if_else(start_diff > 0, base_time_start + days(row_number - 1), base_time_start + days(row_number - 2))
      )
    }
    
    expanded_df <- expanded_df %>%
      arrange(ID, start_old, row_number) %>% 
      group_by(ident) %>%
      mutate(
        end = calculate_end(start_old, end_old, row_number),
        start = calculate_start(start_old, end_old, row_number, end)
      ) %>%
      ungroup()
    
    data <- data %>%
      anti_join(temp_df, by = c("ID", "start", "end", "state"))
    
    data <- bind_rows(data,
                      expanded_df %>%
                        select(-ident, -row_number,
                               -start_old, -end_old))
    
    data <- data[order(data$ID, data$start), ]
    
    data <- data[!(
      is.na(as.character(data$end)) |
        as.character(data$end) == ""), ]
    
    data <- add_days(data, time_var = start) %>% 
      filter(day >= 1 & day <= 14)
    
    data <- data %>%
      arrange(ID, start) %>%
      group_by(ID) %>%
      mutate(
        end_neu = if_else(is.na(dplyr::lead(start)), end, dplyr::lead(start))
      ) %>%
      ungroup() %>%
      select(-end) %>%
      rename(end = end_neu)
    
    # state duration
    data$time_sec <- as.numeric(difftime(data$end, data$start, units = "secs"))

    data <- create_ID_day_layout(data) %>% 
      mutate(time_on = 0,
             time_off = 0
             ) %>% 
      group_by(ID, day) %>% 
      summarise(
        time_on = sum(time_sec[state == "True"], na.rm = TRUE),
        time_off = sum(time_sec[state == "False"], na.rm = TRUE),
        .groups = "drop"
        ) %>% 
      mutate(time_total = (time_on + time_off)) %>% 
      arrange(ID, day)
    
   data <- data %>% 
     mutate(
       time_on = time_on / 60,
       time_off = time_off / 60,
       time_total = (time_on + time_off) / 60
       )
    
    return(data)
  }
  if (file_name == "sosci") {
    data <- data %>%
      mutate(
        ASKU = rowMeans(across(paste0("ASKU_", 1:3)), na.rm = TRUE),
        BADS = rowSums(across(paste0("BADS_", 1:9)), na.rm = TRUE),
        DAS  = rowSums(across(paste0("DAS_", 1:20)), na.rm = TRUE),
        PHQ  = rowSums(across(paste0("PHQ_", 1:9)), na.rm = TRUE),
        PTQ  = rowSums(across(paste0("PTQ_", 1:15)), na.rm = TRUE),
        RSQ  = rowSums(across(paste0("RSQ_", 1:10)), na.rm = TRUE),
        SDS  = rowSums(across(paste0("SDS_", 1:3)), na.rm = TRUE)
      )
    
    return(data)
  }
  if (file_name == "app_use") {
    data <- replace_ids(data)
    
    data <- data %>%
      rename(
        start_time_original = start_time,
        end_time_original = end_time,
        last_used_original = last_used,
        fg_time_ms_original = fg_time_ms
      )
    
    data <- data %>%
      arrange(ID, app_name, start_time_original, end_time_original) %>% 
      mutate(block = paste(ID, app_name, start_time_original, sep = "--")) %>% 
      group_by(block) %>%
      mutate(
        start_time = if_else(
          row_number() == 1,
          start_time_original,
          dplyr::lag(last_used_original)
          ),
        fg_time_ms = if_else(
          row_number() == 1,
          fg_time_ms_original,
          fg_time_ms_original - dplyr::lag(fg_time_ms_original)
          ),
        end_time = last_used_original
        ) %>%
      ungroup()
    
    data <- add_days(data, time_var = start_time) %>% 
      filter(fg_time_ms != 0) %>% 
      filter(day >= 1 & day <= 14)

    data <- data %>%
      select(ID, app_name, day, start_time, end_time, fg_time_ms)
    
    unique_combinations <- data %>%
      filter(fg_time_ms > 0) %>% 
      distinct(ID, app_name)
    
    data_selection <- data %>%
      group_by(ID, app_name, day) %>% 
      summarise(fg_time_ms = sum(fg_time_ms, .na.rm = TRUE), .groups = "drop")
    
    data <- unique_combinations %>%
      tidyr::crossing(day = 1:14) %>%
      left_join(data_selection, by = c("ID", "app_name", "day")) %>% 
      mutate(fg_time_ms = coalesce(fg_time_ms, 0)) %>% 
      arrange(ID, app_name, day)

    apps <- process_data("apps")
    
    data <- merge(data, apps,
                  by.x = "app_name", by.y = "package_name",
                  all.x = TRUE)
    
    data <- data %>%
      select(ID, app_name, day, fg_time_ms, category, subcategory, relevance) %>% 
      mutate(fg_time_ms = fg_time_ms / 1000 / 60)
    
    return(data)
  }
  if (file_name == "ue") {
    data <- data %>%
      mutate(
        across(c(SUTAQ_1, SUTAQ_3, SUTAQ_5, SUTAQ_7), ~7-.),
        SUTAQ = rowMeans(across(paste0("SUTAQ_", 1:9)), na.rm = TRUE),
        UEQ = rowMeans(across(paste0("UEQ_", 1:8)), na.rm = TRUE)
      )
    return(data)
  }
  if (file_name == "demography") {
    data <- data %>%
      mutate(
        age_group = as.factor(ifelse(age > 17, "E", "J")))
    return(data)
  }
  else return(data)
}
