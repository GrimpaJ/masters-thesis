Sys.setenv(TZ = "UTC")

if (!exists("lpa_done", envir = globalenv())) {
  lpa_done <- FALSE
}

check_packages <- function() {
  needed_packages <- c(
    # Basic functionality
    "dplyr",
    "tidyr",
    "tidyverse",
    "lubridate", # date / time objects
    "effsize", # effect sizes
    "rstatix", # effect sizes (Wilcoxen)
    "Hmisc",
    # Plotting
    "ggplot2",
    "plotly", # interactive plots
    #"randomcoloR", # color palettes
    #"colorspace", # color palettes
    "RColorBrewer", # color palettes
    "reshape2",
    #"corrplot",
    "viridis",
    "shiny", # shiny App
    "shinyWidgets", # shiny App
    # LMMs
    "lme4",
    "lmerTest",
    # other
    "car",         # Levene Test
    "mclust", # lpa
    "boot",
    "dtwclust", # time series clustering
    "doParallel", # boot lpa
    "foreach", # boot lpa
    "ltm" # point-biserial correlation
  )

  for (package in rev(needed_packages)) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  }
}

source_script <- function(script_name) {
  script_name <- paste0(script_name, ".R")
  script_path <- file.path(getwd(), "code", script_name)
  source(script_path)
}

save_csv <- function(data) {
  data_name <- deparse(substitute(data))
  save_name <- paste0(data_name, ".csv")
  save_location <- file.path(getwd(), "output", save_name)
  write.csv(data, save_location, row.names = FALSE)
}



parse_date <- function(time_var, type = "datetime", format = NULL) {
  if (type == "datetime") {
    orders_used <- if (is.null(format)) "%Y-%m-%dT%H:%M:%OS%z" else format
    return(parse_date_time(time_var, orders = orders_used, tz = "UTC"))
  } else if (type == "date") {
    orders_used <- if (is.null(format)) "dmy" else format
    dt <- parse_date_time(time_var, orders = orders_used, tz = "UTC")
    return(as.Date(dt))
  } else {
    stop("Unbekannter type. Bitte 'datetime' oder 'date' verwenden.")
  }
}


replace_ids <- function(data, var_name = "user_id") {
  codes_ethica <- process_data("codebook") %>%
    select(id_ethica, ID)
  
  data <- data %>%
    left_join(codes_ethica, by = setNames("id_ethica", var_name)) %>% 
    select(-all_of(var_name)) %>%
    relocate(ID, .before = everything())
  return(data)
}

add_days <- function(data, time_var = record_time) {
  day_calculation <- import_data("codebook") %>% 
    select(ID, T0)

  data <- data %>% 
    left_join(day_calculation, by = "ID") %>% 
    mutate(adjusted_time = {{time_var}} + hours(4) + minutes(15),
           day = as.numeric(as.Date(adjusted_time) - T0) - 1
           ) %>% 
    select(-T0, -adjusted_time)

  return(data)

}

create_ID_day_layout <- function(data, fill = NA) {
  unique_combinations <- data %>%
    select(ID) %>%
    distinct()
  
  new_data <- unique_combinations %>% 
    crossing(day = 1:14) %>%
    left_join(data, by = c("ID", "day")) %>% 
    mutate(across(everything(), ~ coalesce(., fill)))
  
  return(new_data)
}

remove_excluded_ids <- function(data, cutoff = 0.2) {
  codebook <- process_data("codebook")
  
  app_usage_data <- codebook %>%
    select(ID, app_usage)
  
  missing_phq_data <- process_data("ema") %>%
    group_by(ID) %>%
    summarise(missing_ratio = sum(is.na(PHQ)) / 14, .groups = 'drop')
  
  filtered_data <- data %>%
    left_join(app_usage_data, by = "ID") %>%
    left_join(missing_phq_data, by = "ID") %>%
    filter(app_usage != "no" & missing_ratio < cutoff) %>%
    select(-app_usage, -missing_ratio)
  
  return(filtered_data)
}

# 
rename_weekday <- function(ID, day) {
  current_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  
  codebook <- process_data("codebook")
  
  days_long <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  days_short <- c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")
  
  #T0_date <- codebook$T0[ID == ID]
  T0_date <- codebook$T0[match(ID, codebook$ID)]
  T0_day <- weekdays(as.Date(T0_date))
  start_index <- match(T0_day, days_long)
  
  current_index <- (start_index - 1 + (day - 1)) %% 7 + 1
  next_index <- ifelse(current_index == length(days_long), 1, current_index + 1)
  
  Sys.setlocale("LC_TIME", current_locale)
  
  return(paste0(days_short[current_index], " > ", days_short[next_index]))
}

remove_irrelevant_apps <- function(data) {
  irrelevant_apps <- process_data("apps") %>%
    select(package_name, relevance) %>%
    rename(app_name = package_name)
  
  data <- data %>%
    left_join(irrelevant_apps, by = "app_name")
  
  data <- data %>%
    filter(relevance != "no") %>%
    select(-relevance)

  return(data)
}
