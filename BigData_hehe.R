library(dplyr)

prepare_app_use <- function() {
  file_path <- c("C:/Users/Jonas/Clouds/Nextcloud/5170.SMARDY/Daten EthicaData/Rohdaten/")
  
  file_names <- c("app_usage_1", 
                "app_usage_2",
                "app_usage_3",
                "app_usage_4",
                "app_usage_5"
                )
  
  raw_data <- list()
  
  for (file_name in file_names) {
    full_file <- paste0(file_name, ".csv")
    absolute_path <- file.path(file_path, full_file)
    raw_data[[file_name]] <- read.csv(absolute_path)
    cat(paste0("File completed: ", full_file, "\n"))
    }

  combined_df <- bind_rows(raw_data)
  
  # Filter data
  combined_df <- combined_df %>% 
    filter(fg_time_ms > 0)
  
  columns_to_compare <- c("user_id", "app_name", "end_time", "start_time", "fg_time_ms", "last_used")
  
  unique_df <- combined_df %>%
    distinct(across(all_of(columns_to_compare)))
  
  # save as csv
  save_location <- file.path(getwd(), "data", "app_use.csv")
  write.csv(unique_df, save_location, row.names = FALSE)
}

prepare_app_use()
