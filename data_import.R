# needed packages
library(dplyr)
library(tidyr)
library(lubridate)
Sys.setenv(TZ = "UTC")

# import raw data from csv
import_data <- function(file_name) {
  filepath <- file.path(getwd(), "data", paste0(file_name,".csv"))
  data <- read.csv(filepath, header = TRUE, sep = ",")
  if (file_name == "apps") {
    data <- data %>%
      mutate(subcategory = as.factor(subcategory),
             category = as.factor(category),
             relevance = as.factor(relevance))
    return(data)
  }
  if (file_name == "codebook") {
    data <- data %>%
      mutate(
        ID = as.factor(ID),
        id_ethica = as.factor(id_ethica),
        T0 = parse_date(T0, type = "date"),
        T1 = parse_date(T1, type = "date"),
        T1_calculated = parse_date(T1_calculated, type = "date"),
        app_usage = as.factor(app_usage),
        sufficient_data = as.factor(sufficient_data),
        usable = as.factor(usable),
        age = as.integer(age),
        sex = as.factor(sex),
        T0_day = weekdays(T0, abbreviate = TRUE)
      )
    return(data)
  }
  if (file_name == "ema") {
    data <- data %>%
      mutate(ID = as.factor(ID),
             record_time = parse_date(record_time, type = "datetime", format = "dmy HM"),
             mutate(across(starts_with(c("PHQ_", "PANAVA_", "HiTOP_", "BADS_", "MRNT_")), as.integer)),
             status = as.factor(status)
             )
    return(data)
  }
  if (file_name == "screenstate") {
    data <- data %>%
      mutate(
        user_id = as.factor(user_id),
        end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"),
        record_time = as.POSIXct(record_time, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"),
        state = as.factor(state)
        )
    return(data)
  }
  if (file_name == "sosci") {
    data <- data %>%
      mutate(Time = as.factor(Time),
             ID = as.factor(ID),
             across(starts_with(c("ASKU_", "BADS_", "DAS_", "SDS_", "ETAM_", "PHQ_", "PTQ_", "RSQ_")), as.integer)
             )
    return(data)
  }
  if (file_name == "app_use") {
    data <- data %>%
      mutate(
        user_id = as.factor(user_id),
        fg_time_ms = as.integer(fg_time_ms),
        start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"),
        end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"),
        last_used = as.POSIXct(last_used, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"),)
    return(data)
  }
  if (file_name == "ue") {
    data <- data %>%
      mutate(ID = as.factor(ID),
             across(starts_with(c("SUTAQ_", "UEQ_")), as.integer))
    return(data)
  }  
  if (file_name == "modules") {
    data <- data %>%
      mutate(
        ID = as.factor(ID),
        across(starts_with(c("H_", "Rel_")), as.integer)
      )
    return(data)
  }
  if (file_name == "demography") {
    data <- data %>%
      mutate(
        ID = as.factor(ID),
        age = as.integer(age),
        sex = as.factor(sex),
        marital_status = as.factor(marital_status),
        residence	= as.factor(residence),
        nationality	= as.factor(nationality),
        ed_qual	= as.factor(ed_qual),
        occupation = as.factor(occupation),
        dmhi_exp = as.factor(dmhi_exp),
        clin = as.factor(clin),
        cur_therapy = as.factor(cur_therapy),
        mult_diag = as.factor(mult_diag)
      )
    return(data)
  }
  else return(data)
}
