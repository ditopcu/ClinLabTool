format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)


number_format <- function(x, n = 1) format(round(x, n), nsmall = n)

en_weekdays <- structure(1:7, .Label = c("Mon", "Tue", "Wed", "Thu", "Fri", 
                               "Sat", "Sun"), class = c("ordered", "factor"))


en_wday <- function(date, week_start = 1) {
  
  
  en_weekdays[wday(date, label = FALSE, week_start = week_start)]
  # wday(receiving_date, label = TRUE, week_start = 1, locale="English_United States"))
}



get_device_analyse_data <- function(data, specs) {
  
  raw_data <- data |> 
    filter(!is.na(result_time )) |> 
    arrange(result_time) |> 
    mutate(result_date = as_date(result_time), hour = hour(result_time)) |> 
    select(index, device,  sub_device, result_time, test_name, result_date, hour) 
  
  sum_data <- raw_data |> 
    group_by(device, sub_device, result_date, hour) |> 
    summarise(test_count = max(row_number()), .groups = "drop") |> 
    left_join(specs, by = c("device", "sub_device") ) |> 
    rename(device_capacity = test_per_hour) |> 
    mutate(used_device_capacity = test_count/device_capacity*100) |> 
    mutate(w_day = en_wday(result_date)) |> 
    select(result_date, w_day, hour, test_count, device_capacity, used_device_capacity) |> 
    group_by(result_date) |> 
    mutate(cum_test_count = cumsum(test_count), cum_device_capacity = cumsum(device_capacity )) |>  
    ungroup() |> 
    arrange(result_date, hour)
  
  
  
}


summarise_device_load_weekday <- function(df) {
  
  summary_week_day <- df |> 
    group_by(w_day, hour) |> 
    summarise(across(test_count:cum_device_capacity, median), .groups = "drop") 
  
  summary_week_day
  
}

summarise_device_load_overall <- function(df) {
  
  summary_overall <- df |> 
    group_by(hour) |> 
    summarise(across(test_count:cum_device_capacity, median))
  
  summary_overall
  
}



hourly_TAT_formatter <- function(df) {
  
  df |> 
    mutate(hour = format_hm(as.character(hms::hms(hour = hour)))) |> 
    mutate(median_TAT = number_format(median_TAT, 2)) |> 
    spread(hour, median_TAT, fill = "-") 
    
}


summarise_TAT_hourly_multiple <- function(TAT_df){
  
  
  TAT_data <- TAT_df  |>    
    arrange(receiving_time) |> 
    mutate(receiving_date = as_date(receiving_time), w_day =en_wday(receiving_time) ) |> 
    mutate( hour = hour(receiving_time))  
  
  TAT_summary_hourly <- TAT_data |> 
    group_by(w_day, receiving_date,  hour) |> 
    summarise(median_TAT = median(inlab_TAT), .groups = "drop") |> 
    arrange(receiving_date, hour) |> 
    select(receiving_date, w_day, hour, median_TAT ) 
  
  TAT_summary_hourly 
  

  
  
}

summarise_TAT_hourly_weekday <- function(TAT_df){
  
  TAT_data <- TAT_df  |>    
    arrange(receiving_time) |> 
    mutate(receiving_date = as_date(receiving_time), w_day =en_wday(receiving_time) ) |> 
    mutate(hour = hour(receiving_time)) 
  
  TAT_summary_weekday <- TAT_data |> 
    group_by(w_day, hour)|> 
    summarise(median_TAT = median(inlab_TAT), .groups = "drop")
  
  TAT_summary_weekday 
  
  

  

}

summarise_TAT_hourly_overall <- function(TAT_df){
  
  TAT_data <- TAT_df  |>    
    arrange(receiving_time) |> 
    mutate(receiving_date = as_date(receiving_time), w_day =en_wday(receiving_time) ) |> 
    mutate(recievin_date = as_date(receiving_time), hour = hour(receiving_time)) 
  
  TAT_summary_overall <- TAT_data |> 
    group_by(hour)|> 
    summarise(median_TAT = median(inlab_TAT), .groups = "drop")
  
  TAT_summary_overall 
  
}



calc_TAT <- function(df, type = "in_lab") {
  
  if (type == "in_lab") {
    
    df |> 
      mutate(inlab_TAT = as.double(difftime(first_validation_time, receiving_time, units = "hours"))) |> 
      select(sample_id,receiving_time, test_id, test_name, test_group, inlab_TAT)
    # select(sample_id,test_name, first_validation_time, receiving_time, TAT)
    
    
  }
  
}

TAT_outlier_calc <- function(df, type = "by_test") {
  
  if (type == "by_test") {
    group_val = c("test_group", "test_name")
  } else if (type == "by_test_group") {
    group_val = "test_group"
  }
  
  
  no_outlier_TAT_df <- df  |> 
    group_by(across(all_of(group_val  )))  |> 
    mutate(Q1 = quantile(inlab_TAT, probs = 0.25), Q3 = quantile(inlab_TAT, probs = 0.75), IQR = IQR(inlab_TAT) ) |> 
    mutate(low_limit = Q1 - 1.5 * IQR, high_limit = Q3 + 1.5 * IQR) |> 
    select(test_id, test_name, test_group, inlab_TAT, Q1, Q3, IQR, low_limit, high_limit) |> 
    ungroup()

  no_outlier_TAT_df
  
}


summarise_TAT <- function(df, type = "by_test", exclude_outlier = FALSE) {
  
  if (type == "by_test") {
    group_val = c("test_group", "test_name")
  } else if (type == "by_test_group") {
    group_val = "test_group"
  }
  
  if (exclude_outlier) {
    
    no_outlier_TAT_df <- df  |> 
      group_by(across(all_of(group_val  )))  |> 
      mutate(Q1 = quantile(inlab_TAT, probs = 0.25), Q3 = quantile(inlab_TAT, probs = 0.75), IQR = IQR(inlab_TAT) ) |> 
      mutate(low_limit = Q1 - 1.5 * IQR, high_limit = Q3 + 1.5 * IQR) |> 
      select(test_id, test_name, test_group, inlab_TAT, Q1, Q3, IQR, low_limit, high_limit) |> 
      filter(inlab_TAT >=  low_limit, inlab_TAT <=high_limit)  |> 
      ungroup()
    
    no_outlier_TAT_sum <- no_outlier_TAT_df |> 
      group_by(across(group_val)) |> 
      summarise(mean_TAT = mean(inlab_TAT ), sd = sd(inlab_TAT ),
                median_TAT =  median(inlab_TAT),IQR = IQR(inlab_TAT ), .groups = "drop_last") |> 
      ungroup()
    
    return(no_outlier_TAT_sum)
    
  }
  
  all_TAT_sum <- df  |> 
    group_by(across(group_val)) |> 
    summarise(mean_TAT = mean(inlab_TAT ), sd = sd(inlab_TAT ),
              median_TAT =  median(inlab_TAT),IQR = IQR(inlab_TAT ), .groups = "drop_last")  |> 
    ungroup()
  
  all_TAT_sum
}
