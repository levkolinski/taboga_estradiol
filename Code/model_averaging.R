# loading packages
library(tidyverse)
library(lme4)
library(MuMIn)
library(zoo)
library(sjPlot)

# loading weather data
weather_data<-read.csv("/Users/levkolinski/Desktop/taboga_estradiol/Data/MERRA2_dataset.csv", sep = ",") |> 
  select(-X)
weather_data$date<-as.Date(weather_data$date)
weather_data$rainfall<-as.numeric(weather_data$rainfall)
weather_data$temperature <- as.numeric(weather_data$temperature)


# loading e data
e_data<-read.csv("/Users/levkolinski/Desktop/taboga_estradiol/Data/estradiol_dataset.csv", sep = ";") 

e_data<-e_data |> 
  ## adding in age year column
  mutate(age_at_sample_years = as.numeric(sub("\\..*", "", age_at_sample))) |> 
  ## adding in hour column to account for diurnal variation
  mutate(hour = as.numeric(str_extract(time, "^[^:]+"))) 
e_data$date<-as.Date(e_data$date)


# loading duckface data
duckface_data<-read.csv("/Users/levkolinski/Desktop/taboga_estradiol/Data/duckfaces_dataset.csv", sep = ";")

duckface_data$Date<-as.Date(duckface_data$Date)
duckface_data<-duckface_data |> 
  rename("ind_id" = Actor) |> 
  rename("date" = Date) |> 
  select(ind_id, date, age_at_sample)


# Creating functions
# Calculate cumulative rainfall over the last 30 days
calculate_weather_metrics <- function(combined_df, weather_data) {
  # Join the weather data with the combined dataframe
  combined_df_with_weather <- combined_df %>%
    left_join(weather_data, by = "date") %>%
    arrange(ind_id, date)
  
  # Calculate cumulative rainfall for the past 30 days
  combined_df_with_weather <- combined_df_with_weather %>%
    mutate(
      mean_rain_last_30_days = rollapply(rainfall, width = 30, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
      mean_max_temp_last_30_days = rollapply(temperature, width = 30, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(combined_df_with_weather)
}



# finding duckface and E data within 5 days of each other
expanded_duckface_data_5_days <- duckface_data %>%
  rowwise() %>%
  do(data.frame(ind_id = .$ind_id, 
                date = seq(.$date - days(3), .$date + days(2), by = "day"),
                age_at_sample = .$age_at_sample,
                original_duckface_date = .$date
  )) %>%
  ungroup()


duckface_e_df_5_days <- expanded_duckface_data_5_days %>%
  inner_join(e_data, by = c("ind_id", "date")) |> 
  select(ind_id, sample_id, age_at_sample.x, date, original_duckface_date, e_conc_ug) %>%
  rename("sample_date" = date,
         "age_at_sample" = age_at_sample.x) %>%
  distinct(sample_id, .keep_all = TRUE) %>%  # in case there are more than 1 duckfaces observed by sampled date, only keep 1
  mutate("duckface_sample" = 1L) 

duckface_sampleids_5_days<-duckface_e_df_5_days |> 
  pull(sample_id)

non_duckface_e_df_5_days <- e_data |> 
  filter(!sample_id %in% duckface_sampleids_5_days) |> 
  select(ind_id, sample_id, age_at_sample, date, e_conc_ug) |> 
  mutate("duckface_sample" = 0L)

combined_df_5_days <- bind_rows(duckface_e_df_5_days, non_duckface_e_df_5_days) |> 
  mutate(date = coalesce(sample_date, date)) %>%
  select(-sample_date)

# calculating rainfall in past 30 days of e sample
combined_df_5_days <- calculate_weather_metrics(combined_df_5_days, weather_data) |> 
  filter(!is.na(mean_rain_last_30_days)) |> 
  filter(!is.na(mean_max_temp_last_30_days))

## breaking df into young and old

combined_df_5_days_young<-combined_df_5_days |> 
  filter(age_at_sample < mean_age_above_threshold)

combined_df_5_days_mature <- combined_df_5_days |> 
  filter(age_at_sample >= mean_age_above_threshold)


shapiro.test(log(combined_df_5_days_young$e_conc_ug))
shapiro.test(log(combined_df_5_days_mature$e_conc_ug))


## mature dataset

# running t test to look at E and duckface...significant difference!
t_test_result_5_days <- t.test(log(e_conc_ug) ~ duckface_sample, data = combined_df_5_days_mature)


ggplot(data = combined_df_5_days_mature, aes(x = factor(duckface_sample), y = log(e_conc_ug))) +
  geom_boxplot() 


full_formula_lmer_mature <- lmer(log(e_conc_ug) ~ age_at_sample + duckface_sample + 
                            scale(mean_rain_last_30_days) +scale(mean_max_temp_last_30_days) + (1 | ind_id),
                          data = combined_df_5_days_mature, 
                          na.action = "na.fail")


plot_model(full_formula_lmer_mature, type = "est", show.values = TRUE, value.offset = .3)

dredged_model_lmer_mature <- dredge(full_formula_lmer_mature)
averaged_model_lmer_mature <- model.avg(dredged_model_lmer_mature, subset = delta < 2)
summary(averaged_model_lmer_mature)



## young dataset

# running t test to look at E and duckface...significant difference!
t_test_result_5_days_young <- t.test(log(e_conc_ug) ~ duckface_sample, data = combined_df_5_days_young)


ggplot(data = combined_df_5_days_young, aes(x = factor(duckface_sample), y = log(e_conc_ug))) +
  geom_boxplot() 


full_formula_lmer_young <- lmer(log(e_conc_ug) ~ age_at_sample + duckface_sample + 
                                   scale(mean_rain_last_30_days) +scale(mean_max_temp_last_30_days) + (1 | ind_id),
                                 data = combined_df_5_days_young, 
                                 na.action = "na.fail")


plot_model(full_formula_lmer_young, type = "est", show.values = TRUE, value.offset = .3)

dredged_model_lmer_young <- dredge(full_formula_lmer_young)
averaged_model_lmer_young <- model.avg(dredged_model_lmer_young, subset = delta < 2)







