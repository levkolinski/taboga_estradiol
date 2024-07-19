library(tidyverse)
library(lme4)
set.seed(1234)
# Loading data ----
e_data<-read.csv("/Users/levkolinski/Desktop/taboga_estradiol/Data/estradiol_dataset.csv", sep = ";")
duckface_data<-read.csv("/Users/levkolinski/Desktop/taboga_estradiol/Data/duckfaces_dataset.csv", sep = ";")
head(duckface_data)

# ## removing outlier E2 values ----
# e_data_outliers <- e_data |> 
#   filter(e_conc_ug < (quantile(e_conc_ug, 0.25) - 1.5 * IQR(e_conc_ug)) |
#            e_conc_ug > (quantile(e_conc_ug, 0.75) + 1.5 * IQR(e_conc_ug))) |> 
#   pull(sample_id)
# 
# e_data <- e_data |>
#   filter(!sample_id %in% e_data_outliers)

## adding in age year column ----
e_data <- e_data |> 
  mutate(age_at_sample_years = as.numeric(sub("\\..*", "", age_at_sample)))
colnames(e_data)
# Population level age/E2 trends ----

## calculating average pre-pubertal E2 ----
prepubertal_data<-e_data |> 
  filter(age_at_sample >= 2.5 & age_at_sample <= 3) |>  # we want age 2.5-3...def pre menstruation
  select(ind_id, age_at_sample, e_conc_ug) |> 
  summarise(mean_e_conc_ug = mean(e_conc_ug, na.rm = TRUE),
            sd_e_conc_ug = sd(e_conc_ug, na.rm = TRUE)) 

mean_prepubertal_e_conc_ug <- prepubertal_data$mean_e_conc_ug
sd_prepubertal_e_conc_ug <- prepubertal_data$sd_e_conc_ug

calculated_treshold <- mean_prepubertal_e_conc_ug + 2*sd_prepubertal_e_conc_ug
print(paste("Calculated E2 threshold for menarche: ", calculated_treshold))

## creating df ----
e_data_age_at_menarche_df <- e_data |>
  select(c(age_at_sample, age_at_sample_years, e_conc_ug)) |>
  group_by(age_at_sample) |>
  mutate(mean_e_at_age = mean(e_conc_ug, na.rm = TRUE),
         se_e_at_age = sd(e_conc_ug, na.rm = TRUE) / sqrt(n())) |>
  ungroup() |>
  arrange(age_at_sample) |>
  distinct() |> 
  group_by(age_at_sample_years) |>
  mutate(mean_e_at_age_years = mean(e_conc_ug, na.rm = TRUE),
         se_e_at_age_years = sd(e_conc_ug, na.rm = TRUE) / sqrt(n())) |>
  ungroup() |>
  select(-e_conc_ug) |>
  arrange(age_at_sample_years) |>
  distinct() 

## average_age_at_menarche----
average_age_at_menarche <- e_data_age_at_menarche_df |> 
  filter(age_at_sample > 3 & age_at_sample<10) |> 
  filter(mean_e_at_age > calculated_treshold) |> 
  slice(1) |> 
  pull(age_at_sample)


## get sample size ----
n_figure_1 <- nrow(e_data |> 
                     filter(age_at_sample > 2.5))

##Figure 1 ----
ggplot(data = e_data_age_at_menarche_df |> 
         select(age_at_sample_years, mean_e_at_age_years, se_e_at_age_years) |> 
         distinct(), aes(x = age_at_sample_years, y = mean_e_at_age_years)) + 
  geom_smooth() + 
  geom_point()+
  geom_vline(xintercept = average_age_at_menarche, linetype = "dashed", color = "red") +
  geom_hline(yintercept = calculated_treshold, linetype = "dashed", color = "skyblue") +
  geom_errorbar(aes(ymin = mean_e_at_age_years - se_e_at_age_years, ymax = mean_e_at_age_years + se_e_at_age_years), width = 0.2) +
  # scale_y_continuous(limits = c(0, 5)) +
  ggtitle(paste0("Estradiol by estimated age at sample (N = ", n_figure_1, " samples)"))+
  xlab("Age at sample")+
  ylab("Estradiol (ug/g)")
# 
# ##extracting geom_smooth formula ----
# loess_model <- loess(mean_e_at_age_years ~ age_at_sample_years, data = e_data_figure1_df)
# summary(loess_model)
# 
# ## finding point where geom_smooth slope is 0 ----
# x_seq <- seq(min(e_data_figure1_df$age_at_sample_years), max(e_data_figure1_df$age_at_sample_years), 
#              length.out = 1000)
# 
# # Predict y values and derivatives
# y_pred <- predict(loess_model, newdata = data.frame(age_at_sample_years = x_seq))
# y_deriv <- diff(y_pred) / diff(x_seq)
# 
# # Find where the derivative crosses zero
# zero_crossings <- which(diff(sign(y_deriv)) != 0)
# x_zero_slope <- min(x_seq[zero_crossings])
# 
# 
# ggplot(data = e_data_figure1_df, aes(x = age_at_sample_years, y = mean_e_at_age)) + 
#   geom_smooth() + 
#   geom_point()+
#   geom_vline(xintercept = average_age_at_menarche, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = x_zero_slope, linetype = "dashed", color = "darkgreen") +
#   geom_hline(yintercept = calculated_treshold, linetype = "dashed", color = "skyblue") +
#   geom_errorbar(aes(ymin = mean_e_at_age - se_e_at_age, ymax = mean_e_at_age + se_e_at_age), width = 0.2) +
#   # scale_y_continuous(limits = c(0, 5)) +
#   ggtitle(paste0("Estradiol by estimated age at sample (N = ", n_figure_1, " samples)"))+
#   xlab("Age at sample")+
#   ylab("Estradiol (ug/g)")

#Figure 2----

# Panel of all individual females who have good E coverage before and after menarche
females_for_menarche_panel<-e_data |> 
  group_by(ind_id) |> 
  filter(min(age_at_sample)<3) |>
  mutate(count = n()) |> 
  ungroup() |> 
  select(ind_id,count) |> 
  arrange(desc(count)) |> 
  distinct() |> 
  filter(count>25) |> 
  pull(ind_id)

females_for_menarche_panel_df<- e_data |> 
  filter(ind_id %in% females_for_menarche_panel) |> 
  filter(age_at_sample < 8) |>
  select(ind_id, age_at_sample, sample_id, date, month, time, e_conc_ug, is_pregnant) |> 
  arrange(ind_id, age_at_sample)

n_figure_2 <-nrow(females_for_menarche_panel_df)

ggplot(data = females_for_menarche_panel_df, aes(x = age_at_sample, y = e_conc_ug, color = ind_id)) +
  geom_point()+
  geom_vline(xintercept = average_age_at_menarche, linetype = "dashed", color = "red") +
  geom_hline(yintercept = calculated_treshold, linetype = "dashed", color = "skyblue") +
  facet_wrap(~ ind_id)+
  ggtitle(paste0("Estradiol by age at sample, separated by individual (n = ", n_figure_2, " samples)"))+
  xlab("Age at sample")+
  ylab("Estradiol (ug/g)")+
  theme(legend.position = "none")

females_for_menarche_panel_df |> 
  filter(ind_id =="MIL") |> 
  arrange(sample_id) |> 
  View() 


# Rise in E ~ first duck face----

## prepping data
first_duckface_data <- duckface_data |> 
  select(Actor,Date, BehaviorType, Recipient) |> 
  filter(Actor %in% females_for_menarche_panel) |> 
  group_by(Actor) |> 
  slice_min(Date) |> 
  distinct() |> 
  rename("ind_id" = Actor) |> 
  rename("first_duckface_date" = Date)


calculate_first_rise <- function(df) {
  df %>%
    group_by(ind_id) |> 
    arrange(date) %>%
    filter(age_at_sample >2.5) |> 
    mutate(Rise = e_conc_ug > lag(e_conc_ug, default = first(e_conc_ug))) %>%
    filter(Rise) %>%
    slice(1) 
}


first_rise_data <- e_data %>%
  group_by(ind_id) %>%
  do(calculate_first_rise(.)) %>%
  ungroup() %>%
  select(ind_id, First_Rise_Date = date, age_at_sample) |> 
  filter(ind_id %in% females_for_menarche_panel)



merged_data <- left_join(first_duckface_data, first_rise_data, by = "ind_id")

reference_date <- as.Date("2023-09-01")

merged_data$first_duckface_date <-as.Date(merged_data$first_duckface_date)
merged_data$First_Rise_Date <-as.Date(merged_data$First_Rise_Date)

merged_data$first_duckface_date <-as.numeric(reference_date-merged_data$first_duckface_date)
merged_data$First_Rise_Date <-as.numeric(reference_date-merged_data$First_Rise_Date)
mean(merged_data$age_at_sample)


model <- lm(First_Rise_Date ~ first_duckface_date, data = merged_data)
summary(model)


model <- lmer(First_Rise_Date ~ first_duckface_date + (1 | ind_id), data = merged_data)
summary(model)

# checking model diagnostics - FAIL
par(mfrow = c(2,2))
plot(model)

# residuals v fitted is NOT linear....bad
# normal q-q plot looks pretty straight....good


# trying bayesian approach
library(brms)
library(lme4)
e_data$e_conc_ug

# E and pregnancy ----
pregnant_df<-e_data |> 
  filter(preg_trim != "") |> 
  select(ind_id, age_at_sample, e_conc_ug, preg_trim, pregnancy_num)
View(pregnant_df)

n_pregnant_df <- nrow(pregnant_df)

par(mfrow = c(1,1))
ggplot(data = pregnant_df, aes(x = preg_trim, y = log(e_conc_ug)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.1)+
  ggtitle(paste("Estradiol across gestation (N = ", n_pregnant_df, "samples)")) +
  xlab("Pregnancy trimester")+
  ylab("Log-transformed fecal estradiol (ug/g)")
            


anova_result_pregancy_e <- aov(log(e_conc_ug) ~ preg_trim, data = pregnant_df)
summary(anova_result_pregancy_e)
tukey_results_pregnancy_e <- TukeyHSD(anova_result_pregancy_e)
plot(tukey_results_pregnancy_e)



