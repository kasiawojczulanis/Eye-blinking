# Intra-observer repeatability 

# Observer: Feliksa: n = 29 (to be 30)
# randomly selected files/recs from three sets, seasons: 2021-2023)


# Basics/libs ----

rm(list = ls())
library(tidyverse)
library(readxl)
library(magrittr)

# Read and adjust data ----

# Folder with data
path <- "./Raw data/eyeblinks_reps"

csvfile <- list.files(path = path, pattern = "*.csv", recursive = TRUE)

df_blinks <- list()

for(i in 1:length(csvfile)) {
  
  df_temp <- read.csv(file = paste(path, csvfile[i], sep = "/"))
  
  
  # file/bird id
  filename <- csvfile[i]
  
  
  # recording duration 
  onset <- df_temp$time[1]
  ending <- tail(df_temp$time, 1)
  recording_duration <- ending - onset
  
  # disturbance duration
  disturb_dur_temp <- df_temp %>%
    select(code, time) %>% 
    filter(code %in% c("disturbance_start", "disturbance strat", "disturbance_strat", "disturbance start", "dis start",
                       "disturbance end", "disturbance_end", "dis end")) %>% 
    mutate(code = if_else(code == "disturbance_strat", "disturbance_start", 
                          if_else(code == "disturbance strat", "disturbance_start", 
                                  if_else(code == "disturbance start", "disturbance_start", 
                                          if_else(code == "dis start", "disturbance_start", code)))),
           code = if_else(code == "disturbance end", "disturbance_end", 
                          if_else(code == "dis end", "disturbance_end", code)))
  
  
  
  
  # files with some disturbance
  if (nrow(disturb_dur_temp) > 0) {
    
    disturb_dur_temp$dist_block <- rep(seq(from = 1, 
                                           to = nrow(disturb_dur_temp)/2, 
                                           by = 1), each = 2)
    
    disturb_dur <- disturb_dur_temp %>% 
      pivot_wider(names_from = code, values_from = time) %>% 
      mutate(dist_time_diff = disturbance_end - disturbance_start)
    
    disturbance_duration <- sum(disturb_dur$dist_time_diff)
    
  }
  
  # files with no disturbance
  if (nrow(disturb_dur_temp) == 0) {
    
    disturbance_duration <- 0
    
  }
  
  
  #  n blinkings
  blinking_recs <- df_temp %>%
    filter(code  %in% c("blinkig", "blinkink", "eye_blinking", "blinking", "blink")) %>% 
    distinct()
  
  n_blinks <- nrow(blinking_recs)
  
  df_blinks[[i]] <- data.frame(filename,  recording_duration, disturbance_duration, n_blinks)
  
}

df <- lapply(df_blinks, data.frame) %>%
  bind_rows()

df_rate <- df %>% 
  mutate(valid_duration_time = recording_duration - disturbance_duration,
         eye_rate = n_blinks/valid_duration_time,
         ring = str_sub(string = filename, start = 1, end = 5))


# Fix season
reps_metadt <- readxl::read_excel("./Raw data/eyeblinks_reps.xlsx", col_types = "text")

df_rate_rep <- left_join(df_rate, reps_metadt, by = "ring") %>% 
  relocate(ring, .after = season) %>% 
  mutate(seas_ring = paste(season, "_", ring, sep = ""),
         set = "rep")

rds_title <- "./Preprocessed data/df_rate_reps"
saveRDS(df_rate_rep, rds_title)

# individual apparently not processed twice (for now to be deleted, so n = 29)
df_rate_rep <- df_rate_rep [-18, ]

# Read original set

df_rate_orig <- readRDS("./Preprocessed data/df_allfiltered.RDS") 
df_rate_orig <- df_rate_orig %>% 
  select(filename, recording_duration, disturbance_duration, n_blinks, 
         valid_duration_time, eye_rate, season, ring) %>% 
  mutate(seas_ring = paste(season, "_", ring, sep = ""),
         set = "orig") %>% 
  filter(seas_ring %in% df_rate_rep$seas_ring) 

# Combine rep and orig
meas_rpt <- rbind(df_rate_orig, df_rate_rep)


rds_title <- "C:/Users/User/Dropbox/Working files/Projects/Eye-blinking/Preprocessed data/measurments_rpt.RDS"
saveRDS(meas_rpt, rds_title)

# Final analysis ----

meas_rpt <- readRDS("./Preprocessed data/measurments_rpt.RDS")

# Plot
ggplot(meas_rpt, aes(x = ring, y = eye_rate, 
                     group = set, col = set)) + 
  geom_line(aes(group = ring), col = "black") +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  geom_hline(aes(yintercept = mean(df_rate_orig$eye_rate)), col = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(meas_rpt$eye_rate)), col = "black", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(df_rate_rep$eye_rate)), col = "blue", linetype = "dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Bird ID", y = "Eye blinking rate [n/sec]", col = "data set (orig/rep)",
       title = "Repeatability (intra-observer difference)",
       subtitle = "Horizontal lines denote the mean for considered sample (n = 29)")


# Model

library(rptR)
set.seed(130478)
rpt_eye_observer <- rpt(eye_rate ~ 1 + (1 | ring), 
                        data = meas_rpt,
                        grname = "ring", nboot = 100,
                        npermut = 100, datatype = "Gaussian")

