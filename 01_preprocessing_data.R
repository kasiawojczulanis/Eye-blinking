rm(list = ls())

library(tidyverse)
library(readxl)
library(magrittr)
setwd("C:/Users/User/Dropbox")


# Combine and pre-process raw data files ------

# To be performed season by season (for better quality/consistency check); enter the manually

# seas <- "2019" #specific for the season - set measured by Emilia (which apparently has different way to annotate the blinking) - to be replaced by set measured by Felix
# seas <- "2020" #specific for the season - set measured by Emilia (which apparently has different way to annotate the blinking) - to be replaced by set measured by Felix
# seas <- "2021" #specific for the season
# seas <- "2022" #specific for the season
# seas <- "2023" #specific for the season


# Folder with data
path <- paste("./Working files/Projects/Eye-blinking/Raw data/eyeblinks_", seas, sep = "") # spec


# Proper loop for processing all the files in the saeson-folder
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
  
  
  #  output data frame
  df_blinks[[i]] <- data.frame(filename,  recording_duration, disturbance_duration, n_blinks)
  
}

df <- lapply(df_blinks, data.frame) %>%
  bind_rows()


# Add season, get ring id
df_rate <- df %>% 
  mutate(valid_duration_time = recording_duration - disturbance_duration,
         eye_rate = n_blinks/valid_duration_time,
         season = seas,
         ring = str_sub(string = filename, start = 1, end = 5))

# Save the temporary output
# rds_title <- paste("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate", seas, ".RDS", sep = "")
# saveRDS(df_rate, rds_title)


# Add sex and nest -------

# To be performed season by season; enter the manually


# Sx data
# sex() # generate the file (read the function from ./Metabases/scripts)
sx <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/SHO_LIAK_sex_meta_2024-04-02.RDS")



# Nest data - 2019 ----
capt19 <- read_excel("./Liak2019/FieldData/CapturingDt2019.xlsx") # data from outside/KWJ
capt19 <- capt19 %>% 
  select (Nest, RingNo) %>%
  mutate(RingNo = as.character(RingNo))

# Eye-rate data 2019
# df_rate2019 <- df_rate
df_rate2019 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2019.RDS") 

capt_sx <- left_join(capt19, sx, by = "RingNo")
capt_sx <- capt_sx %>% 
  distinct() %>% # some inds capt >1
  rename(ring = "RingNo") # to combined with dr_rate

# 2019: rings to fix (X/wrong no)
# 269X7 = 26937
# 29X21 = 29521
# 26985 = 26975
# 48025 = 46025
# 48087 = 43087
# 50156 = 50146
# 55892 = 44892

ringfix19 <- function(x) {
  switch(x,
         "269X7" = "26937",
         "29X21" = "29521",
         "26985" = "26975",
         "48025" = "46025",
         "48087" = "43087",
         "50156" = "50146",
         "55892" = "44892",
         "NA")
}; ringfix19 <- Vectorize(ringfix19)


df_rate2019 <- df_rate2019 %>% 
  mutate(ring_temp = ringfix19(ring),
         ring = if_else(ring_temp == "NA", ring, ring_temp)) %>% 
  select(-ring_temp)

df_rate2019 <- left_join(df_rate2019, capt_sx, by = "ring")

# Save final pre-processed data for 2019
saveRDS(df_rate2019, "./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2019.RDS") 



# Nest data - 2020 ----
capt20 <- read_excel("./Liak2020/FieldData/CapturingDt2020.xlsx") 
capt20 <- capt20 %>% 
  select (Nest, RingNo) %>%
  mutate(RingNo = as.character(RingNo))

# Eye-rate data 2020
# df_rate2020 <- df_rate
df_rate2020 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2020.RDS")

capt_sx <- left_join(capt20, sx, by = "RingNo")
capt_sx <- capt_sx %>% 
  distinct() %>% # some inds capt >1
  rename(ring = "RingNo") # to combined with dr_rate

# 2020: rings to fix (X/wrong no)

ringfix20 <- function(x) {
  switch(x,
         "50598" = "50498", # K17_15
         "55009" = "55509",  # K16_12 
         "NA")
}; ringfix20 <- Vectorize(ringfix20)


df_rate2020 <- df_rate2020 %>% 
  mutate(ring_temp = ringfix20(ring),
         ring = if_else(ring_temp == "NA", ring, ring_temp)) %>% 
  select(-ring_temp)

df_rate2020 <- left_join(df_rate2020, capt_sx, by = "ring")

# Save final pre-processed data for 2020
saveRDS(df_rate2020, "./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2020.RDS") 


# Nest data - 2021 ----
capt21 <- read_excel("./Liak2021/FieldData/CapturingDt2021.xlsx") 
capt21 <- capt21 %>% 
  select (Nest, RingNo) %>% 
  mutate(RingNo = as.character(RingNo))

# Eye-rate data 2021
# df_rate2021 <- df_rate
df_rate2021 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2021.RDS") 

capt_sx <- left_join(capt21, sx, by = "RingNo")
capt_sx <- capt_sx %>% 
  distinct() %>% # some inds capt >1
  rename(ring = "RingNo") # to combined with dr_rate

# 2021: rings to fix (X/wrong no) - apparently nothing here

df_rate2021 <- left_join(df_rate2021, capt_sx, by = "ring")

# Save final pre-processed data for 2021
saveRDS(df_rate2021, "./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2021.RDS") 



# Nest data - 2022 ----
capt22 <- read_excel("./Liak2022/FieldData/CapturingDt2022.xlsx") 
capt22 <- capt22 %>% 
  select (Nest, RingNo) %>% 
  mutate(RingNo = as.character(RingNo))

# Eye-rate data 2022
# df_rate2022 <- df_rate
df_rate2022 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2022.RDS") 

capt_sx <- left_join(capt22, sx, by = "RingNo")
capt_sx <- capt_sx %>% 
  distinct() %>% # some inds capt >1
  rename(ring = "RingNo") # to combined with dr_rate

# 2022: rings to fix (X/wrong no) - apparently nothing here

df_rate2022 <- left_join(df_rate2022, capt_sx, by = "ring")

# Save final pre-processed data for 2022
saveRDS(df_rate2022, "./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2022.RDS") 


# Nest data - 2023 ----
capt23 <- read_excel("./Liak2023/FieldData/CapturingDt2023.xlsx") 
capt23 <- capt23 %>% 
  select (Nest, RingNo) %>% 
  mutate(RingNo = as.character(RingNo))

# Eye-rate data 2023
# df_rate2023 <- df_rate
df_rate2023 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2023.RDS") 

capt_sx <- left_join(capt23, sx, by = "RingNo")
capt_sx <- capt_sx %>% 
  distinct() %>% # some inds capt >1
  rename(ring = "RingNo") # to combined with dr_rate

# 2023: rings to fix (X/wrong no) - apparently nothing here

df_rate2023 <- left_join(df_rate2023, capt_sx, by = "ring")

# Save final pre-processed data for 2023
saveRDS(df_rate2023, "./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2023.RDS") 





# Filtering data ----

# Much needed as there are some quite short recordings (arbitrary value set on 10 blinks)
df_rate2019 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2019.RDS")
df_rate2020 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2020.RDS")
df_rate2021 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2021.RDS")
df_rate2022 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2022.RDS")
df_rate2023 <- readRDS("./Working files/Projects/Eye-blinking/Preprocessed data/df_rate2023.RDS")

# Five seasons data but there are two observers (Emilia: 2019, 2020; Felix: 2021, 2022, 2023), and apparently there is an effect
all_data_5seas <- rbind(df_rate2019, df_rate2020, df_rate2021, df_rate2022, df_rate2023)
all_data_5seas_filtered <- all_data_5seas %>% # data filtering/see below
  filter(n_blinks >=10) %>% 
  mutate(observer = if_else(season %in% c("2019", "2020"), "Emilia", "Felix"))


# Observer effect

ggplot(all_data_5seas_filtered, aes(x = season, y = eye_rate, fill = observer)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_manual(values = c("tomato3", "steelblue4"))



# For now only the three sets are considerd (single observer - Felix)
all_data_3seas <- rbind(df_rate2021, df_rate2022, df_rate2023)

# Filtering (threshold: 10 blinks - established arbitrary)

all_data_3seas_filtered <- all_data_3seas %>% 
  filter(n_blinks >=10)

saveRDS(all_data_3seas_filtered, "./Working files/Projects/Eye-blinking/Preprocessed data/df_all3_filtered.RDS") 