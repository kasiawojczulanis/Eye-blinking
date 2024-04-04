# Hormones 

# All data (for now three seasons), with some individuals measured in >1 seasons

# Basics/libs ----

rm(list = ls())
library(tidyverse)
library(readxl)
library(magrittr)

# Read data ----

horms <- readxl::read_excel("C:/Users/User/Dropbox/Metabases/Hornsund/SHO_LIAK_hormones.xlsx")
# 2019: incubation
# 2020, 2021: early-chick rearing

# eye rate
df_rate2019  <- readRDS("./Preprocessed data/df_rate2019.RDS") 

# To be fixed (hormones data still missing)
# df_rate2020  <- readRDS("./Preprocessed data/df_rate2020.RDS") 
# df_rate2021  <- readRDS("./Preprocessed data/df_rate2021.RDS") 

# incubation set (2019)
horms_inc <- horms %>% 
  filter(Season == 2019,
         Type == "baseline",
         Hormone == "cort") %>% 
  mutate(ring = as.character(BirdID)) 

horms_inc_blinks <- left_join(df_rate2019, horms_inc, by = "ring")

# Analysis ----

# Plot
ggplot(horms_inc_blinks, aes(x = Concentration, y = eye_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "hormone concentration", 
       y = "eye blinking rate [n/sec]",
       title = "Baseline corticosterone and eye-blinking rate during the incubation period")

# Test/bootstrap
horm_cor <- cor(horms_inc_blinks$eye_rate, horms_inc_blinks$Concentration)

N <- 1000
xcor <- numeric(N)

for(i in 1:N) {
  boot_df <- sample_n(horms_inc_blinks, nrow(horms_inc_blinks), replace = TRUE)
  boot_df <- boot_df %>% 
    filter(!is.na(Concentration))
  
  xcor[i] <- cor(boot_df$eye_rate, boot_df$Concentration)
  
}

1-sum(xcor<= xobs)/N

xcor <- data.frame(xcor)

ggplot(xcor, aes(xcor)) + 
  geom_density(fill = "grey") +
  geom_vline(aes(xintercept = xobs)) +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "cort-eye_rate correlation/bootstrapped")
