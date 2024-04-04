# Intra-individual repeatability 

# All data (for now three seasons), with some individuals measured in >1 seasons

# Basics/libs ----

rm(list = ls())
library(tidyverse)
library(readxl)
library(magrittr)

# Read data ----
df_all3_filtered  <- readRDS("./Preprocessed data/df_all3_filtered.RDS") 


# Prepare data -----

# list of repeated inds
n_rep_ind <- df_all3_filtered %>% 
  group_by(ring, Sx) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# n inds measured >1 (a summary table)
n_rep_ind %>% 
  group_by(n) %>% 
  summarise(N_individuals = n()) %>% 
  rename(N_measurments = "n")

# select inds for the analysis (sampled >1)
df_rate_rep_sel <- df_all3_filtered %>% 
  filter(ring %in% n_rep_ind$ring) 

# Model ----
library(rptR)
rpt_eye <- rpt(eye_rate ~ 1 + (1 | ring), 
                 data = df_rate_rep_sel,
                 grname = "ring", nboot = 100,
                 npermut = 100, datatype = "Gaussian")



# Plot
ggplot(data = df_rate_rep_sel, 
       aes(x = as.character(ring), y = eye_rate, col = season)) + 
  geom_line(aes(group = ring), col = "black") +
  geom_point() + 
  geom_hline(aes(yintercept = mean(eye_rate)), linetype = "dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Bird ID", y = "Eye blinking rate [n/sec]", col = "Year",
       title = "Repeatability (inter-annual/within-individual difference)")


