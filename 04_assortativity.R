# Assortativity ----

# All data (for now three seasons), with some individuals measured in >1 seasons

# Basics/libs ----

rm(list = ls())
library(tidyverse)
library(readxl)
library(magrittr)

# Read data ----
df_all3_filtered  <- readRDS("./Preprocessed data/df_all3_filtered.RDS") 


# Prepare data -----

# list of complete nest (m+f = 2) 
compl_nests <- df_all3_filtered %>% 
  group_by(season, Nest, Sx) %>% 
  summarise(n = n()) %>% 
  group_by(Nest) %>% 
  summarise(n = n()) %>% 
  filter(n == 2)

compl_nests <- compl_nests$Nest

# select data
df_rate_assortativity <- df_all3_filtered %>% 
  filter(Nest %in% compl_nests,
         !is.na(Sx)) %>% 
  select(Nest, Sx, eye_rate) %>% 
  group_by(Nest, Sx) %>% 
  summarise(mean_eye_rate = mean(eye_rate, na.rm = TRUE)) %>% # for inds measured >1
  ungroup() 

df_rate_assortativity <- pivot_wider(data = df_rate_assortativity, 
                                     names_from = Sx, 
                                     values_from = mean_eye_rate)

saveRDS(df_rate_assortativity, "./Preprocessed data/df_rate_assortativity.RDS")

# Analysis ---

df_rate_assortativity <- readRDS("./Preprocessed data/df_rate_assortativity.RDS")

# Plot
ggplot(df_rate_assortativity, aes(x = f, y = m)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "females", y = "males", title = "Eye blinking rate in breeding partners [n blinks/sec]")


xobs <- cor(df_rate_assortativity$f, df_rate_assortativity$m)
N <- 1000
xcor <- numeric(N)

for(i in 1:N) {
  xf <- sample(df_rate_assortativity$f, length(df_rate_assortativity$f), replace = FALSE)
  xm <- sample(df_rate_assortativity$m, length(df_rate_assortativity$m), replace = FALSE)
  xcor[i] <- cor(xf, xm)
  
}

1-sum(xcor<= xobs)/N

xcor <- data.frame(xcor)

ggplot(xcor, aes(xcor)) + 
  geom_density(fill = "grey") +
  geom_vline(aes(xintercept = xobs)) + 
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))




