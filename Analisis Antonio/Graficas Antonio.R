library(readxl)
library(rstatix)
library(tidyverse)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(rcartocolor)
source("summarySE.R")


data <- read_xlsx("Analisis Antonio/Analisis Antonio.xlsx",sheet = "RC")%>%
  gather(cond_tip_est_val, value, -c("ID","SEXO", "Edad")) %>%
  separate(cond_tip_est_val, c("COND","VAR","RESP"), sep = "_")

filter(data, VAR=="ACC") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel1") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="Correct Answers (%)") + 
  guides(fill = guide_legend(title = "Condition:"))

filter(data, VAR=="RT") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel2") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="Reaction Times (ms)") + 
  guides(fill = guide_legend(title = "Condition:"))

filter(data, VAR=="IE") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_carto_d(palette = "Pastel") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="Inverse Efficiency Index") + 
  guides(fill = guide_legend(title = "Condition:"))


