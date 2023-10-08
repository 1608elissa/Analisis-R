library(readxl)
library(rstatix)
library(tidyverse)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(rcartocolor)
library(ez)
source("summarySE.R")


data <- read_xlsx("Analisis Antonio/Analisis Antonio.xlsx",sheet = "RC")%>%
  gather(cond_tip_est_val, value, -c("ID","SEXO")) %>%
  separate(cond_tip_est_val, c("COND","VAR"), sep = "_")

data1 <- read_xlsx("Analisis Antonio/Analisis Antonio.xlsx",sheet = "Amplitud")%>%
  gather(tip_cond, value, -c("ID")) %>%
  separate(tip_cond, c("TIPO","COND"), sep = "_")

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

filter(data, VAR=="EI") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_carto_d(palette = "Pastel") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="Inverse Efficiency Index") + 
  guides(fill = guide_legend(title = "Condition:"))



filter(data, VAR=="ACC") %>%
  ezANOVA(data=., dv=value, wid =ID, within=COND)

filter(data, VAR=="RT") %>%
  ezANOVA(data=., dv=value, wid =ID, within=COND)

filter(data, VAR=="EI") %>%
  ezANOVA(data=., dv=value, wid =ID, within=COND)



filter(data1, TIPO=="PD") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel1") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="Pd Amplitude") + 
  guides(fill = guide_legend(title = "Condition:"))


filter(data1, TIPO=="N2pcP") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel2") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="N2pc Amplitude (P7/P8)") + 
  guides(fill = guide_legend(title = "Condition:"))


filter(data1, TIPO=="N2pcO") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  theme_classic() +
  scale_fill_carto_d(palette = "Pastel") +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge") +
  labs(x= "CONDITION", y="N2pc Amplitude (O1/02)") + 
  guides(fill = guide_legend(title = "Condition:"))




