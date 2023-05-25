library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)

#### BASE DE DATOS CORRELACIONES ####
data <- read_xlsx("Junto.xlsx",sheet = "Correlaciones")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                     "CRI_Total", "IDARE_R_PUNTAJE", "IDERE_R_PUNTAJE",
                                     "COVID_CAT", "SUEÑO_NOR", "AMP_DUR_ROS", "AMP_DUR_ESC",
                                     "SUP_DUR_ROS", "SUP_DUR_ESC", "AMP_1DU_ROS",
                                     "AMP_1DU_ESC", "SUP_1DU_ROS", "SUP_1DU_ESC")) %>%
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")

data$DECADA <- as.factor(data$DECADA)
data$SEXO <- as.factor(data$SEXO)
data$VD <- as.factor(data$VD)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)





filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "EI") +  
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()



filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()




filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()




filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()
