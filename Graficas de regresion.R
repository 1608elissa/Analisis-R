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


#### INDICE DE EFICIENCIA ####
p1 <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p2 <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p3 <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p4 <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()


grid.arrange(p1,p2,p3,p4)

p5 <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p8 <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p6 <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p7 <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "EI") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

grid.arrange(p5,p6,p7,p8)

#### INDICE DE AMPLIFICACION ####
p10 <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "AMPLIFICACION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p9 <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "AMPLIFICACION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p12 <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "AMPLIFICACION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p11 <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "AMPLIFICACION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

grid.arrange(p9,p10,p11,p12)

#### INDICES DE SUPRESION ####
p14 <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "SUPRESION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p13 <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "SUPRESION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p16 <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "SUPRESION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p15 <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "SUPRESION") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()  
  
grid.arrange(p13,p14,p15,p16)
  
#### D PRIMA ####
p18 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p17 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p20 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p19 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()  

grid.arrange(p17,p18,p19,p20)

p21 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p22 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p24 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

p23 <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "D PRIMA") +  
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()  

grid.arrange(p21,p22,p23,p24)
