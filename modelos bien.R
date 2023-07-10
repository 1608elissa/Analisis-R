library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)

#### BASE DE DATOS CORRELACIONES ####
data <- read_xlsx("Junto.xlsx",sheet = "Correlaciones")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                     "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                     "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                     "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                     "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                     "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                     "OCUPACION","OCUPA_CAT","OCUPACION_CAT",
                                     "AMP_ROS","SUP_ROS")) %>%
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")

data %>%
  mutate(DECADA = as.factor(DECADA),
         SEXO = as.factor(SEXO),
         VD = as.factor(VD),
         COND = as.factor(COND),
         VAL = as.factor(VAL),
         TIPO = as.factor(TIPO),
         IDERE_R_CAT = as.factor(IDERE_R_CAT),
         IDARE_R_CAT = as.factor(IDARE_R_CAT),
         CRI_Total_CAT = as.factor(CRI_Total_CAT),
         EDAD_CAT = as.factor(EDAD_CAT),
         ESCOLARIDAD_CAT = as.factor(ESCOLARIDAD_CAT),
         MoCA_CAT = as.factor(MoCA_CAT))


#### INDICES DE AMPLIFICACION ####
e <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

f <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = e)

step(object = f, direction = "both", trace = 1)

modeloAMPDUR <- (lm(formula = value ~ ESCOLARIDAD + MoCA + ESCOLARIDAD:MoCA, data = e))
summary(modeloAMPDUR)

lm(formula = value ~ ESCOLARIDAD, data = e)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "ESCOLARIDAD", y = "EI") +  
  theme_classic()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "MoCA", y = "EI") +  
  theme_classic()

#### INDICES DE SUPRESION ####
g <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

h <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = g)

step(object = h, direction = "both", trace = 1)

modeloSUPDUR <- (lm(formula = value ~ EDAD + CRI_Total + EDAD:CRI_Total, data = g))
summary(modeloSUPDUR)

lm(formula = value ~ EDAD, data = g)%>%
  summary()

lm(formula = value ~ CRI_Total, data = g)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "Reserva cognitiva", y = "EI") +  
  theme_classic()

#### D PRIMA ####
m <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")

n <- lm(formula= value ~ EDAD+ESCOLARIDAD+MoCA+CRI_Total, data = m)

step(object = n, direction = "both", trace = 1)

modeloEIDPROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total, data = m))
summary(modeloEIDPROS)

lm(formula = value ~ EDAD, data = m)%>%
  summary()

lm(formula = value ~ MoCA, data = m)%>%
  summary()

lm(formula = value ~ CRI_Total, data = m)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "MoCA", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "Reserva cognitiva", y = "EI") +  
  theme_classic()

o <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

p <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = o)

step(object = p, direction = "both", trace = 1)

modeloEIDPESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = o))
summary(modeloEIDPESC)

lm(formula = value ~ EDAD, data = o)%>%
  summary()

lm(formula = value ~ ESCOLARIDAD, data = o)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "ESCOLARIDAD", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "MoCA", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="purple") +
  labs(x = "Reserva cognitiva", y = "EI") +  
  theme_classic()

