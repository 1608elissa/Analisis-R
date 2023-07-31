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

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = EDAD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()


#### EDAD ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

#### ESCOLARIDAD ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "ESCOLARIDAD", y = "AMPLIFICACION") +  
  theme_classic()

#### CRI_Total ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "CRI_Total", y = "EI") +  
  theme_classic()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

#### IDARE_R_PUNTAJE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

#### IDERE_R_PUNTAJE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = IDERE_R_PUNTAJE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "IDERE_R_PUNTAJE", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

#### MoCA ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "MoCA", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "MoCA", y = "EI") +  
  theme_classic()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

#### SUEÑO_NOR ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

#### SUEÑO_2DA ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

#### OCUPA_CAT ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = OCUPA_CAT, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "OCUPA_CAT", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = OCUPA_CAT, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "OCUPA_CAT", y = "EI") +  
  theme_classic()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

#### COVID_CAT ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()








