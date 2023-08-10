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

#### ESCOLARIDAD ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * ESCOLARIDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", !ESCOLARIDAD_CAT="MEDIO")%>%
  ggplot(aes(x = EDAD, y = value, colour= ESCOLARIDAD_CAT )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI rostros") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", ESCOLARIDAD_CAT=="BAJO")%>%
  lm(formula= value ~ EDAD , data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", ESCOLARIDAD_CAT=="ALTO")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * ESCOLARIDAD,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * ESCOLARIDAD, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * ESCOLARIDAD, data =.)%>%
  summary()

#### MOCA ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * MoCA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * MoCA,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * MoCA, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * MoCA, data =.)%>%
  summary()

#### CRI_Total ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * CRI_Total,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * CRI_Total, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * CRI_Total, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  ggplot(aes(x = EDAD, y = value, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "AMPLIFICACION") +  
  theme_classic()

#### OCUPA_CAT ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * OCUPA_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * OCUPA_CAT,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * OCUPA_CAT, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * OCUPA_CAT, data =.)%>%
  summary()

#### IDARE_R_PUNTAJE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * IDARE_R_PUNTAJE,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * IDARE_R_PUNTAJE, data =.)%>%
  summary()

#### IDERE_R_PUNTAJE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * IDERE_R_PUNTAJE,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * IDERE_R_PUNTAJE, data =.)%>%
  summary()

#### SUEÑO HABITUAL ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_NOR, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * SUEÑO_NOR,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_NOR, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_NOR, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value, colour= SUEÑO_NOR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = SUEÑO_NOR, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUEÑO_NOR", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = SUEÑO_NOR, y = EDAD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUEÑO_NOR", y = "EDAD") +  
  theme_classic()


filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", EDAD_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", EDAD_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

#### SUEÑO SEGUNDA SESION ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_2DA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * SUEÑO_2DA,  data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_2DA, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * SUEÑO_2DA, data =.)%>%
  summary()

#### COVID ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD * COVID_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", COVID_CAT="1")%>%
  ggplot(aes(x = EDAD, y = value, colour= COVID_CAT )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI rostros") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD * COVID_CAT,  data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", COVID_CAT="3")%>%
  ggplot(aes(x = EDAD, y = value, colour= COVID_CAT )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI rostros") +  
  theme_classic()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * COVID_CAT, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")%>%
  lm(formula= value ~ EDAD * COVID_CAT, data =.)%>%
  summary()

