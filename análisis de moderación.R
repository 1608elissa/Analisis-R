library(readxl)
library(rstatix)
library(tidyverse)
library(moderndive)


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

data$DECADA <- as.factor(data$DECADA)
data$SEXO <- as.factor(data$SEXO)
data$VD <- as.factor(data$VD)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

#### AMPLIFICACION ANALISIS CON ATENDER ESCENAS ####
### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * EDAD_CAT, data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * ESCOLARIDAD_CAT, data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * MoCA_CAT, data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * CRI_Total_CAT, data =.)%>%
  summary()

### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * IDARE_R_CAT, data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ AMP_ROS * IDERE_R_CAT, data =.)%>%
  summary()

#### AMPLIFICACION ANALISIS CON ATENDER ROSTROS ####
### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * EDAD_CAT, data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * ESCOLARIDAD_CAT, data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * MoCA_CAT, data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * CRI_Total_CAT, data =.)%>%
  summary()

### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * IDARE_R_CAT, data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * IDERE_R_CAT, data =.)%>%
  summary()

#### SUPRESION ANALISIS CON ATENDER ESCENAS ####
### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * EDAD_CAT, data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * ESCOLARIDAD_CAT, data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * MoCA_CAT, data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * CRI_Total_CAT, data =.)%>%
  summary()

### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * IDARE_R_CAT, data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * IDERE_R_CAT, data =.)%>%
  summary()

#### SUPRESION ANALISIS CON ATENDER ROSTROS ####
### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS * EDAD_CAT, data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS * ESCOLARIDAD_CAT, data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS * MoCA_CAT, data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS * CRI_Total_CAT, data =.)%>%
  summary()

### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS * IDARE_R_CAT, data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, IDERE_R_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
 lm(formula= value ~ SUP_ROS , data =.) -> alto


filter(data, IDERE_R_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> bajo

stargazer::stargazer(alto, bajo, type="text", df=FALSE, column.labels=c("Alto","Bajo"))
rm(alto,bajo)

#### GRAFICAS PARA AMPLIFICACION ROSTROS ####
data %>%
  mutate(IDERE_R_CAT = as.factor(IDERE_R_CAT),
         IDARE_R_CAT = as.factor(IDARE_R_CAT),
         CRI_Total_CAT = as.factor(CRI_Total_CAT),
         EDAD_CAT = as.factor(EDAD_CAT),
         ESCOLARIDAD_CAT = as.factor(ESCOLARIDAD_CAT),
         MoCA_CAT = as.factor(MoCA_CAT))

filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = IDERE_R_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()
