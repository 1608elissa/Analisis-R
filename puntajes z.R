library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(psych)
library(magrittr)

#### BASE DE DATOS ####
data <- read_xlsx("Junto.xlsx",sheet = "PuntajeZ")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", 
                                     "ESCOLARIDAD", "CRI_Total","IDARE_R_PUNTAJE",  
                                     "IDARE_E_PUNTAJE","IDERE_R_PUNTAJE",
                                     "IDERE_E_PUNTAJE","COVID_CAT", "SHIPLEY",
                                     "SUEÑO_NOR","SUEÑO_2DA","OCUPA_CAT",
                                     "AMP_ROS","SUP_ROS")) %>%
  separate(cond_tip_est_val, c("VAR","TIPO", "Z"), sep = "_")


data %>%
  mutate(DECADA = as.factor(DECADA),
         SEXO = as.factor(SEXO),
         VAR = as.factor(VAR),
         TIPO = as.factor(TIPO))


##### ANOVAS #####
data1 <- read_xlsx("Junto.xlsx",sheet = "anovas")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT",
                                     "MoCA","MoCA_Z","MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z",
                                     "ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                     "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE",
                                     "IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE","IDERE_R_Z","IDERE_R_CAT",
                                     "IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY",
                                     "SHIPLEY_Z","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT",
                                     "SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT","OCUPACION","OCUPAC_CAT",
                                     "OCUPA_Z","OCUPA_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(cond_tip_est_val, c("VD","COND","TIPO"), sep = "_")

data1$DECADA <- as.factor(data1$DECADA)
data1$SEXO <- as.factor(data1$SEXO)
data1$COND <- as.factor(data1$COND)
data1$VD <- as.factor(data1$VD)
data1$TIPO <- as.factor(data1$TIPO)


filter(data1, COND== "ROSTROS", TIPO== "TOT") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data1, COND== "ROSTROS", TIPO== "TOT") %>%
  aov(value ~ DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, COND== "ESCENAS", TIPO== "TOT") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data1, COND== "ESCENAS", TIPO== "TOT") %>%
  aov(value ~ DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, COND== "ROSTROS", TIPO== "Z") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data1, COND== "ROSTROS", TIPO== "Z") %>%
  aov(value ~ DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, COND== "ESCENAS", TIPO== "Z") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data1, COND== "ESCENAS", TIPO== "Z") %>%
  aov(value ~ DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, TIPO== "Z") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  summary()

filter(data1, TIPO== "Z") %>%
  aov(value~COND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()
	
data1 <- read_xlsx("Junto.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                 "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                 "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                 "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_CAT","SUEÑO_2DA",
                                 "SUEÑO_2DA_CAT","OCUPACION","OCUPAC_CAT","OCUPA_CAT")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL","TIPO"),
           sep = "_")

data1$DECADA <- as.factor(data1$DECADA)
data1$DPRIMA <- as.factor(data1$DPRIMA)
data1$COND <- as.factor(data1$COND)
data1$VAL <- as.factor(data1$VAL)
data1$TIPO <- as.factor(data1$TIPO)

filter(data1, DPRIMA== "EIDP", !VAL=="TOT", !TIPO=="Z") %>%
  aov(value ~ DECADA*COND*VAL, data=.)%>%
  summary()

filter(data1, DPRIMA== "EIDP", !VAL=="TOT", !TIPO=="Z") %>%
  aov(value~ DECADA*COND*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### REGRESIONES SIMPLES ####

## ATT CON WM ##
filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()

## VARIABLES ##
filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ ESCOLARIDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ ESCOLARIDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ ESCOLARIDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ CRI_Total, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ CRI_Total, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ CRI_Total, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDARE_R_PUNTAJE, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDARE_R_PUNTAJE, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDARE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDARE_E_PUNTAJE, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDARE_E_PUNTAJE, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDARE_E_PUNTAJE, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDERE_R_PUNTAJE, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDERE_R_PUNTAJE, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDERE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_E_PUNTAJE, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDERE_E_PUNTAJE, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDERE_E_PUNTAJE, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ MoCA, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ MoCA, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ MoCA, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_NOR, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ SUEÑO_NOR, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ SUEÑO_NOR, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_2DA, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ SUEÑO_2DA, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ SUEÑO_2DA, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ OCUPA_CAT, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ OCUPA_CAT, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ OCUPA_CAT, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ COVID_CAT, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ COVID_CAT, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ COVID_CAT, data = data)%>%
  summary()

#### MODELOS WM, ATT Y VARIABLES ####

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*ESCOLARIDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*ESCOLARIDAD, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*CRI_Total, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*CRI_Total, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*IDARE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*IDARE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*IDERE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*IDERE_E_PUNTAJE, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*MoCA, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*MoCA, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*SUEÑO_NOR, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*SUEÑO_NOR, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*SUEÑO_2DA, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*SUEÑO_2DA, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*OCUPA_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*OCUPA_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*SHIPLEY, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*SHIPLEY, data =.)%>%
  summary()


#### MODELOS WM, ATT CON EDAD ####

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ ESCOLARIDAD*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ ESCOLARIDAD*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ ESCOLARIDAD*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ ESCOLARIDAD*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ CRI_Total*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ CRI_Total*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ CRI_Total*EDAD, data = data)%>%
  summary()

filter(data2, !CRI_Total_CAT=="BAJO") %$%
  lm(formula= AMP_ROS ~ CRI_Total*EDAD, data = .)%>%
  summary()

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  lm(formula= AMP_ROS ~ CRI_Total*EDAD, data = .)%>%
  summary()

lm(formula= SUP_ROS ~ CRI_Total*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDARE_R_PUNTAJE*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDARE_R_PUNTAJE*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDARE_R_PUNTAJE*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDARE_E_PUNTAJE*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDARE_E_PUNTAJE*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDARE_E_PUNTAJE*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDARE_E_PUNTAJE*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_R_PUNTAJE*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDERE_R_PUNTAJE*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDERE_R_PUNTAJE*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ IDERE_E_PUNTAJE*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_E_PUNTAJE*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ IDERE_E_PUNTAJE*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ IDERE_E_PUNTAJE*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ MoCA*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ MoCA*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ MoCA*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ MoCA*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_NOR*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_NOR*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ SUEÑO_NOR*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ SUEÑO_NOR*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑO_2DA*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUEÑO_2DA*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ SUEÑO_2DA*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ SUEÑO_2DA*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ OCUPA_CAT*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ OCUPA_CAT*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ OCUPA_CAT*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ OCUPA_CAT*EDAD, data = data)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SHIPLEY*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SHIPLEY*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ SHIPLEY*EDAD, data = data)%>%
  summary()

lm(formula= SUP_ROS ~ SHIPLEY*EDAD, data = data)%>%
  summary()

#### ANALISIS CON GRUPOS ####
data2 <- read_xlsx("Junto.xlsx",sheet = "Regresiones")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT",
                                  "CRI_Total","CRI_Total_Z","CRI_Total_CAT","IDARE_R_PUNTAJE",
                                  "IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z",
                                  "IDARE_E_CAT","IDERE_R_PUNTAJE","IDERE_R_Z","IDERE_R_CAT",
                                  "IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT",
                                  "SHIPLEY","SHIPLEY_Z","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z",
                                  "SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPAC_CAT","OCUPA_Z","OCUPA_CAT","AMP_ROS",
                                  "AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

data2$DECADA <- as.factor(data2$DECADA)
data2$VAR <- as.factor(data2$VAR)
data2$COND <- as.factor(data2$COND)
data2$TIPO <- as.factor(data2$TIPO)

data3 <- read_xlsx("Junto.xlsx",sheet = "Multiplicacion")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT",
                                  "CRI_Total","CRI_Total_Z","CRI_Total_CAT","IDARE_R_PUNTAJE",
                                  "IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z",
                                  "IDARE_E_CAT","IDERE_R_PUNTAJE","IDERE_R_Z","IDERE_R_CAT",
                                  "IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT",
                                  "SHIPLEY","SHIPLEY_Z","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z",
                                  "SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPAC_CAT","OCUPA_Z","OCUPA_CAT","AMP_ROS",
                                  "AMP_ROS_Z","SUP_ROS","SUP_ROS_Z", 
                                  "CRIq_SUP","IDEREE_AGE","SHIPLEY_AGE","CRIq_AGE","SUEÑOSES_AGE","SUEÑONOR_AGE")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

### ANALISIS DE VARIABLES CON ESCENAS/SUPRESION Y ROSTROS/AMPLIFICACION

### AMPLIFICACION - ROSTROS 
filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)
# U= 2107, p=9.336e-07

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4151, p=0.01913

filter(data2, COND== "ROSTROS", TIPO== "Z", !OCUPA_CAT=="MEDIO") %$%
  wilcox.test(value ~ OCUPA_CAT, data =.)
# U= 1849, p=0.06951


### SUPRESION - ESCENAS 
filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)
# U= 2064, p=1.102e-05

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)
# U= 1819, p=0.02434

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 3982, p=0.00443

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  aov(value ~ SUP_ROS*CRI_Total_CAT, data =.)%>%
  summary()
# F= 8.606, p=0.00415 

filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="LOW") %$%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()
# R=0.07279, B= -0.0007933, p=0.0273 
filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="HIGH") %$%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()
# R=0.0455, B= 0.0005078, p=0.0719


### ANALISIS DE VARIABLES CON EDAD

### ROSTROS
filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)
# U= 1669, p=0.06163

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4151, p=0.01278

filter(data2, COND== "ROSTROS", TIPO== "Z", !SUEÑO_NOR_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*SUEÑO_NOR_CAT, data =.)%>%
  summary()
# F= 0.483 , p=0.488 

### ESCENAS
filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(value ~ IDARE_E_CAT, data =.)
# U= 1541, p=0.05724

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 3982, p=0.006003

filter(data2, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*IDERE_E_CAT, data =.)%>%
  summary()
# F= 8.475, p=0.00426  

filter(data2, COND== "ESCENAS", TIPO== "Z", IDERE_E_CAT=="HIGH") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()
# R=0.3538, B= 0.74533, p=1.64e-07 

filter(data2, COND== "ESCENAS", TIPO== "Z", IDERE_E_CAT=="LOW") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()
# R=0.1192 , B= 0.29350, p=0.00264


### AMPLIFICACION
filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ ESCOLARIDAD_CAT, data =.)
# U= 42256, p=4.663e-06

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ CRI_Total_CAT, data =.)
# U= 23680, p=0.1851

filter(data2, !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ IDARE_E_CAT, data =.)
# U= 36512, p=0.0003077

filter(data2, !IDERE_R_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ IDERE_R_CAT, data =.)
# U= 46496, p=4.556e-10

filter(data2, !OCUPA_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ OCUPA_CAT, data =.)
# U= 43088, p=0.0001428

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(AMP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()
# F= 14.021, p=0.000206 

filter(data2, CRI_Total_CAT=="LOW") %$%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
# R= -0.004673, B= -0.0005463, p=0.995

filter(data2, CRI_Total_CAT=="HIGH") %$%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
# R=0.09905, B= -0.47338, p=2.71e-06


### SUPRESION
filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()
# F= 31.892, p=3.02e-08 

filter(data2, CRI_Total_CAT=="LOW") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
# R= 0.0882, B= -0.34704, p=5.35e-06
filter(data2, CRI_Total_CAT=="HIGH") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
# R= 0.05386, B= 0.4154, p=0.000491


#### DESCRIPTIVOS DE LA MUESTRA #####
data$DECADA <- as.factor(data$DECADA)

filter(data, TIPO=="ROSTROS")%>%
  aov(value ~ DECADA, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  aov(value ~ DECADA, data =.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, TIPO=="ESCENAS")%>%
  aov(value ~ DECADA, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  aov(value ~ DECADA, data =.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

aov(AMP_ROS ~ DECADA, data = data)%>%
  summary()

aov(AMP_ROS ~ DECADA, data = data)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

aov(SUP_ROS ~ DECADA, data = data)%>%
  summary()

aov(SUP_ROS ~ DECADA, data = data)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


filter(data1, COND== "ROSTROS", TIPO== "TOT") %$%
  describeBy(value, DECADA)
filter(data1, COND== "ROSTROS", TIPO== "Z") %$%
  describeBy(value, DECADA)

filter(data1, COND== "ESCENAS", TIPO== "TOT") %$%
  describeBy(value, DECADA)
filter(data1, COND== "ESCENAS", TIPO== "Z") %$%
  describeBy(value, DECADA)

filter(data1) %$%
  describeBy(AMP_ROS, DECADA)
filter(data1) %$%
  describeBy(AMP_ROS_Z, DECADA)

filter(data1) %$%
  describeBy(SUP_ROS, DECADA)
filter(data1) %$%
  describeBy(SUP_ROS_Z, DECADA)


#### RESULTADOS MEDIAS POR GRUPOS ####

### AMPLIFICACION - ROSTROS
filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  describeBy(value, EDAD_CAT)
#Alto: 0.62 (SD=1.15)
#Bajo: -0.41 (SD=0.87)

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.2 (SD=0.8)
#Bajo: 0.17 (SD=1.12)

filter(data2, COND== "ROSTROS", TIPO== "Z", !OCUPA_CAT=="MEDIO") %$%
  describeBy(value, OCUPA_CAT)
#Alto: -0.6 (SD=0.85)
#Bajo: 0.3 (SD=1.13)



### SUPRESION - ESCENAS 
filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  describeBy(value, EDAD_CAT)
#Alto: 0.46 (SD=1.07)
#Bajo: -0.41 (SD=1.05)

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(value, CRI_Total_CAT)
#Alto: 0.23 (SD=0.92)
#Bajo: -0.24 (SD=0.98)

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.24 (SD=0.84)
#Bajo: 0.21 (SD=1.08)

filter(data3, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_SUP, CRI_Total_CAT)
#Alto: 0.01 (SD=1.76)
#Bajo: -0.08 (SD=1.11)


### ROSTROS Y EDAD      
filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(value, CRI_Total_CAT)
#Alto: 0.12 (SD=0.86)
#Bajo: -0.2 (SD=0.93)

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.2 (SD=0.8)
#Bajo: 0.17 (SD=1.12)

filter(data3, COND== "ROSTROS", TIPO== "Z", !SUEÑO_NOR_CAT=="MEDIO") %$%
  describeBy(SUEÑONOR_AGE, SUEÑO_NOR_CAT)
#Alto: -0.06 (SD=0.88)
#Bajo: -0.01 (SD=1.3)



### ESCENAS Y EDAD
filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %$%
  describeBy(value, IDARE_E_CAT)
#Alto: -0.2 (SD=0.99)
#Bajo: 0.15 (SD=1.04)

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.24 (SD=0.84)
#Bajo: 0.21 (SD=1.08)

filter(data3, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO") %$%
  describeBy(IDEREE_AGE, IDERE_E_CAT)
#Alto: -0.2 (SD=1.25)
#Bajo: -0.25 (SD=1.32)



### AMPLIFICACION Y EDAD
filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, ESCOLARIDAD_CAT)
#Alto: 0.1 (SD=1.19)
#Bajo: -0.45 (SD=0.88)

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, CRI_Total_CAT)
#Alto: 0.14 (SD=0.96)
#Bajo: -0.22 (SD=0.93)

filter(data2, !IDARE_E_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, IDARE_E_CAT)
#Alto: 0.05 (SD=0.99)
#Bajo: -0.24 (SD=1.01)

filter(data2, !IDERE_R_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, IDERE_R_CAT)
#Alto: 0.26 (SD=0.93)
#Bajo: -0.31 (SD=1)

filter(data2, !OCUPA_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, OCUPA_CAT)
#Alto: 0.22 (SD=1.04)
#Bajo: -0.18 (SD=0.92)

filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_AGE, CRI_Total_CAT)
#Alto: 1.45 (SD=1.25)
#Bajo: 0.88 (SD=0.89)



### SUPRESION Y EDAD
filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_AGE, CRI_Total_CAT)
#Alto: 1.45 (SD=1.25)
#Bajo: 0.88 (SD=0.89)



#### MEDIAS DE CUESTIONARIOS POR GRUPO ####

filter(data3, !EDAD_CAT=="MEDIO") %$%
  describeBy(EDAD, EDAD_CAT)
#Alto: 65.36 (SD=4.78)
#Bajo: 24.2 (SD=2.5)

filter(data3, !EDAD_CAT=="MEDIO") %$%
  describeBy(EDAD_Z, EDAD_CAT)
#Alto: 1.47 (SD=0.32)
#Bajo: -1.3 (SD=0.17)



filter(data3, !MoCA_CAT=="MEDIO") %$%
  describeBy(MoCA, MoCA_CAT)
#Alto: 28.51 (SD=0.65)
#Bajo: 26.54 (SD=0.5)

filter(data3, !MoCA_CAT=="MEDIO") %$%
  describeBy(MoCA_Z, MoCA_CAT)
#Alto: 0.93 (SD=0.57)
#Bajo: -0.8 (SD=0.44)



filter(data3, !ESCOLARIDAD_CAT=="MEDIO") %$%
  describeBy(ESCOLARIDAD, ESCOLARIDAD_CAT)
#Alto: 91.94 (SD=2.24)
#Bajo: 12.02 (SD=1.57)

filter(data3, !ESCOLARIDAD_CAT=="MEDIO") %$%
  describeBy(ESCOLARIDAD_Z, ESCOLARIDAD_CAT)
#Alto: 1.33 (SD=0.68)
#Bajo: -1.69 (SD=0.48)



filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRI_Total, CRI_Total_CAT)
#Alto: 143.94 (SD=9.23)
#Bajo: 92.81 (SD=2.37)

filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRI_Total_Z, CRI_Total_CAT)
#Alto: 1.71 (SD=0.53)
#Bajo: -1.2 (SD=0.13)




filter(data3, !IDARE_R_CAT=="MEDIO") %$%
  describeBy(IDARE_R_PUNTAJE, IDARE_R_CAT)
#Alto: 52.81 (SD=2.99)
#Bajo: 40.43 (SD=1.94)

filter(data3, !IDARE_R_CAT=="MEDIO") %$%
  describeBy(IDARE_R_Z, IDARE_R_CAT)
#Alto: 1.32 (SD=0.6)
#Bajo: -1.17 (SD=0.39)



filter(data3, !IDARE_E_CAT=="MEDIO") %$%
  describeBy(IDARE_E_PUNTAJE, IDARE_E_CAT)
#Alto: 48.56 (SD=2.49)
#Bajo: 37.55 (SD=2.47)

filter(data3, !IDARE_E_CAT=="MEDIO") %$%
  describeBy(IDARE_E_Z, IDARE_E_CAT)
#Alto: 1.16 (SD=0.52)
#Bajo: -1.13 (SD=0.51)



filter(data3, !IDERE_R_CAT=="MEDIO") %$%
  describeBy(IDERE_R_PUNTAJE, IDERE_R_CAT)
#Alto: 50.28 (SD=3.22)
#Bajo: 40.4 (SD=3.07)

filter(data3, !IDERE_R_CAT=="MEDIO") %$%
  describeBy(IDERE_R_Z, IDERE_R_CAT)
#Alto: 0.93 (SD=0.68)
#Bajo: -1.14 (SD=0.64)



filter(data3, !IDERE_E_CAT=="MEDIO") %$%
  describeBy(IDERE_E_PUNTAJE, IDERE_E_CAT)
#Alto: 50.23 (SD=2.46)
#Bajo: 39.53 (SD=2.34)

filter(data3, !IDERE_E_CAT=="MEDIO") %$%
  describeBy(IDERE_E_Z, IDERE_E_CAT)
#Alto: 1.13 (SD=0.55)
#Bajo: -1.25 (SD=0.52)



filter(data3, !SHIPLEY_CAT=="MEDIO") %$%
  describeBy(SHIPLEY, SHIPLEY_CAT)
#Alto: 115.69 (SD=2.16)
#Bajo: 100.24 (SD=3.8)

filter(data3, !SHIPLEY_CAT=="MEDIO") %$%
  describeBy(SHIPLEY_Z, SHIPLEY_CAT)
#Alto: 1.18 (SD=1.31)
#Bajo: -1.62 (SD=0.69)



filter(data3, !SUEÑO_NOR_CAT=="MEDIO") %$%
  describeBy(SUEÑO_NOR, SUEÑO_NOR_CAT)
#Alto: 5.62 (SD=0.69)
#Bajo: 7.4 (SD=0.61)

filter(data3, !SUEÑO_NOR_CAT=="MEDIO") %$%
  describeBy(SUEÑO_NOR_Z, SUEÑO_NOR_CAT)
#Alto: -1.12 (SD=0.69)
#Bajo: 0.66 (SD=0.6)



filter(data3, !SUEÑO_2DA_CAT=="MEDIO") %$%
  describeBy(SUEÑO_2DA, SUEÑO_2DA_CAT)
#Alto: 5.6 (SD=0.58)
#Bajo: 8.22 (SD=0.52)

filter(data3, !SUEÑO_2DA_CAT=="MEDIO") %$%
  describeBy(SUEÑO_2DA_Z, SUEÑO_2DA_CAT)
#Alto: -1.14 (SD=0.53)
#Bajo: 1.29 (SD=0.49)



filter(data3, !OCUPACION_CAT=="MEDIO") %$%
  describeBy(OCUPA_CAT, OCUPACION_CAT)
#Alto: 4 (4-5)
#Bajo: 2 (0-2)

filter(data3, !OCUPACION_CAT=="MEDIO") %$%
  describeBy(OCUPA_Z, OCUPACION_CAT)
#Alto: 0.95 (SD=0.31)
#Bajo: -1.31 (SD=0.66)


