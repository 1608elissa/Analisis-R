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
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")

data %>%
  mutate(DECADA = as.factor(DECADA),
         SEXO = as.factor(SEXO),
         VD = as.factor(VD),
         COND = as.factor(COND),
         VAL = as.factor(VAL),
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
                                     "SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT","OCUPACION","OCUPA_CAT","OCUPA_Z",	
                                     "OCUPACION_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(cond_tip_est_val, c("VD","COND","TIPO"), sep = "_")


data1$DECADA <- as.factor(data1$DECADA)
data1$SEXO <- as.factor(data1$SEXO)
data1$COND <- as.factor(data1$COND)
data1$VD <- as.factor(data1$VD)
data1$TIPO <- as.factor(data1$TIPO)


filter(data1, TIPO== "Z") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  summary()

filter(data1, DPRIMA== "DPR", !VAL=="TOT") %>%
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
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                 "OCUPACION","OCUPA_CAT","OCUPACION_CAT")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")

filter(data1, DPRIMA== "EIDP", !VAL=="TOT") %>%
  aov(value ~ DECADA*COND*VAL, data=.)%>%
  summary()

filter(data1, DPRIMA== "EIDP", !VAL=="TOT") %>%
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
  lm(formula= value ~ AMP_ROS*COVID_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*COVID_CAT, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*SHIPLEY, data =.)%>%
  summary()

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS*SHIPLEY, data =.)%>%
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

filter(data2, COND=="ROSTROS", !CRI_Total_CAT=="MEDIO")%>%
  lm(formula= value ~ AMP_ROS*CRI_Total, data =.)%>%
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
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                  "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE",
                                  "IDERE_R_Z","IDERE_R_CAT","IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY","SHIPLEY_Z",
                                  "SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPA_CAT","OCUPA_Z","OCUPACION_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

data3 <- read_xlsx("Junto.xlsx",sheet = "Multiplicacion")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                  "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE",
                                  "IDERE_R_Z","IDERE_R_CAT","IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY","SHIPLEY_Z",
                                  "SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPA_CAT","OCUPA_Z","OCUPACION_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z", 
                                  "CRIq_SUP","IDEREE_AGE","SHIPLEY_AGE","CRIq_AGE","SUEÑOSES_AGE")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

### ANALISIS DE VARIABLES CON ESCENAS/SUPRESION Y ROSTROS/AMPLIFICACION

### AMPLIFICACION - ROSTROS 
filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)
# U= 1469, p=8.9x10-6

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4298, p=0.01278


### SUPRESION - ESCENAS 
filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)
# U= 1464, p=1.102e-05

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)
# U= 756, p=0.02434

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4144, p=0.00443

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  aov(value ~ SUP_ROS*CRI_Total_CAT, data =.)%>%
  summary()
# F= 8.854 , p=0.00412 

filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="BAJO") %$%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()
filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="ALTO") %$%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()
  


### ANALISIS DE VARIABLES CON EDAD

### ROSTROS
filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)
# U= 749, p=0.03053

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4298, p=0.01278

filter(data2, COND== "ROSTROS", TIPO== "Z", !SUEÑO_NOR_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*SUEÑO_NOR_CAT, data =.)%>%
  summary()
# F= 2.748 , p=0.09914 

### ESCENAS
filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(value ~ IDARE_E_CAT, data =.)
# U= 1590, p=0.04019

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)
# U= 4144, p=0.00443

filter(data2, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*IDERE_E_CAT, data =.)%>%
  summary()
# F= 8.181, p=0.005043  

filter(data2, COND== "ESCENAS", TIPO== "Z", IDERE_E_CAT=="ALTO") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()
filter(data2, COND== "ESCENAS", TIPO== "Z", IDERE_E_CAT=="BAJO") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()

filter(data2, COND== "ESCENAS", TIPO== "Z", !SHIPLEY_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*SHIPLEY_CAT, data =.)%>%
  summary()
# F= 4.010, p=0.0487  

filter(data2, COND== "ESCENAS", TIPO== "Z", SHIPLEY_CAT=="ALTO") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()
filter(data2, COND== "ESCENAS", TIPO== "Z", SHIPLEY_CAT=="BAJO") %$%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()

### AMPLIFICACION
filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ ESCOLARIDAD_CAT, data =.)
# U= 12536, p=1.805e-05

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ CRI_Total_CAT, data =.)
# U= 10544, p=0.03429

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(AMP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()
# F= 8.557, p=0.00374 

filter(data2, CRI_Total_CAT=="BAJO") %$%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
filter(data2, CRI_Total_CAT=="ALTO") %$%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()



### SUPRESION
filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()
# F= 28.383 , p=2.11e-07 

filter(data2, CRI_Total_CAT=="BAJO") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
filter(data2, CRI_Total_CAT=="ALTO") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()

filter(data2, !MoCA_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*MoCA_CAT, data =.)%>%
  summary()
# F= 16.727 , p=4.74e-05 

filter(data2, !SUEÑO_2DA_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*SUEÑO_2DA_CAT, data =.)%>%
  summary()

filter(data2, SUEÑO_2DA_CAT=="BAJO") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()
filter(data2, SUEÑO_2DA_CAT=="ALTO") %$%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data =.)%>%
  summary()



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


filter(data, TIPO=="ROSTROS") %$%
  describeBy(value, DECADA)

filter(data, TIPO=="ESCENAS") %$%
  describeBy(value, DECADA)

filter(data) %$%
  describeBy(AMP_ROS, DECADA)

filter(data) %$%
  describeBy(SUP_ROS, DECADA)


anova_test(SUEÑO_NOR~DECADA, data=data)
aov(SUEÑO_NOR~DECADA, data=data)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data) %$%
  describeBy(SUEÑO_NOR, DECADA)

anova_test(SUEÑO_2DA~DECADA, data=data)
aov(SUEÑO_2DA~DECADA, data=data)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data) %$%
  describeBy(SUEÑO_2DA, DECADA)

filter(data, COND=="ROSTROS", TIPO=="TOT") %$%
  describeBy(value, DECADA)

filter(data, COND=="ESCENAS", TIPO=="TOT") %$%
  describeBy(value, DECADA)

filter(data) %$%
  describeBy(AMP_ROS, DECADA)

filter(data) %$%
  describeBy(SUP_ROS, DECADA)


#### RESULTADOS MEDIAS POR GRUPOS ####

### AMPLIFICACION - ROSTROS
filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  describeBy(value, EDAD_CAT)
#Alto: 0.59 (SD=1.01)
#Bajo: -0.23 (SD=1.22)

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.2 (SD=0.72)
#Bajo: 0.17 (SD=1.17)

### SUPRESION - ESCENAS 
filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %$%
  describeBy(value, EDAD_CAT)
#Alto: 0.48 (SD=1.01)
#Bajo: -0.24 (SD=1.37)

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(value, CRI_Total_CAT)
#Alto: 0.26 (SD=0.91)
#Bajo: -0.18 (SD=1.1)

filter(data3, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_SUP, CRI_Total_CAT)
#Alto: 0.14 (SD=1.95)
#Bajo: -0.27 (SD=1.32)

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.25 (SD=0.75)
#Bajo: 0.21 (SD=1.13)


### ROSTROS Y EDAD
filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  describeBy(value, CRI_Total_CAT)
#Alto: 0.17 (SD=0.8)
#Bajo: -0.08 (SD=1.43)

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.2 (SD=0.72)
#Bajo: 0.17 (SD=1.17)


### ESCENAS Y EDAD
filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %$%
  describeBy(value, IDARE_E_CAT)
#Alto: -0.22 (SD=0.88)
#Bajo: 0.11 (SD=0.91)

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %$%
  describeBy(value, MoCA_CAT)
#Alto: -0.25 (SD=0.75)
#Bajo: 0.21 (SD=1.13)

filter(data3, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO") %$%
  describeBy(IDEREE_AGE, IDERE_E_CAT)
#Alto: -0.19 (SD=1.24)
#Bajo: -0.27 (SD=1.5)

filter(data3, COND== "ESCENAS", TIPO== "Z", !SHIPLEY_CAT=="MEDIO") %$%
  describeBy(SHIPLEY_AGE, SHIPLEY_CAT)
#Alto: 0.04 (SD=1.37)
#Bajo: 0.18 (SD=1.99)

### AMPLIFICACION Y EDAD
filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, ESCOLARIDAD_CAT)
#Alto: 0.1 (SD=1.19)
#Bajo: -0.45 (SD=0.88)

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(AMP_ROS_Z, CRI_Total_CAT)
#Alto: 0.14 (SD=0.96)
#Bajo: -0.22 (SD=0.93)

filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_AGE, CRI_Total_CAT)
#Alto: 1.88 (SD=1.19)
#Bajo: 1.2 (SD=0.89)



### SUPRESION Y EDAD

filter(data3, !CRI_Total_CAT=="MEDIO") %$%
  describeBy(CRIq_AGE, CRI_Total_CAT)
#Alto: 1.88 (SD=1.19)
#Bajo: 1.2 (SD=0.89)

filter(data3, !SUEÑO_2DA_CAT=="MEDIO") %$%
  describeBy(SUEÑOSES_AGE, SUEÑO_2DA_CAT)
#Alto: -0.01 (SD=1.31)
#Bajo: -0.38 (SD=1.29)



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


