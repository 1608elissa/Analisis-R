library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)

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

#### MODELOS ####
e <- filter(data, TIPO=="ROSTROS")

f <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = e)

step(object = f, direction = "both", trace = 1)

modeloROS <- (lm(formula = value ~ EDAD + ESCOLARIDAD + MoCA + CRI_Total + 
                   EDAD:ESCOLARIDAD + EDAD:MoCA + EDAD:CRI_Total + ESCOLARIDAD:CRI_Total + 
                   MoCA:CRI_Total + EDAD:ESCOLARIDAD:CRI_Total + EDAD:MoCA:CRI_Total, 
                 data = e))
summary(modeloROS)

g <- filter(data, TIPO=="ESCENAS")

h <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = g)

step(object = h, direction = "both", trace = 1)

modeloESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = g))
summary(modeloESC)

modeloAMP <- (lm(formula = AMP_ROS ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = data))
summary(modeloAMP)

modeloSUP <- (lm(formula = SUP_ROS ~ EDAD + ESCOLARIDAD + MoCA + CRI_Total, data = data))
summary(modeloSUP)


#### REGRESIONES SIMPLES ####

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

filter(data, TIPO=="ROSTROS")%>%
  lm(formula= value ~ CRI_Total*EDAD, data =.)%>%
  summary()

filter(data, TIPO=="ESCENAS")%>%
  lm(formula= value ~ CRI_Total*EDAD, data =.)%>%
  summary()

lm(formula= AMP_ROS ~ CRI_Total*EDAD, data = data)%>%
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
data2 <- read_xlsx("Junto.xlsx",sheet = "GpoZ")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO","EDAD", "MoCA", 
                                     "MoCA_CAT", "CRI_Total","CRI_Total_CAT",  
                                     "IDARE_E_PUNTAJE","IDARE_E_CAT",
                                     "IDERE_E_PUNTAJE","IDERE_E_CAT", "SHIPLEY",
                                     "SHIPLEY_CAT","SUEÑO_NOR","SUEÑONOR_CAT",
                                     "SUEÑO_2DA","SUEÑO2_CAT","OCUPA_CAT",
                                     "OCUPACION_CAT","AMP_ROS","SUP_ROS")) %>%
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")

data2 %>%
  mutate(DECADA = as.factor(DECADA),
         SEXO = as.factor(SEXO),
         VD = as.factor(VD),
         COND = as.factor(COND),
         VAL = as.factor(VAL),
         TIPO = as.factor(TIPO))


### ANALISIS DE VARIABLES CON ESCENAS/SUPRESION Y ROSTROS/AMPLIFICACION
filter(data2, TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS*CRI_Total_CAT, data = .)%>%
  summary()

filter(data2, TIPO=="ESCENAS", CRI_Total_CAT=="BAJO")%>%
  lm(formula= value ~ SUP_ROS, data = .)%>%
  summary()


### ANALISIS DE VARIABLES CON EDAD
filter(data2, TIPO=="ROSTROS")%>%
  lm(formula= value ~ SUEÑONOR_CAT*EDAD, data = .)%>%
  summary()

filter(data2, TIPO=="ROSTROS", !SUEÑONOR_CAT=="MEDIO" )%>%
  lm(formula= value ~ SUEÑONOR_CAT*EDAD, data = .)%>%
  summary()

lm(formula= AMP_ROS ~ CRI_Total_CAT*EDAD, data = data2)%>%
  summary()

filter(data2, TIPO=="ESCENAS", !CRI_Total_CAT=="MEDIO")%>%
  lm(formula= AMP_ROS ~ CRI_Total_CAT*EDAD, data = .)%>%
  summary()

filter(data2, !CRI_Total_CAT=="MEDIO")%>%
  lm(formula= value ~ CRI_Total_CAT*EDAD, data = .)%>%
  summary()

filter(data2, TIPO=="ESCENAS")%>%
  lm(formula= value ~ IDERE_E_CAT*EDAD, data = .)%>%
  summary()

filter(data2, TIPO=="ESCENAS", !IDERE_E_CAT == "MEDIO")%>%
  lm(formula= value ~ IDERE_E_CAT*EDAD, data = .)%>%
  summary()

filter(data2, TIPO=="ESCENAS", !IDERE_E_CAT == "BAJO")%>%
  lm(formula= value ~ IDERE_E_CAT*EDAD, data = .)%>%
  summary()

filter(data2, TIPO=="ESCENAS",!SHIPLEY_CAT == "MEDIO")%>%
  lm(formula= value ~ SHIPLEY_CAT*EDAD, data = .)%>%
  summary()

filter(data2, !CRI_Total_CAT == "MEDIO")%>%
  lm(formula= SUP_ROS ~ CRI_Total_CAT*EDAD, data = .)%>%
  summary()
