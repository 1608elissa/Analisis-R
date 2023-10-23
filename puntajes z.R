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
data2 <- read_xlsx("Junto.xlsx",sheet = "Regresiones")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                  "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE",
                                  "IDERE_R_Z","IDERE_R_CAT","IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY","SHIPLEY_Z",
                                  "SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPA_CAT","OCUPA_Z","OCUPACION_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

### ANALISIS DE VARIABLES CON ESCENAS/SUPRESION Y ROSTROS/AMPLIFICACION

### AMPLIFICACION - ROSTROS 
filter(data2, COND== "ROSTROS", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)

filter(data2, COND== "ROSTROS", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)


### SUPRESION - ESCENAS 
filter(data2, COND== "ESCENAS", !EDAD_CAT=="MEDIO") %$%
  wilcox.test(value ~ EDAD_CAT, data =.)

filter(data2, COND== "ESCENAS", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)

filter(data2, COND== "ESCENAS", !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(value ~ IDARE_E_CAT, data =.)

filter(data2, COND== "ESCENAS", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)

filter(data2, COND== "ESCENAS", !EDAD_CAT=="MEDIO") %$%
  aov(value ~ SUP_ROS*EDAD_CAT, data =.)%>%
  summary()



### ANALISIS DE VARIABLES CON EDAD

### ROSTROS
filter(data2, COND== "ROSTROS", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)

filter(data2, COND== "ROSTROS", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)

filter(data2, COND== "ROSTROS", !SUEÑO_NOR_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*SUEÑO_NOR_CAT, data =.)%>%
  summary()


### ESCENAS
filter(data2, COND== "ESCENAS", !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(value ~ IDARE_E_CAT, data =.)

filter(data2, COND== "ESCENAS", !MoCA_CAT=="MEDIO") %$%
  wilcox.test(value ~ MoCA_CAT, data =.)

filter(data2, COND== "ESCENAS", !IDERE_E_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*IDERE_E_CAT, data =.)%>%
  summary()

filter(data2, COND== "ESCENAS", !SHIPLEY_CAT=="MEDIO") %$%
  aov(value ~ EDAD_Z*SHIPLEY_CAT, data =.)%>%
  summary()


### AMPLIFICACION
filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ ESCOLARIDAD_CAT, data =.)

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ CRI_Total_CAT, data =.)

filter(data2, !IDARE_E_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ IDARE_E_CAT, data =.)

filter(data2, !IDERE_R_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ IDERE_R_CAT, data =.)

filter(data2, !SUEÑO_2DA_CAT=="MEDIO") %$%
  wilcox.test(AMP_ROS_Z ~ SUEÑO_2DA_CAT, data =.)

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(AMP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()


### SUPRESION
filter(data2, !IDERE_E_CAT=="MEDIO") %$%
  wilcox.test(SUP_ROS_Z ~ IDERE_E_CAT, data =.)

filter(data2, !CRI_Total_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =.)%>%
  summary()

filter(data2, !MoCA_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*MoCA_CAT, data =.)%>%
  summary()

filter(data2, !SUEÑO_2DA_CAT=="MEDIO") %$%
  aov(SUP_ROS_Z ~ EDAD_Z*SUEÑO_2DA_CAT, data =.)%>%
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


