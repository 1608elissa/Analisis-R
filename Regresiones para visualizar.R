library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)

#### BASE DE DATOS ####

data <- read_xlsx("Junto.xlsx",sheet = "NATyZ")%>%
  gather(VAR_COND_TIPO, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT",
                                  "MoCA","MoCA_Z","MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z",
                                  "ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                  "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE",
                                  "IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE","IDERE_R_Z","IDERE_R_CAT",
                                  "IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY",
                                  "SHIPLEY_Z","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT",
                                  "SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT","OCUPACION","OCUPAC_CAT",
                                  "OCUPA_Z","OCUPA_Z_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(VAR_COND_TIPO, c("VD","COND","TIPO"), sep = "_")


#### REGRESIONES SIMPLES ####

## ATT CON WM ##
data %>%
  filter(COND == "ROSTROS", TIPO == "Z") %>%
  lm(value ~ AMP_ROS_Z, data = .) %>%
  summary()

filter(data, COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z, data =.)%>%
  summary()


## ATT Y WM CON EDAD ##
data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data = .)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data = .)%>%
  summary()


#### REGRESIONES ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

# ROSTROS + AMPLIFICACION + EDAD
data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

ROCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

ROMO <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


# ESCENAS + SUPRESION + EDAD
data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


#### REGRESIONES ATT + WM + EDAD + MODULADOR ####

# AMPLIFICACION + EDAD
data %>%
  lm(formula= AMP_ROS_Z ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

 data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= AMP_ROS_Z ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


# SUPRESION + EDAD
data %>%
  lm(formula= SUP_ROS_Z ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  lm(formula= SUP_ROS_Z ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


# ROSTROS + EDAD
data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


# ESCENAS + EDAD
data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()



