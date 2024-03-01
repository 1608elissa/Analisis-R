library(readxl)
library(rstatix)
library(writexl)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(psych)

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
ROAM <- data %>%
  filter(COND == "ROSTROS", TIPO == "Z") %>%
  lm(value ~ AMP_ROS_Z, data = .) %>%
  tidy()

EXSU <- filter(data, COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z, data =.)%>%
  tidy()

ATTWM <- data.frame(ROAM,EXSU)

## ATT Y WM CON EDAD ##
ROAGE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  tidy()

ESAGE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ EDAD_Z, data =.)%>%
  tidy()

AMAGE <- data %>%
  lm(formula= AMP_ROS_Z ~ EDAD_Z, data = .)%>%
  tidy()

SUAGE <- data %>%
  lm(formula= SUP_ROS_Z ~ EDAD_Z, data = .)%>%
  tidy()

ATTWMAGE <- data.frame(ROAGE,ESAGE,AMAGE,SUAGE)
write_xlsx(list(ATTWM = ATTWM, ATTWMAGE = ATTWMAGE),"Resultados regresiones simples.xlsx")

#### REGRESIONES ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

# ROSTROS + AMPLIFICACION + EDAD
ROESCOL <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

ROCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

ROMOCA <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

ROAR <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ROAE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ROER <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ROEE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ROSN <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

ROS2 <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

ROSTROS_AMP_MOD <- data.frame(ROESCOL,ROCRI,ROMOCA,ROAR,ROAE,ROER,ROEE,ROSN,ROS2)


# ESCENAS + SUPRESION + EDAD
ESESCOL <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

ESCRI <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

ESMOCA <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

ESAR <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ESAE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ESER <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ESEE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ESSN <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

ESS2 <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

ESCENAS_SUP_MOD <- data.frame(ESESCOL,ESCRI,ESMOCA,ESAR,ESAE,ESER,ESEE,ESSN,ESS2)


write_xlsx(list(ESCENAS_SUP_MOD = ESCENAS_SUP_MOD,
                ROSTROS_AMP_MOD = ROSTROS_AMP_MOD),"Resultados regresiones moderadores.xlsx")


#### REGRESIONES ATT + WM + EDAD + MODULADOR ####

# AMPLIFICACION + EDAD
AMESCOL <- data %>%
  lm(formula= AMP_ROS_Z ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

AMCRI <- data %>%
  lm(formula= AMP_ROS_Z ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

AMMOCA <- data %>%
  lm(formula= AMP_ROS_Z ~ MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

AMAR <- data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

AMAE <- data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

AMER <- data %>%
  lm(formula= AMP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

AMEE <- data %>%
  lm(formula= AMP_ROS_Z ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

AMSN <- data %>%
  lm(formula= AMP_ROS_Z ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

AMS2 <- data %>%
  lm(formula= AMP_ROS_Z ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

AMP_MOD <- data.frame(AMESCOL,AMCRI,AMMOCA,AMAR,AMAE,AMER,AMEE,AMSN,AMS2)


# SUPRESION + EDAD
SUESCOL <- data %>%
  lm(formula= SUP_ROS_Z ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

SUCRI <- data %>%
  lm(formula= SUP_ROS_Z ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

SUMOCA <- data %>%
  lm(formula= SUP_ROS_Z ~ MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

SUAR <- data %>%
  lm(formula= SUP_ROS_Z ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

SUAE <- data %>%
  lm(formula= SUP_ROS_Z ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

SUER <- data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

SUEE <- data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

SUSN <- data %>%
  lm(formula= SUP_ROS_Z ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

SUS2 <- data %>%
  lm(formula= SUP_ROS_Z ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

SUP_MOD <- data.frame(SUESCOL,SUCRI,SUMOCA,SUAR,SUAE,SUER,SUEE,SUSN,SUS2)


# ROSTROS + EDAD
ROSESCOL <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

ROSCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

ROSMOCA <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

ROSAR <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ROSAE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ROSER <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ROSEE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ROSSN <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

ROSS2 <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

ROSTROS_MOD <- data.frame(ROSESCOL,ROSCRI,ROSMOCA,ROSAR,ROSAE,ROSER,ROSEE,ROSSN,ROSS2)


# ESCENAS + EDAD
ESCESCOL <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  tidy()

ESCCRI <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  tidy()

ESCMOCA <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  tidy()

ESCAR <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ESCAE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ESCER <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  tidy()

ESCEE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  tidy()

ESCSN <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  tidy()

ESCS2 <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  tidy()

ESCENAS_MOD <- data.frame(ESCESCOL,ESCCRI,ESCMOCA,ESCAR,ESCAE,ESCER,ESCEE,ESCSN,ESCS2)

write_xlsx(list(AMP_MOD = AMP_MOD, SUP_MOD = SUP_MOD,
                ROSTROS_MOD = ROSTROS_MOD, ESCENAS_MOD = ESCENAS_MOD),"Resultados regresiones con edad.xlsx")


