library(interactions)
library(sandwich)
library(jtools)

data2 <- read_xlsx("Junto.xlsx",sheet = "Regresiones")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT",
                                  "CRI_Total","CRI_Total_Z","CRI_Total_CATE","CRI_Total_CAT","IDARE_R_PUNTAJE",
                                  "IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z",
                                  "IDARE_E_CAT","IDERE_R_PUNTAJE","IDERE_R_Z","IDERE_R_CAT",
                                  "IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT",
                                  "SHIPLEY","SHIPLEY_Z","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z",
                                  "SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPAC_CAT","OCUPA_Z","OCUPA_CAT","AMP_ROS",
                                  "AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")


interact_plot(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, plot.points = TRUE) ##MEDIA, +1DE, -1DE con puntitos
sim_slopes(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, jnplot = TRUE) #REGIONES SIGNIFICATIVAS
interact_plot(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, interval = TRUE) #MEDIA, +1DE, -1DE



WMSUPCRIcat <- filter(data2, COND== "ESCENAS", TIPO== "Z")%>%
  lm(formula= value ~ SUP_ROS_Z*CRI_Total_CAT, data =.)

interact_plot(WMSUPCRIcat, pred = SUP_ROS_Z, modx = CRI_Total_CAT, interval = TRUE)


WMSUPCRI <- filter(data2, COND== "ESCENAS", TIPO== "Z")%>%
  lm(formula= value ~ SUP_ROS_Z*CRI_Total_Z, data =.)

sim_slopes(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, jnplot = TRUE)
interact_plot(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, interval = TRUE)


johnson_neyman(model = WMSUPCRI, pred = SUP_ROS_Z,
               modx = CRI_Total_Z)
 
summ(WMSUPCRI) 

sim_slopes(WMSUPCRI, pred = SUP_ROS_Z, modx = CRI_Total_Z, cond.int = TRUE)



SUPAGECRI <- lm(formula= SUP_ROS_Z ~ EDAD_Z*CRI_Total_Z, data =data2)
  

sim_slopes(SUPAGECRI, pred = EDAD_Z, modx = CRI_Total_Z, jnplot = TRUE)
interact_plot(SUPAGECRI, pred = EDAD_Z, modx = CRI_Total_Z, plot.points = TRUE)
interact_plot(SUPAGECRI, pred = EDAD_Z, modx = CRI_Total_Z, interval = TRUE)


SUPAGECRIcat <- lm(formula= SUP_ROS_Z ~ EDAD_Z*CRI_Total_CAT, data =data2)

interact_plot(SUPAGECRIcat, pred = EDAD_Z, modx = CRI_Total_CAT, plot.points = TRUE)
interact_plot(SUPAGECRIcat, pred = EDAD_Z, modx = CRI_Total_CAT, interval = TRUE)

