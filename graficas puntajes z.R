library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(psych)
library(magrittr)


##### GRAFICAS ARTICULO BASICAS ####
data <- read_xlsx("Junto.xlsx",sheet = "PuntajeZ")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", 
                                     "ESCOLARIDAD", "CRI_Total","IDARE_R_PUNTAJE",  
                                     "IDARE_E_PUNTAJE","IDERE_R_PUNTAJE",
                                     "IDERE_E_PUNTAJE","COVID_CAT", "SHIPLEY",
                                     "SUEÑO_NOR","SUEÑO_2DA","OCUPA_CAT",
                                     "AMP_ROS","SUP_ROS")) %>%
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")


ggplot(data = data, aes(x = EDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

ggplot(data = data, aes(x = EDAD, y = SUP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) + 
  scale_y_continuous(breaks = seq(-2, 5))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) + 
  scale_y_continuous(breaks = seq(-2, 5))


filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = value, y = AMP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))+ 
  scale_y_continuous(breaks = seq(-2.5, 2.5)) +
  scale_x_continuous(breaks = seq(-2, 5))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = value, y = SUP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) +
  scale_y_continuous(breaks = seq(-2.5, 2.5)) +
  scale_x_continuous(breaks = seq(-2, 5))



#### GRAFICAS ARTICULO REGRESIONES MULTIPLES Y GRAFICAS DE VIOLIN ####

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

### AMPLIFICACION - ROSTROS 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "AGE", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15)) 

filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=EDAD_CAT, fill=EDAD_CAT)) + 
  scale_fill_brewer(palette="BuPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "AGE GROUP", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE FUNCTION GROUP", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))


### SUPRESION - ESCENAS 

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "AGE", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +  
  ylim (-1, 3) +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +  
  xlim (-1,2) + ylim (-1, 3) +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +  
  ylim (-1, 3) +
  theme_classic()+ 
  theme(text = element_text(size = 15)) 

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPPRESSION INDEX", y = "WM ATTEND SCENES/IGNORE FACES (IEI)", color = "CRI_Total_CAT") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=EDAD_CAT, fill=EDAD_CAT)) + 
  scale_fill_brewer(palette="BuPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "AGE GROUP", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE FUNCTION GROUP", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))



### ROSTROS 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15)) 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15)) 

filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE FUNCTION GROUP", y = "WM ATTEND FACES/IGNORE SCENES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))



### ESCENAS

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = IDARE_E_PUNTAJE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#008B8B", lwd = 1.5) +
  labs(x = "STATE ANXIETY", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WWM ATTEND SCENES/IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= IDERE_E_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WM ATTEND SCENES/IGNORE FACES (IEI)", color = "IDERE_E_CAT") +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !SHIPLEY_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= SHIPLEY_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WM ATTEND SCENES/IGNORE FACES (IEI)", color = "SHIPLEY_CAT") +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=IDARE_E_CAT, fill=IDARE_E_CAT)) + 
  scale_fill_brewer(palette="Greens") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "STATE ANXIETY GROUP", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE FUNCTION GROUP", y = "WM ATTEND SCENES/IGNORE FACES (IEI)") +
  ylim (-1, 3) +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))



### AMPLIFIFCACION

ggplot(data = data, aes(x = ESCOLARIDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#5F9EA0", lwd = 1.5) +
  labs(x = "YEARS OF SCHOOLING", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 15))

ggplot(data = data, aes(x = CRI_Total, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 15))

filter(data2, !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = AMP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "CRI_CAT") +
  theme_classic()+ 
  theme(text = element_text(size = 15))

filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=AMP_ROS_Z, x=ESCOLARIDAD_CAT, fill=ESCOLARIDAD_CAT)) +
  scale_fill_brewer(palette="Reds") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "YEARS SCHOOLING GROUP", y = "ENHANCEMENT INDEX") +
  theme_classic()+ 
  theme(legend.position = "none") +
  ylim (-2, 2) +
  theme(text = element_text(size = 15))

filter(data2, !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=AMP_ROS_Z, x=CRI_Total_CAT, fill=CRI_Total_CAT)) +
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "ENHANCEMENT INDEX", cex.lab = 60) +
  theme_classic()+ 
  theme(legend.position = "none") +
  ylim (-2, 2) +
  theme(text = element_text(size = 15))




### SUPRESION

filter(data2, !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = SUP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "CRI_CAT") +
  theme_classic()+ 
  ylim (-2, 2) +
  theme(text = element_text(size = 15))

filter(data2, !SUEÑO_2DA_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = SUP_ROS_Z, colour= SUEÑO_2DA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "SLEEP-SES_CAT") +
  theme_classic()+ 
  theme(text = element_text(size = 15))










