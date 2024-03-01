library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(RColorBrewer)

##### GRAFICAS ARTICULO BASICAS ####

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

ggplot(data = data, aes(x = EDAD_Z, y = AMP_ROS_Z)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

ggplot(data = data, aes(x = EDAD_Z, y = SUP_ROS_Z)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = EDAD_Z, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WME/AF") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) 

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) 


filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = value, y = AMP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WME/AF", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) + 
  scale_y_continuous(breaks = seq(-2, 2))

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = value, y = SUP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WME/IF", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(text = element_text(size=17)) + 
  scale_y_continuous(breaks = seq(-2, 2))



#### REGRESIONES SIMPLES EN ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

### AMPLIFICACION - ROSTROS 

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = MoCA_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefROMOCA[1], slope = coefROMOCA[2], color = "#CD6090", lwd = 1.5) +
  labs(x = "CogFun", y = "WME/AF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = CRI_Total_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefROCRI[1], slope = coefROCRI[2], color = "#CD6090", lwd = 1.5) +
  labs(x = "CogRes", y = "WME/AF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17)) 

### SUPRESION - ESCENAS 

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = MoCA_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefESMOCA[1], slope = coefESMOCA[2], color = "#8B3A62", lwd = 1.5) +
  labs(x = "CogFun", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = IDARE_E_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefESAE[1], slope = coefESAE[2], color = "#8B3A62", lwd = 1.5) +
  labs(x = "SAnx", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = SUEÑO_NOR_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#8B3A62", lwd = 1.5) +
  labs(x = "S/session", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))


#### REGRESIONES SIMPLES EN ATT + WM + EDAD + MODULADOR ####

### AMPLIFIFCACION

ggplot(data = data, aes(x = ESCOLARIDAD_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefAMESCOL[1], slope = coefAMESCOL[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "YS", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = CRI_Total_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefAMCRI[1], slope = coefAMCRI[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "CogRes", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = MoCA_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "CogFun", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = IDARE_R_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "TAnx", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = IDARE_E_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "SAnx", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = IDERE_R_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "TDep", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = SUEÑO_NOR_Z, y = AMP_ROS_Z)) +
  geom_point() +
  geom_abline(intercept = coefESSN[1], slope = coefESSN[2], color = "#5F9EA0", lwd = 1.5) +
  labs(x = "s/session", y = "ENHANCEMENT INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))


### SUPRESION

ggplot(data = data, aes(x = IDERE_R_Z, y = SUP_ROS_Z)) +
  geom_point() + 
  geom_abline(intercept = coefSUER[1], slope = coefSUER[2], color = "#509FC7", lwd = 1.5) +
  labs(x = "TDep", y = "SUPPRESSION INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = IDERE_E_Z, y = SUP_ROS_Z)) +
  geom_point() + 
  geom_abline(intercept = coefSUEE[1], slope = coefSUEE[2], color = "#509FC7", lwd = 1.5) +
  labs(x = "SDep", y = "SUPPRESSION INDEX") +  
  theme_classic() +
  theme(text = element_text(size = 17))


### ROSTROS 

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = CRI_Total_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefROSCRI[1], slope = coefROSCRI[2], color = "#698B69", lwd = 1.5) +
  labs(x = "CogRes", y = "WME/AF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = MoCA_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefROSMOCA[1], slope = coefROSMOCA[2], color = "#698B69", lwd = 1.5) +
  labs(x = "CogFun", y = "WME/AF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))


### ESCENAS

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = MoCA_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefESCMOCA[1], slope = coefESCMOCA[2], color = "#6E8B3D", lwd = 1.5) +
  labs(x = "CogFun", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = IDARE_E_Z, y = value)) +
  geom_point() +
  geom_abline(intercept = coefESCAE[1], slope = coefESCAE[2], color = "#6E8B3D", lwd = 1.5) +
  labs(x = "SAnx", y = "WME/IF") +  
  theme_classic()+ 
  theme(text = element_text(size = 17))



#### INTERACCIONES EN REGRESIONES DE ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

### AMPLIFICACION - ROSTROS 

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = AMP_ROS_Z, y = value, colour= IDARE_E_CAT)) +
  geom_point() +  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "ENHANCEMENT INDEX", y = "WME/AF", color = "SAnx_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$IDARE_E_CAT)), name = "Set1"))

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = AMP_ROS_Z, y = value, colour= EDAD_CAT)) +
  geom_point() +  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "ENHANCEMENT INDEX", y = "WME/AF", color = "Age_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$EDAD_CAT)), name = "Set1"))

filter(data, COND=="ROSTROS", TIPO == "Z",!IDARE_E_CAT=="MEDIUM") %>%
  wilcox.test(value ~ IDARE_E_CAT, data =.)

filter(data, COND=="ROSTROS", TIPO == "Z",!EDAD_CAT=="MEDIUM") %>%
  wilcox.test(value ~ EDAD_CAT, data =.)



### SUPRESION - ESCENAS 

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= IDERE_E_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WME/IF", color = "SDep_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$IDERE_E_CAT)), name = "Set1"))

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= EDAD_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPPRESSION INDEX", y = "WME/IF", color = "Age_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$EDAD_CAT)), name = "Set1"))

filter(data, COND=="ESCENAS", TIPO == "Z",!EDAD_CAT=="MEDIUM") %>%
  wilcox.test(value ~ EDAD_CAT, data =.)

filter(data, COND=="ESCENAS", TIPO == "Z",!IDERE_E_CAT=="MEDIUM") %>%
  wilcox.test(value ~ IDERE_E_CAT, data =.)

filter(data, COND=="ESCENAS", TIPO == "Z",!SUEÑO_NOR_CAT=="MEDIUM") %>%
  wilcox.test(value ~ SUEÑO_NOR_CAT, data =.)

filter(data, COND=="ESCENAS", TIPO == "Z",!SUEÑO_2DA_CAT=="MEDIUM") %>%
  wilcox.test(value ~ SUEÑO_2DA_CAT, data =.)


filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "LOW")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_NOR_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "LOW AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/usual") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_NOR_CAT)), name = "Set2"))

filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "HIGH")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_NOR_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "HIGH AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/usual") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_NOR_CAT)), name = "Set2"))

filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "MEDIUM")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_NOR_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "MEDIUM AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/usual") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_NOR_CAT)), name = "Set2"))


filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "LOW")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_2DA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "LOW AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/session") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_2DA_CAT)), name = "Set2"))

filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "HIGH")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_2DA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "HIGH AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/session") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_2DA_CAT)), name = "Set2"))

filter(data, COND=="ESCENAS", TIPO == "Z", EDAD_CAT== "MEDIUM")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= SUEÑO_2DA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title= "MEDIUM AGE", x = "SUPPRESSION INDEX", y = "WME/IF", color = "S/session") +
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_2DA_CAT)), name = "Set2"))


#### REGRESIONES SIMPLES EN ATT + WM + EDAD + MODULADOR ####

### AMPLIFIFCACION

ggplot(data = data, aes(x = EDAD_Z, y = AMP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "CogRes_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = EDAD_Z, y = AMP_ROS_Z, colour= MoCA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "CogFun_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = EDAD_Z, y = AMP_ROS_Z, colour= IDARE_R_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "TAnx_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17))

ggplot(data = data, aes(x = EDAD_Z, y = AMP_ROS_Z, colour= IDARE_E_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "SAnx_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17))

filter(data, !CRI_Total_CAT=="MEDIUM") %>%
  wilcox.test(AMP_ROS_Z ~ CRI_Total_CAT, data =.)

filter(data, !MoCA_CAT=="MEDIUM") %>%
  wilcox.test(AMP_ROS_Z ~ MoCA_CAT, data =.)

filter(data, !IDARE_R_CAT=="MEDIUM") %>%
  wilcox.test(AMP_ROS_Z ~ IDARE_R_CAT, data =.)

filter(data, !IDARE_E_CAT=="MEDIUM") %>%
  wilcox.test(AMP_ROS_Z ~ IDARE_E_CAT, data =.)


### SUPRESION

ggplot(data = data, aes(x = EDAD_Z, y = SUP_ROS_Z, colour= ESCOLARIDAD_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "YS_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17)) 
ggplot(data = data, aes(x = EDAD_Z, y = SUP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "CogRes_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17)) 
ggplot(data = data, aes(x = EDAD_Z, y = SUP_ROS_Z, colour= MoCA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "CogFun_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17)) 
ggplot(data = data, aes(x = EDAD_Z, y = SUP_ROS_Z, colour= SUEÑO_2DA_CAT)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "S/session_Cat") +  
  theme_classic() +
  theme(text = element_text(size = 17)) 

filter(data, !ESCOLARIDAD_CAT=="MEDIUM") %>%
  wilcox.test(SUP_ROS_Z ~ ESCOLARIDAD_CAT, data =.)

filter(data, !CRI_Total_CAT=="MEDIUM") %>%
  wilcox.test(SUP_ROS_Z ~ CRI_Total_CAT, data =.)

filter(data, !MoCA_CAT=="MEDIUM") %>%
  wilcox.test(SUP_ROS_Z ~ MoCA_CAT, data =.)

filter(data, !SUEÑO_2DA_CAT=="MEDIUM") %>%
  wilcox.test(SUP_ROS_Z ~ SUEÑO_2DA_CAT, data =.)


### ROSTROS 

filter(data, COND=="ROSTROS", TIPO == "Z")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= SUEÑO_NOR_CAT)) +
  geom_point() +  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WME/AF", color = "S/session_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$SUEÑO_NOR_CAT)), name = "Dark2"))

filter(data, COND=="ROSTROS", TIPO == "Z",!SUEÑO_NOR_CAT=="MEDIUM") %>%
  wilcox.test(value ~ SUEÑO_NOR_CAT, data =.)

### ESCENAS 

filter(data, COND=="ESCENAS", TIPO == "Z")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= IDERE_E_CAT)) +
  geom_point() +  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WME/IF", color = "SDep_Cat") +
  theme_classic()+ 
  theme(text = element_text(size = 17)) + 
  scale_color_manual(values = brewer.pal(n = length(unique(data$IDERE_E_CAT)), name = "Dark2"))

filter(data, COND=="ESCENAS", TIPO == "Z",!IDERE_E_CAT=="MEDIUM") %>%
  wilcox.test(value ~ IDERE_E_CAT, data =.)


#### REGRESIONES CON NOMBRE PARA ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

# ROSTROS + AMPLIFICACION + EDAD
ROCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

ROMOCA <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  summary()

# ESCENAS + SUPRESION + EDAD
ESMOCA <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*MoCA_Z*EDAD_Z, data =.)%>%
  summary()

ESAE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

ESSN <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)%>%
  summary()

#### REGRESIONES CON NOMBRE PARA ATT + WM + EDAD + MODULADOR ####

# AMPLIFICACION + EDAD
AMESCOL <- data %>%
  lm(formula= AMP_ROS_Z ~ ESCOLARIDAD_Z*EDAD_Z, data =.)%>%
  summary()

AMCRI <- data %>%
  lm(formula= AMP_ROS_Z ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

AMMOCA <- data %>%
  lm(formula= AMP_ROS_Z ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

AMAR <- data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_R_Z*EDAD_Z, data =.)%>%
  summary()

AMAE <- data %>%
  lm(formula= AMP_ROS_Z ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()

AMER <- data %>%
  lm(formula= AMP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

AMS2 <- data %>%
  lm(formula= AMP_ROS_Z ~ SUEÑO_2DA_Z*EDAD_Z, data =.)%>%
  summary()


# SUPRESION + EDAD
SUER <- data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_R_Z*EDAD_Z, data =.)%>%
  summary()

SUEE <- data %>%
  lm(formula= SUP_ROS_Z ~ IDERE_E_Z*EDAD_Z, data =.)%>%
  summary()

# ROSTROS + EDAD
ROSCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ CRI_Total_Z*EDAD_Z, data =.)%>%
  summary()

ROSMOCA <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

# ESCENAS + EDAD
ESCMOCA <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ MoCA_Z*EDAD_Z, data =.)%>%
  summary()

ESCAE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ IDARE_E_Z*EDAD_Z, data =.)%>%
  summary()


#### COEFICIENTES ATT + WM + MODULADOR + EDAD ####
# (para reducir el efecto de la edad) #

# ROSTROS + AMPLIFICACION + EDAD
coefROCRI <- coef(ROCRI)
coefROMOCA <- coef(ROMOCA)

# ESCENAS + SUPRESION + EDAD
coefESMOCA <- coef(ESMOCA)
coefESAE <- coef(ESAE)
coefESSN <- coef(ESSN)


#### COEFICIENTES ATT + WM + EDAD + MODULADOR ####

# AMPLIFICACION + EDAD
coefAMESCOL <- coef(AMESCOL)
coefAMCRI <- coef(AMCRI)
coefAMMOCA <- coef(AMMOCA)
coefAMAR <- coef(AMAR)
coefAMAE <- coef(AMAE)
coefAMER <- coef(AMER)
coefAMS2 <- coef(AMS2)

# SUPRESION + EDAD
coefSUER <- coef(SUER)
coefSUEE <- coef(SUEE)

# ROSTROS + EDAD
coefROSCRI <- coef(ROSCRI)
coefROSMOCA <- coef(ROSMOCA)

# ESCENAS + EDAD
coefESCMOCA <- coef(ESCMOCA)
coefESCAE <- coef(ESCAE)
