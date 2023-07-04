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

#### FILTROS BASES DE DATOS ####
filEIDPESC <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")
filEIDPROS <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")

#### AMPLIFICACION ANALISIS CON ATENDER ESCENAS ####
### EDAD ###
modelampescEDAD <- lm(formula= value ~ AMP_ROS * EDAD_CAT, data = filEIDPESC)
summary(modelampescEDAD)

### ESCOLARIDAD_CAT ###
modelampescESCOL <- lm(formula= value ~ AMP_ROS * ESCOLARIDAD_CAT, data = filEIDPESC)
summary(modelampescESCOL)

### MoCA_CAT ###
modelampescMOCA <- lm(formula= value ~ AMP_ROS * MoCA_CAT, data = filEIDPESC)
summary(modelampescMOCA)

### CRI_Total_CAT ###
modelampescCRI <- lm(formula= value ~ AMP_ROS * CRI_Total_CAT, data = filEIDPESC)
summary(modelampescCRI)

### IDARE_R_CAT ###
modelampescIDARE_R <- lm(formula= value ~ AMP_ROS * IDARE_R_CAT, data = filEIDPESC)
summary(modelampescIDARE_R)

### IDERE_R_CAT ###
modelampescIDERE_R <- lm(formula= value ~ AMP_ROS * IDERE_R_CAT, data = filEIDPESC)
summary(modelampescIDERE_R)

#### AMPLIFICACION ANALISIS CON ATENDER ROSTROS ####
### EDAD ###
modelamprosEDAD <- lm(formula= value ~ AMP_ROS * EDAD_CAT, data = filEIDPROS)
summary(modelamprosEDAD)

### ESCOLARIDAD_CAT ###
modelamprosESCOL <- lm(formula= value ~ AMP_ROS * ESCOLARIDAD_CAT, data = filEIDPROS)
summary(modelamprosESCOL)

### MoCA_CAT ###
modelamprosMOCA <- lm(formula= value ~ AMP_ROS * MoCA_CAT, data = filEIDPROS)
summary(modelamprosMOCA)

### CRI_Total_CAT ###
modelamprosCRI <- lm(formula= value ~ AMP_ROS * CRI_Total_CAT, data = filEIDPROS)
summary(modelamprosCRI)

### IDARE_R_CAT ###
modelamprosIDARE_R <- lm(formula= value ~ AMP_ROS * IDARE_R_CAT, data = filEIDPROS)
summary(modelamprosIDARE_R)

### IDERE_R_CAT ###
modelamprosIDERE_R <- lm(formula= value ~ AMP_ROS * IDERE_R_CAT, data = filEIDPROS)
summary(modelamprosIDERE_R)


#### SUPRESION ANALISIS CON ATENDER ESCENAS ####
### EDAD ###
modelsupescEDAD <- lm(formula= value ~ SUP_ROS * EDAD_CAT, data = filEIDPESC)
summary(modelsupescEDAD)

### ESCOLARIDAD_CAT ###
modelsupescESCOL <- lm(formula= value ~ SUP_ROS * ESCOLARIDAD_CAT, data = filEIDPESC)
summary(modelsupescESCOL)

### MoCA_CAT ###
modelsupescMOCA <- lm(formula= value ~ SUP_ROS * MoCA_CAT, data = filEIDPESC)
summary(modelsupescMOCA)

### CRI_Total_CAT ###
modelsupescCRI <- lm(formula= value ~ SUP_ROS * CRI_Total_CAT, data = filEIDPESC)
summary(modelsupescCRI)

### IDARE_R_CAT ###
modelsupescIDARE_R <- lm(formula= value ~ SUP_ROS * IDARE_R_CAT, data = filEIDPESC)
summary(modelsupescIDARE_R)

### IDERE_R_CAT ###
modelsupescIDERE_R <- lm(formula= value ~ SUP_ROS * IDERE_R_CAT, data = filEIDPESC)
summary(modelsupescIDERE_R)

#### SUPRESION ANALISIS CON ATENDER ROSTROS ####
### EDAD ###
modelsuprosEDAD <- lm(formula= value ~ SUP_ROS * EDAD_CAT, data = filEIDPROS)
summary(modelsuprosEDAD)

### ESCOLARIDAD_CAT ###
modelsuprosESCOL <- lm(formula= value ~ SUP_ROS * ESCOLARIDAD_CAT, data = filEIDPROS)
summary(modelsuprosESCOL)

### MoCA_CAT ###
modelsuprosMOCA <- lm(formula= value ~ SUP_ROS * MoCA_CAT, data = filEIDPROS)
summary(modelsuprosMOCA)

### CRI_Total_CAT ###
modelsuprosCRI <- lm(formula= value ~ SUP_ROS * CRI_Total_CAT, data = filEIDPROS)
summary(modelsuprosCRI)

### IDARE_R_CAT ###
modelsuprosIDARE_R <- lm(formula= value ~ SUP_ROS * IDARE_R_CAT, data = filEIDPROS)
summary(modelsuprosIDARE_R)

### IDERE_R_CAT ###
modelsuprosIDERE_R <- lm(formula= value ~ SUP_ROS * IDERE_R_CAT, data = filEIDPROS)
summary(modelsuprosIDERE_R)




filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()
