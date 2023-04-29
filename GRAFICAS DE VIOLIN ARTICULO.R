library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

#### BASES DE DATOS ####
# PARA RC, TR, EI #
data <- read_xlsx("Junto.xlsx",sheet = "TR_RC_EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                 "CRI_Total_Z", "ENF_NEURO_FAM", "ENF_PSIC", "COVID",
                                 "EDIMBURGO", "IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE",
                                 "IDERE_R_NIVEL", "IDERE_E_PUNTAJE", "IDERE_E_NIVEL", 
                                 "SHIPLEY")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"), sep = "_")

data$DECADA <- as.factor(data$DECADA)
data$SEXO <- as.factor(data$SEXO)
data$ENF_NEURO_FAM <- as.factor(data$ENF_NEURO_FAM)
data$ENF_PSIC <- as.factor(data$ENF_PSIC)
data$COVID <- as.factor(data$COVID)
data$IDARE_R_NIVEL <- as.factor(data$IDARE_R_NIVEL)
data$IDARE_E_NIVEL <- as.factor(data$IDARE_E_NIVEL)
data$IDERE_R_NIVEL <- as.factor(data$IDERE_R_NIVEL)
data$IDERE_E_NIVEL <- as.factor(data$IDERE_E_NIVEL)
data$TR_RC <- as.factor(data$TR_RC)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

# PARA D PRIMA"
data1 <- read_xlsx("Junto.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                 "CRI_Total_Z", "ENF_NEURO_FAM", "ENF_PSIC", "COVID",
                                 "EDIMBURGO", "IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE",
                                 "IDERE_R_NIVEL", "IDERE_E_PUNTAJE", "IDERE_E_NIVEL", 
                                 "SHIPLEY")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")

data1$DECADA <- as.factor(data1$DECADA)
data1$SEXO <- as.factor(data1$SEXO)
data1$ENF_NEURO_FAM <- as.factor(data1$ENF_NEURO_FAM)
data1$ENF_PSIC <- as.factor(data1$ENF_PSIC)
data1$COVID <- as.factor(data1$COVID)
data1$IDARE_R_NIVEL <- as.factor(data1$IDARE_R_NIVEL)
data1$IDARE_E_NIVEL <- as.factor(data1$IDARE_E_NIVEL)
data1$IDERE_R_NIVEL <- as.factor(data1$IDERE_R_NIVEL)
data1$IDERE_E_NIVEL <- as.factor(data1$IDERE_E_NIVEL)
data1$COND <- as.factor(data1$COND)
data1$VAL <- as.factor(data1$VAL)

# PARA COSTO DE TR ESCENAS - ROSTROS #
data5 <- read_xlsx("Junto.xlsx",sheet = "Costo_TR")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                 "CRI_Total_Z", "ENF_NEURO_FAM", "ENF_PSIC", "COVID",
                                 "EDIMBURGO", "IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE",
                                 "IDERE_R_NIVEL", "IDERE_E_PUNTAJE", "IDERE_E_NIVEL", 
                                 "SHIPLEY")) %>%
  separate(tip_cond_val, c("COSTO","TR","TIPO", "VAL"),
           sep = "_")

data5$DECADA <- as.factor(data5$DECADA)
data5$SEXO <- as.factor(data5$SEXO)
data5$ENF_NEURO_FAM <- as.factor(data5$ENF_NEURO_FAM)
data5$ENF_PSIC <- as.factor(data5$ENF_PSIC)
data5$COVID <- as.factor(data5$COVID)
data5$IDARE_R_NIVEL <- as.factor(data5$IDARE_R_NIVEL)
data5$IDARE_E_NIVEL <- as.factor(data5$IDARE_E_NIVEL)
data5$IDERE_R_NIVEL <- as.factor(data5$IDERE_R_NIVEL)
data5$IDERE_E_NIVEL <- as.factor(data5$IDERE_E_NIVEL)
data5$COSTO <- as.factor(data5$COSTO)
data5$TR <- as.factor(data5$TR)
data5$TIPO <- as.factor(data5$TIPO)
data5$VAL <- as.factor(data5$VAL)

# PARA MOVIMIENTOS OCULARES #
data2 <- read_xlsx("Junto.xlsx",sheet = "INDICES")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                 "CRI_Total_Z", "ENF_NEURO_FAM", "ENF_PSIC", "COVID",
                                 "EDIMBURGO", "IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE",
                                 "IDERE_R_NIVEL", "IDERE_E_PUNTAJE", "IDERE_E_NIVEL", 
                                 "SHIPLEY")) %>%
  separate(tip_cond_val, c("MECANISM","MEDICION","CALIF","FASE","ESTIMULO","TIPO","VAL"),
           sep = "_")
data2$DECADA <- as.factor(data2$DECADA)
data2$MECANISM <- as.factor(data2$MECANISM)
data2$MEDICION <- as.factor(data2$MEDICION)
data2$CALIF <- as.factor(data2$CALIF)
data2$FASE <- as.factor(data2$FASE)
data2$ESTIMULO <- as.factor(data2$ESTIMULO)
data2$TIPO <- as.factor(data2$TIPO)
data2$VAL <- as.factor(data2$VAL)

# RESTA DE ROSTROS Y ESCENAS #

data4 <- read_xlsx("Junto.xlsx",sheet = "COSTO_MECANISMOS")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                 "CRI_Total_Z", "ENF_NEURO_FAM", "ENF_PSIC", "COVID",
                                 "EDIMBURGO", "IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE",
                                 "IDERE_R_NIVEL", "IDERE_E_PUNTAJE", "IDERE_E_NIVEL", 
                                 "SHIPLEY")) %>%
  separate(tip_cond_val, c("MECANISM","MEDICION","COND","VAL"),
           sep = "_")
data4$DECADA <- as.factor(data4$DECADA)
data4$MECANISM <- as.factor(data4$MECANISM)
data4$MEDICION <- as.factor(data4$MEDICION)
data4$COND <- as.factor(data4$COND)
data4$VAL <- as.factor(data4$VAL)


#### GRAFICAS DE VIOLIN ####



ggplot(data3, aes(x = DECADA, y = MoCA, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")