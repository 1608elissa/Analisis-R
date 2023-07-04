library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

###### ANOVAS CON VALENCIA EMOCIONAL #####
data <- read_xlsx("Junto.xlsx",sheet = "TR_RC_EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                 "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                 "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                 "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                 "OCUPACION","OCUPA_CAT","OCUPACION_CAT")) %>%
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

##### Respuestas correctas ####
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  summary()

filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value~COND*VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### Tiempo de reaccion ####

filter(data, TR_RC == "TR", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  summary()

filter(data, TR_RC == "TR", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### INDICE DE EFICIENCIA INVERSA ####

filter(data, TR_RC == "EI", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*DECADA*VAL, data=.)%>%
  summary()

filter(data, TR_RC == "EI", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value~COND*DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### D PRIMA ####

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

data1$DECADA <- as.factor(data1$DECADA)
data1$SEXO <- as.factor(data1$SEXO)
data1$ESCOL_CAT <- as.factor(data1$ESCOL_CAT)
data1$ENF_NEURO_FAM <- as.factor(data1$ENF_NEURO_FAM)
data1$ENF_PSIC <- as.factor(data1$ENF_PSIC)
data1$COVID_CAT <- as.factor(data1$COVID_CAT)
data1$IDARE_R_NIVEL <- as.factor(data1$IDARE_R_NIVEL)
data1$IDARE_E_NIVEL <- as.factor(data1$IDARE_E_NIVEL)
data1$IDERE_R_NIVEL <- as.factor(data1$IDERE_R_NIVEL)
data1$IDERE_E_NIVEL <- as.factor(data1$IDERE_E_NIVEL)
data1$COND <- as.factor(data1$COND)
data1$VAL <- as.factor(data1$VAL)


filter(data1, DPRIMA== "DPR", !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL, data=.)%>%
  summary()

filter(data1, DPRIMA== "DPR", !VAL=="TOT") %>%
  aov(value~COND*DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, DPRIMA== "EIDP", !VAL=="TOT") %>%
  aov(value ~ DECADA*COND*VAL*ESCOL_CAT, data=.)%>%
  summary()

filter(data1, DPRIMA== "EIDP", !VAL=="TOT") %>%
  aov(value~ DECADA*COND*VAL*ESCOL_CAT, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data1, DPRIMA== "EIDP", !VAL=="TOT") %>%
  shapiro.test(value)
#### COSTO EN LA MEMORIA DE TRABAJO ESCENAS - ROSTROS ####

data5 <- read_xlsx("Junto.xlsx",sheet = "Costo_TR")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                 "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                 "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                 "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                 "OCUPACION","OCUPA_CAT","OCUPACION_CAT")) %>%
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


filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL, data=.)%>%
  summary()

filter(data5, !VAL=="TOT") %>%
  aov(value~DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

#### MOVIMIENTOS OCULARES ####

data2 <- read_xlsx("Junto.xlsx",sheet = "INDICES")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                 "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                 "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                 "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                 "OCUPACION","OCUPA_CAT","OCUPACION_CAT")) %>%
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


a <- filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT")
b <- filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT")


filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


# RESTAR DE ROSTROS Y ESCENAS #

data4 <- read_xlsx("Junto.xlsx",sheet = "COSTO_MECANISMOS")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                 "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                 "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                 "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                 "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                 "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                 "OCUPACION","OCUPA_CAT","OCUPACION_CAT")) %>%
  separate(tip_cond_val, c("MECANISM","MEDICION","COND","VAL"),
           sep = "_")
data4$DECADA <- as.factor(data4$DECADA)
data4$MECANISM <- as.factor(data4$MECANISM)
data4$MEDICION <- as.factor(data4$MEDICION)
data4$COND <- as.factor(data4$COND)
data4$VAL <- as.factor(data4$VAL)


filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA*COND, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA*COND, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA*COND, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### ANCOVAS ####

# PORCENTAJE DE RESPUESTAS CORRECTAS #
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*CRI_TotaL, data=.)%>%
  summary()
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data, TR_RC == "RC", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# TIEMPOS DE REACCIÓN #
filter(data, TR_RC == "TR", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data, TR_RC == "TR", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data, TR_RC == "TR", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data, TR_RC == "TR", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data, TR_RC == "TR", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# INDICE DE EFICIENCIA INVERSA #
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# INDICE DE EFICIENCIA INVERSA #
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# D PRIMA #
filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL*ESCOLARIDAD, data=.)%>%
  summary()
filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL*MoCA, data=.)%>%
  summary()
filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL*CRI_Total, data=.)%>%
  summary()
filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# INDICES DE AMPLIFICACION Y SUPRESION #
filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*ESCOLARIDAD, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA*ESCOLARIDAD*CRI_Total*IDARE_R_PUNTAJE*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA*ESCOLARIDAD*CRI_Total*IDARE_R_PUNTAJE*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*MoCA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*CRI_Total, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDARE_R_PUNTAJE, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*IDERE_R_PUNTAJE, data=.)%>%
  summary()



 
# RESTA ROSTROS - ESCENAS #
filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*ESCOLARIDAD, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*ESCOLARIDAD, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*ESCOLARIDAD, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*MoCA, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*MoCA, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*MoCA, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*CRI_Total, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*CRI_Total, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*CRI_Total, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDARE_R_PUNTAJE, data=.)%>%
  summary()

filter(data4, MECANISM== "RAMP", MEDICION=="NUM", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="DUR", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDERE_R_PUNTAJE, data=.)%>%
  summary()
filter(data4, MECANISM== "RAMP", MEDICION=="1DU", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA*COND*IDERE_R_PUNTAJE, data=.)%>%
  summary()

# COSTO TR ESCENAS - ROSTROS #
filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL*ESCOLARIDAD, data=.)%>%
  summary()
filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL*MoCA, data=.)%>%
  summary()
filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL*CRI_Total, data=.)%>%
  summary()
filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL*IDARE_R_PUNTAJE, data=.)%>%
  summary()
filter(data5, !VAL=="TOT") %>%
  aov(value ~ DECADA*VAL*IDERE_R_PUNTAJE, data=.)%>%
  summary()
