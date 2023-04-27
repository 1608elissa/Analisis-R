# GRAFICAS #

# PRIMER PASO: INSTALAR LAS LIBRERIAS Y CARGARLAS #
install.packages("plotrix")
install.packages("readxl")
install.packages("rstatix")
install.packages("tidyverse")

library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)
source("summarySE.R")

# SEGUNDO PASO: CARGAR LOS ARCHIVOS DE EXCEL #
# PARA RC, TR, EI #
data <- read_xlsx("Conducta resultados.xlsx",sheet = "TR_RC_EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$TR_RC <- as.factor(data$TR_RC)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

# PARA D PRIMA"
data1 <- read_xlsx("Conducta resultados.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)
data1$VAL <- as.factor(data1$VAL)

# PARA MOVIMIENTOS OCULARES #
data2 <- read_xlsx("AOI resultados.xlsx",sheet = "AMP_SUP")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
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

# GRAFICAS PARA PORCENTAJE DE RESPUESTAS CORRECTAS #

filter(data, TR_RC == "RC", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "RC", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("VAL"))%>%
  ggplot(aes(y=value, x=VAL, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "RC", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("VAL","COND"))%>%
  ggplot(aes(y=value, x=VAL, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

# GRAFICAS PARA TIEMPOS DE REACCION #

filter(data, TR_RC == "TR", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "TR", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("COND"))%>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "TR", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA","COND"))%>%
  ggplot(aes(y=value, x=DECADA, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

# GRAFICAS PARA INDICE DE EFICIENCIA INVERSA #

filter(data, TR_RC == "EI", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "EI", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("COND"))%>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "EI", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  summarySE(measurevar = "value", groupvars = c("VAL"))%>%
  ggplot(aes(y=value, x=VAL, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

# GRAFICAS PARA D PRIMA #

filter(data1, !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data1, !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("VAL"))%>%
  ggplot(aes(y=value, x=VAL, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data1, !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND","VAL"))%>%
  ggplot(aes(y=value, x=VAL, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

# GRAFICAS PARA MOVIMIENTOS OCULARES #

filter(data2, MECANISM== "SUP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data2, MECANISM== "SUP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ROS", TIPO=="TG", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")
