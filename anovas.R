library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

###### ANOVAS CON VALENCIA EMOCIONAL #####
data <- read_xlsx("Conducta resultados.xlsx",sheet = "TR_RC_EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$TR_RC <- as.factor(data$TR_RC)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

##### Respuestas correctas ###
filter(data, TR_RC == "RC", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  summary()

filter(data, TR_RC == "RC", !COND=="PAS", !VAL=="TOT", TIPO=="TG") %>%
  aov(value~COND*VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### Tiempo de reaccion ###

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

data1 <- read_xlsx("Conducta resultados.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)
data1$VAL <- as.factor(data1$VAL)


filter(data1, !VAL=="TOT") %>%
  aov(value ~ COND*DECADA*VAL, data=.)%>%
  summary()

filter(data1, !VAL=="TOT") %>%
  aov(value~COND*DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()



#### MOVIMIENTOS OCULARES ####

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


filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="NUM", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~ VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ VAL*DECADA, data=.)%>%
  summary()

filter(data2, MECANISM== "AMP", MEDICION=="DUR", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
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
