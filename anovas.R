library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)


###### ANOVAS CON VALENCIA EMOCIONAL #####
data <- read_xlsx("Conducta resultados.xlsx",sheet = "TR_RC")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

##### Respuestas correctas ####
filter(data, TR_RC == "RC", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*TIPO*DECADA + Error(ID/(COND*VAL*DECADA)), data=.)%>%
  summary()

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  aov(value~COND*VAL*TIPO*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  aov(value ~ COND*DECADA + Error(ID/(COND*DECADA)), data=.)%>%
  summary()

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  aov(value~COND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

##### Tiempo de reaccion 

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA + Error(ID/(COND*VAL*DECADA)), data=.) %>%
  summary()

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  tukey_hsd() %>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  aov(value ~ COND*DECADA + Error(ID/(COND*DECADA)), data=.) %>%
  summary()

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  tukey_hsd() %>%
  filter(p.adj < 0.05)%>%
  View()

####### SOLO NEUTRALES #####
data1 <- read_xlsx("data.xlsx")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO","EDAD")) %>%
  separate(tip_cond_val, c("TR_RC", "COND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)

##### Respuestas correctas 

filter(data1, TR_RC == "RC", VAL=="NEU") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  summary()

filter(data1, TR_RC == "RC", VAL=="NEU") %>%
  aov(value~COND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### Tiempo de reaccion 

filter(data1, TR_RC == "TR", VAL=="NEU") %>%
  aov(value ~ COND*DECADA, data=.) %>%
  summary()

filter(data1, TR_RC == "TR", VAL=="NEU") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  tukey_hsd() %>%
  filter(p.adj < 0.05)%>%
  View()


####### MECANISMO AMPLIFICACION Y SUPRESION #####
data2 <- read_xlsx("data.xlsx", sheet = "Hoja2")%>%
  gather(tip_cond_val, value, -c("ID","DECADA")) %>%
  separate(tip_cond_val, c("TR_RC", "IND", "VAL"),
           sep = " ")
data2$DECADA <- as.factor(data2$DECADA)
data2$IND <- as.factor(data2$IND)


#### Respuestas correctas
filter(data2, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INRC", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INRC", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### Tiempos de reaccion
filter(data2, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INTR", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INTR", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

#### Eficiencia inversa
filter(data2, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INEI", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  aov(value ~ DECADA, data=.)%>%
  summary()

filter(data2, TR_RC == "INEI", VAL=="NEU") %>%
  aov(value~IND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

####### INDICE DE EFICIENCIA INVERSA #####
data <- read_xlsx("data.xlsx",sheet = "Hoja4")%>%
  gather(tip_cond_val, value, -c("ID","DECADA")) %>%
  separate(tip_cond_val, c("EI", "COND", "VAL"),
           sep = " ")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)


filter(data, EI == "EI", VAL=="NEU") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  summary()

filter(data,  EI == "EI", VAL=="NEU") %>%
  aov(value~COND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()
