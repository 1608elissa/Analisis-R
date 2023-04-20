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
data$TR_RC <- as.factor(data$TR_RC)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

##### Respuestas correctas ###
filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  aov(value ~ COND*VAL*DECADA*TIPO, data=.)%>%
  summary()

filter(data, TR_RC == "RC",  !VAL=="TOT", !TIPO=="TO") %>%
  aov(value~COND*VAL*DECADA*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### Tiempo de reaccion ###

filter(data, TR_RC == "TR", !TIPO=="TO", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA*TIPO, data=.)%>%
  summary()

filter(data, TR_RC == "TR", !TIPO=="TO", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### INDICE DE EFICIENCIA INVERSA ####

data <- read_xlsx("Conducta resultados.xlsx",sheet = "EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","EI","VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)


filter(data) %>%
  aov(value ~ COND*DECADA*VAL, data=.)%>%
  summary()

filter(data) %>%
  aov(value~COND*DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


#### D PRIMA ####

data <- read_xlsx("Conducta resultados.xlsx",sheet = "DPRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)


filter(data) %>%
  aov(value ~ COND*DECADA*VAL, data=.)%>%
  summary()

filter(data) %>%
  aov(value~COND*DECADA*VAL, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()