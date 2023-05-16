library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

###### REGRESIONES CON VALENCIA EMOCIONAL #####
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

filter(data, TR_RC == "RC", TIPO=="TG", !VAL=="TOT") %>%
  lm(formula= value ~ COND+VAL+EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC",TR_RC == "RC", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS",TR_RC == "RC", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC",TR_RC == "RC", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "RC", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "RC", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "RC", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "RC", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "RC", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

##### Tiempo de reaccion ###

filter(data, TR_RC == "TR", TIPO=="TG", !VAL=="TOT") %>%
  lm(formula= value ~ COND+VAL+EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS",TR_RC == "TR", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "TR", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "TR", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC",TR_RC == "TR", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS",TR_RC == "TR", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "TR", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "TR", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC",TR_RC == "TR", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS",TR_RC == "TR", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "TR", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "TR", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "TR", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "TR", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "TR", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "TR", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "TR", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

#### INDICE DE EFICIENCIA INVERSA ####

filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TO", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS", TR_RC == "EI", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROS", TR_RC == "EI", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC", TR_RC == "EI", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "EI", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC", TR_RC == "EI", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESC", TR_RC == "EI", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "EI", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC", TR_RC == "EI", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "EI", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "EI", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="TG", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS", TR_RC == "EI", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESC",TR_RC == "EI", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS", TR_RC == "EI", TIPO=="NT", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

#### D PRIMA ####

data <- read_xlsx("Conducta resultados.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")

data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)

filter(data, COND=="ROSTROS", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROSTROS", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROSTROS", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ESCENAS", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESCENAS", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESCENAS", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="ROSTROS", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ESCENAS", VAL=="TOT") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

##### Respuestas correctas 
data1 <- read_xlsx("Conducta resultados.xlsx",sheet = "TR_RC_EI")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"),
           sep = "_")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)


filter(data, COND=="ROS", TR_RC == "EI", TIPO=="TG", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()


data1 <- read_xlsx("Conducta resultados.xlsx",sheet = "D_PRIMA")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("DPRIMA","COND","VAL"),
           sep = "_")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)


filter(data1, COND=="ESCENAS", VAL=="TOT") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()


data <- read_xlsx("analisis ale.xlsx")
df <- data.frame(x = "TimeOpenArms", y = "Breakpoint", grupo = "Group")
modelo <- lm(data$TimeOpenArms ~ data$Breakpoint)

ggplot(data1, aes(x = EDAD, y = value, color = VAL)) + 
  geom_point(size = 5) + geom_smooth(method = "lm", se= F) 
