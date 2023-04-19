library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)


###### ANOVAS CON VALENCIA EMOCIONAL #####
data <- read_xlsx("Conducta resultados.xlsx",sheet = "TR_RC")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND","TR_RC","TIPO", "VAL"),
           sep = "_")

data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)

##### Respuestas correctas ###

filter(data, TR_RC == "RC", TIPO=="TG", !VAL=="TOT") %>%
  lm(formula= value ~ COND+VAL+EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS",TR_RC == "RC", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", TIPO=="TG", VAL=="NEG") %>%
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

filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="TG", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, COND=="PAS",TR_RC == "RC", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="PAS",TR_RC == "RC", TIPO=="NT", VAL=="NEG") %>%
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

filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="NEU") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="POS") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()
filter(data, COND=="ROS",TR_RC == "RC", TIPO=="NT", VAL=="NEG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

##### Tiempo de reaccion ###

filter(data, TR_RC == "TR", TIPO=="TG", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  summary()

filter(data, TR_RC == "TR", TIPO=="TG", !VAL=="TOT") %>%
  aov(value~COND*VAL*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, TR_RC == "TR", TIPO=="NT", !VAL=="TOT") %>%
  aov(value ~ COND*VAL*DECADA, data=.)%>%
  summary()

filter(data, TR_RC == "TR", TIPO=="NT", !VAL=="TOT") %>%
  aov(value~COND*VAL*DECADA, data=.)%>%
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


##### Respuestas correctas 
data1 <- read_xlsx("TR_RC.xlsx")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("TR_RC", "COND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)
data1$ESCOLARIDAD <- as.numeric(data1$ESCOLARIDAD)

filter(data1, COND=="ROSTRO", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "%RC") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ESCENA", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "%RC") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="PASIV", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "%RC") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  coord_cartesian(ylim = c(75,100)) +
  theme_classic()


filter(data1, COND=="ROSTRO", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

filter(data1, COND=="ESCENA", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

filter(data1, COND=="PASIV", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()


##### Tiempo de reaccion 

data1 <- read_xlsx("TR_RC.xlsx", sheet = "Hoja2")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("TR_RC", "COND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)
data1$ESCOLARIDAD <- as.numeric(data1$ESCOLARIDAD)

filter(data1, COND=="ROSTRO", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ESCENA", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="PASIV", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()


filter(data1, COND=="ROSTRO", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

filter(data1, COND=="ESCENA", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

filter(data1, COND=="PASIV", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()
