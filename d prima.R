library(rstatix)
library(tidyverse)
library(readxl)

####### D PRIMA SOLO NEUTRALES #####
data1 <- read_xlsx("d prima.xlsx", sheet = "dprima")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("COND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)


filter(data1, VAL=="NEU") %>%
  aov(value ~ COND*DECADA, data=.)%>%
  summary()

filter(data1, VAL=="NEU") %>%
  aov(value~COND*DECADA, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

######## GRAFICAS D PRIMA SOLO NEUTRALES #####
source("summarySE.R")

filter(data1, VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data1, VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA")) %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")


####### REGRESION CON D PRIMA SOLO NEUTRALES #####
data1 <- read_xlsx("d prima.xlsx", sheet = "dprima")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("COND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)

filter(data1, COND=="ROSTRO", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "EDAD", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ESCENA", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="PASIV", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

####### REGRESION CON D PRIMA PARA MECANISMO DE SUPRESION #####
data1 <- read_xlsx("d prima.xlsx", sheet = "regresion")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","ESCOLARIDAD","SUPRESION","AMPLIFICACION")) %>%
  separate(tip_cond_val, c("COND"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$COND <- as.factor(data1$COND)


filter(data1, COND=="ROSTRO") %>%
  cor.test(value, SUPRESION)

filter(data1, COND=="ESCENA") %>%
  cor.test(value, SUPRESION)

filter(data1, COND=="ROSTRO") %>%
  cor.test(value, AMPLIFICACION)

filter(data1, COND=="ESCENA") %$%
  cor.test(value, AMPLIFICACION)


filter(data1, COND=="ROSTRO") %>%
  ggplot(aes(x = SUPRESION, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "SUPRESION", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ESCENA") %>%
  lm(value ~ SUPRESION, data = .) %>% 
  summary()

filter(data1, COND=="ESCENA") %>%
  ggplot(aes(x = SUPRESION, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "SUPRESION", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="PASIV") %>%
  lm(value ~ SUPRESION, data = .) %>% 
  summary()

filter(data1, COND=="PASIV") %>%
  ggplot(aes(x = SUPRESION, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "SUPRESION", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ROSTRO") %>%
  ggplot(aes(x = AMPLIFICACION, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "AMPLIFICACION", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, COND=="ESCENA") %>%
  ggplot(aes(x = AMPLIFICACION, y = value)) +
  geom_point() + 
  theme_classic() +
  labs(x = "AMPLIFICACION", y = "d'") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()
