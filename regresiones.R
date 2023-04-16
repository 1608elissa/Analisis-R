####### SOLO NEUTRALES #####

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

####### MECANISMO AMPLIFICACION Y SUPRESION #####

#### Respuestas correctas
data1 <- read_xlsx("TR_RC.xlsx", sheet = "Hoja4")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("TR_RC", "IND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$IND <- as.factor(data1$IND)
data1$ESCOLARIDAD <- as.numeric(data1$ESCOLARIDAD)


filter(data1, TR_RC=="INRC", IND=="SUP", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()


filter(data1, TR_RC=="INRC", IND=="SUP", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "RC") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, TR_RC=="INRC", IND=="AMP", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "RC") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, TR_RC=="INRC", IND=="AMP", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

filter(data1, TR_RC=="INRC", IND=="SUP", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

#### Tiempos de reaccion

data1 <- read_xlsx("TR_RC.xlsx", sheet = "Hoja3")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","ESCOLARIDAD")) %>%
  separate(tip_cond_val, c("TR_RC", "IND", "VAL"),
           sep = " ")
data1$DECADA <- as.factor(data1$DECADA)
data1$IND <- as.factor(data1$IND)
data1$ESCOLARIDAD <- as.numeric(data1$ESCOLARIDAD)

filter(data1, TR_RC=="INTR", IND=="SUP", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, TR_RC=="INTR", IND=="AMP", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data1, TR_RC=="INTR", IND=="AMP", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()

grafica <- filter(data1, TR_RC=="INTR", IND=="SUP", VAL=="NEU") %>%
  lm(value ~ EDAD * ESCOLARIDAD, data = .) %>% 
  summary()


filter(data1, TR_RC=="INTR", IND=="SUP", VAL=="NEU") %>%
  ggplot(aes(x = EDAD, y = value, color = ESCOLARIDAD)) +
  geom_point() + 
  labs(x = "EDAD", y = "TR") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()



