
####SOLO %RC Y TR ####
data <- read_xlsx("data.xlsx")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO")) %>%
  separate(tip_cond_val, c("TR_RC", "COND", "VAL"),
           sep = " ")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)

source("summarySE.R")

#### RC

filter(data, TR_RC == "RC", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "RC", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")
                

###### TR

filter(data, TR_RC == "TR", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")
                

filter(data, TR_RC == "TR", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "TR", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA")) %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")


###### SOLO EI #####

data <- read_xlsx("data.xlsx",sheet = "Hoja4")%>%
  gather(tip_cond_val, value, -c("ID","DECADA")) %>%
  separate(tip_cond_val, c("EI", "COND", "VAL"),
           sep = " ")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)

source("summarySE.R")

filter(data, EI == "EI", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")


filter(data, EI == "EI", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")



###### INDICES DE AMPLIFICACION Y SUPRESION #####

data <- read_xlsx("data.xlsx", sheet = "Hoja2")%>%
  gather(tip_cond_val, value, -c("ID","DECADA")) %>%
  separate(tip_cond_val, c("TR_RC", "IND", "VAL"),
           sep = " ")
data$DECADA <- as.factor(data$DECADA)
data$IND <- as.factor(data$IND)


source("summarySE.R")

#### RC

filter(data, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND", "DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INRC", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND")) %>%
  ggplot(aes(y=value, x=IND, fill= IND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "INRC", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

#### TR

filter(data, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND", "DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INTR", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND")) %>%
  ggplot(aes(y=value, x=IND, fill= IND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "INTR", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

#### EI

filter(data, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND", "DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "INEI", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND")) %>%
  ggplot(aes(y=value, x=IND, fill= IND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "INEI", VAL=="NEU") %>%
  summarySE(measurevar = "value", groupvars = c("IND","DECADA")) %>%
  ggplot(aes(y=value, x=IND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

