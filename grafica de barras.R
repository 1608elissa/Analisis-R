
source("summarySE.R")

#### % DE RESPUESTAS CORRECTAS ####

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO" ) %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("COND"))%>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("VAL"))%>%
  ggplot(aes(y=value, x=VAL, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "VAL")) %>%
  ggplot(aes(y=value, x=COND, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("VAL", "TIPO")) %>%
  ggplot(aes(y=value, x=VAL, fill= TIPO)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "TIPO")) %>%
  ggplot(aes(y=value, x=COND, fill= TIPO)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")


#### TIEMPOS DE REACCION ####

filter(data, TR_RC == "TR", !VAL=="TOT", !TIPO=="TO" )%>%
  view()

filter(data, TR_RC == "TR", VAL=="TOT", TIPO=="TO" ) %>%
  summarySE(measurevar = "value", groupvars = c("DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "TR", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("COND"))%>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")

filter(data, TR_RC == "TR", !VAL=="TOT", !TIPO=="TO") %>%
  summarySE(measurevar = "value", groupvars = c("TIPO"))%>%
  ggplot(aes(y=value, x=TIPO, fill= TIPO)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")+
  guides(fill="none")





filter(data, TR_RC == "EI", !COND== "PAS", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND","DECADA"))%>%
  ggplot(aes(y=value, x=DECADA, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")
